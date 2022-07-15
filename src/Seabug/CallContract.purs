module Seabug.CallContract
  ( callMarketPlaceBuy
  , callMarketPlaceBuyTest
  , callMarketPlaceListNft
  , callMint
  ) where

import Contract.Prelude

import Contract.Address (Slot(Slot))
import Contract.Monad
  ( ConfigParams(ConfigParams)
  , ContractConfig
  , mkContractConfig
  , runContract
  , runContract_
  )
import Contract.Numeric.Natural (toBigInt)
import Contract.Prim.ByteArray
  ( byteArrayToHex
  , hexToByteArray
  , hexToByteArrayUnsafe
  )
import Contract.Transaction
  ( TransactionInput(TransactionInput)
  , TransactionOutput(TransactionOutput)
  )
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , Value
  , flattenNonAdaAssets
  , getCurrencySymbol
  , getTokenName
  , mkCurrencySymbol
  , mkTokenName
  )
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Log.Level (LogLevel(..))
import Data.Tuple.Nested ((/\))
import Data.UInt as UInt
import Effect (Effect)
import Effect.Aff (delay, error)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Partial.Unsafe (unsafePartial)
import Plutus.Conversion (fromPlutusAddress)
import Seabug.Contract.CnftMint (mintCnft)
import Seabug.Contract.MarketPlaceBuy (marketplaceBuy)
import Seabug.Contract.MarketPlaceListNft (ListNftResult, marketPlaceListNft)
import Seabug.Contract.Mint (mintWithCollection)
import Seabug.Metadata.Share (unShare)
import Seabug.Metadata.Types (SeabugMetadata(SeabugMetadata))
import Seabug.Types
  ( MintCnftParams
  , MintParams
  , NftCollection(NftCollection)
  , NftData(NftData)
  , NftId(NftId)
  )
import Serialization.Address (NetworkId, addressBech32, intToNetworkId)
import Serialization.Hash
  ( ed25519KeyHashToBytes
  , ed25519KeyHashFromBytes
  , scriptHashFromBech32
  , scriptHashToBech32Unsafe
  )
import Types.BigNum as BigNum
import Types.Natural as Nat
import Wallet (mkNamiWalletAff)

-- | Exists temporarily for testing purposes
callMarketPlaceBuyTest :: String -> Effect (Promise String)
callMarketPlaceBuyTest = Promise.fromAff <<< pure

callMint :: ContractConfiguration -> MintArgs -> Effect (Promise Unit)
callMint cfg args = Promise.fromAff do
  contractConfig <- buildContractConfig cfg
  mintCnftParams /\ mintParams <- liftEffect $ liftEither $ buildMintArgs args

  -- Uncomment here to mint the cnft
  -- log "Minting cnft..."
  -- cnft <- runContract contractConfig $ mintCnft mintCnftParams

  -- Uncomment here to mint the sgNft, after updating `curr` for the new cnft
  -- curr <- liftM (error "Bad curr")
  --   ( mkCurrencySymbol
  --       ( hexToByteArrayUnsafe
  --           "47cac61ad42cad00878dcd60793cffeeffc478169ac1ff33988054e5"
  --       )
  --   )
  -- tn <- liftM (error "Bad tn") (mkTokenName (hexToByteArrayUnsafe "abcdef"))
  -- let cnft = curr /\ tn
  -- log "Minting nft..."
  -- runContract contractConfig $ mintWithCollection cnft mintParams

  pure unit

-- TODO: we can use this if we need for `callMint`, but I think
-- `awaitTxConfirmed` is coming soon to CTL
countToZero :: Int -> Aff Unit
countToZero n =
  unless (n <= 0) do
    log $ "Waiting before we try to unlock: " <> show n
    (delay <<< wrap) 1000.0
    countToZero (n - 1)

-- | Calls Seabugs marketplaceBuy and takes care of converting data types.
--   Returns a JS promise holding no data.
callMarketPlaceBuy
  :: ContractConfiguration -> BuyNftArgs -> Effect (Promise Unit)
callMarketPlaceBuy cfg args = Promise.fromAff do
  contractConfig <- buildContractConfig cfg
  nftData <- liftEffect $ liftEither $ buildNftData args
  runContract_ contractConfig (marketplaceBuy nftData)

-- | Calls Seabugs marketPlaceListNft and takes care of converting data types.
--   Returns a JS promise holding nft listings.
callMarketPlaceListNft
  :: ContractConfiguration -> Effect (Promise (Array ListNftResultOut))
callMarketPlaceListNft cfg = Promise.fromAff do
  contractConfig <- buildContractConfig cfg
  listnft <- runContract contractConfig marketPlaceListNft
  pure $ buildNftList (unwrap contractConfig).networkId <$> listnft

-- | Configuation needed to call contracts from JS.
type ContractConfiguration =
  { serverHost :: String
  , serverPort :: Int
  , serverSecureConn :: Boolean
  , ogmiosHost :: String
  , ogmiosPort :: Int
  , ogmiosSecureConn :: Boolean
  , datumCacheHost :: String
  , datumCachePort :: Int
  , datumCacheSecureConn :: Boolean
  , networkId :: Int
  , projectId :: String
  , logLevel :: String -- Trace | Debug | Info | Warn | Error
  }

type BuyNftArgs =
  { nftCollectionArgs ::
      { collectionNftCs :: String -- CurrencySymbol
      , lockLockup :: BigInt -- BigInt
      , lockLockupEnd :: BigInt -- Slot
      , lockingScript :: String -- ValidatorHash
      , author :: String -- PaymentPubKeyHash
      , authorShare :: BigInt -- Natural
      , daoScript :: String -- ValidatorHash
      , daoShare :: BigInt -- Natural
      }
  , nftIdArgs ::
      { collectionNftTn :: String -- TokenName
      , price :: BigInt -- Natural
      , owner :: String -- PaymentPubKeyHash
      }
  }

-- Placeholder for types I'm not sure how should we represent on frontend.
type ValueOut = Array
  { currencySymbol :: String, tokenName :: String, amount :: BigInt }

type ListNftResultOut =
  { input :: { transactionId :: String, inputIndex :: Int }
  , output :: { address :: String, value :: ValueOut, dataHash :: String }
  , metadata ::
      { seabugMetadata ::
          { policyId :: String --MintingPolicyHash
          , mintPolicy :: String --ByteArray
          , collectionNftCS :: String -- CurrencySymbol
          , collectionNftTN :: String -- TokenName
          , lockingScript :: String --ValidatorHash
          , authorPkh :: String -- PubKeyHash
          , authorShare :: BigInt -- Share
          , marketplaceScript :: String -- ValidatorHash
          , marketplaceShare :: BigInt -- share
          , ownerPkh :: String -- PubKeyHash
          , ownerPrice :: BigInt --Natural
          }
      , ipfsHash :: String
      }
  }

type MintArgs =
  { imageUri :: String
  , tokenNameString :: String
  , name :: String
  , description :: String
  , price :: BigInt -- Natural
  }

buildContractConfig
  :: ContractConfiguration -> Aff (ContractConfig (projectId :: String))
buildContractConfig cfg = do
  serverPort <- liftM (error "Invalid server port number")
    $ UInt.fromInt' cfg.serverPort
  ogmiosPort <- liftM (error "Invalid ogmios port number")
    $ UInt.fromInt' cfg.ogmiosPort
  datumCachePort <- liftM (error "Invalid datum cache port number")
    $ UInt.fromInt' cfg.datumCachePort
  networkId <- liftM (error "Invalid network id")
    $ intToNetworkId cfg.networkId
  logLevel <- liftM (error "Invalid log level")
    $ stringToLogLevel cfg.logLevel

  wallet <- Just <$> mkNamiWalletAff
  mkContractConfig $ ConfigParams
    { ogmiosConfig:
        { port: ogmiosPort
        , host: cfg.ogmiosHost
        , secure: cfg.ogmiosSecureConn
        }
    , datumCacheConfig:
        { port: datumCachePort
        , host: cfg.datumCacheHost
        , secure: cfg.datumCacheSecureConn
        }
    , ctlServerConfig:
        { port: serverPort
        , host: cfg.serverHost
        , secure: cfg.serverSecureConn
        }
    , networkId: networkId
    , logLevel: logLevel
    , extraConfig: { projectId: cfg.projectId }
    , wallet
    }

stringToLogLevel :: String -> Maybe LogLevel
stringToLogLevel "Trace" = Just Trace
stringToLogLevel "Debug" = Just Debug
stringToLogLevel "Info" = Just Info
stringToLogLevel "Warn" = Just Warn
stringToLogLevel "Error" = Just Error
stringToLogLevel _ = Nothing

buildNftList :: NetworkId -> ListNftResult -> ListNftResultOut
buildNftList
  network
  { input: TransactionInput input, output: TransactionOutput output, metadata } =
  let
    transactionId = byteArrayToHex $ unwrap input.transactionId
    inputIndex = UInt.toInt input.index
    -- TODO: What do we do if this fails?
    address =
      addressBech32 $ unsafePartial $ fromPlutusAddress network output.address
    dataHash = fromMaybe mempty $ byteArrayToHex <<< unwrap <$> output.dataHash
    ipfsHash = metadata.ipfsHash
    seabugMetadata = convertSeabugMetaData metadata.seabugMetadata
  in
    { input: { transactionId, inputIndex }
    , output: { address, dataHash, value: convertValue output.amount }
    , metadata: { ipfsHash, seabugMetadata }
    }
  where
  convertValue :: Value -> ValueOut
  convertValue val = mkValueRecord <$> flattenNonAdaAssets val

  mkValueRecord
    :: (CurrencySymbol /\ TokenName /\ BigInt)
    -> { currencySymbol :: String, tokenName :: String, amount :: BigInt }
  mkValueRecord (currencySymbol /\ tokenName /\ amount) =
    { currencySymbol: byteArrayToHex $ getCurrencySymbol currencySymbol
    , tokenName: byteArrayToHex $ getTokenName tokenName
    , amount
    }

  convertSeabugMetaData :: SeabugMetadata -> _
  convertSeabugMetaData (SeabugMetadata m) =
    { policyId: scriptHashToBech32Unsafe "policy_vkh" $ unwrap m.policyId -- or the prefix should just be 'script'
    , mintPolicy: byteArrayToHex m.mintPolicy
    , collectionNftCS: byteArrayToHex $ getCurrencySymbol m.collectionNftCS
    , collectionNftTN: byteArrayToHex $ getTokenName m.collectionNftTN
    , lockingScript: scriptHashToBech32Unsafe "script" $ unwrap m.lockingScript
    , authorPkh: byteArrayToHex $ unwrap $ ed25519KeyHashToBytes $ unwrap
        m.authorPkh
    , authorShare: unShare m.authorShare
    , marketplaceScript: scriptHashToBech32Unsafe "script" $ unwrap
        m.marketplaceScript
    , marketplaceShare: unShare m.marketplaceShare
    , ownerPkh: byteArrayToHex $ unwrap $ ed25519KeyHashToBytes $ unwrap
        m.ownerPkh
    , ownerPrice: toBigInt m.ownerPrice
    }

buildNftData :: BuyNftArgs -> Either Error NftData
buildNftData { nftCollectionArgs, nftIdArgs } = do
  nftCollection <- mkCollection nftCollectionArgs
  nftId <- mkId nftIdArgs
  pure $ NftData { nftCollection, nftId }
  where
  mkId r = do
    tn <- note (error $ "Invalid collection token name: " <> r.collectionNftTn)
      $ mkTokenName
      =<< hexToByteArray r.collectionNftTn
    price <- note (error $ "Invalid price: " <> show r.price)
      $ Nat.fromBigInt r.price
    owner <- note (error $ "Invalid owner: " <> r.owner)
      $ wrap
      <<< wrap
      <$> (ed25519KeyHashFromBytes <<< wrap =<< hexToByteArray r.owner)
    pure $ NftId
      { collectionNftTn: tn
      , price
      , owner
      }
  mkCollection r = do
    collectionNftCs <-
      note (error $ "Invalid collection currency symbol: " <> r.collectionNftCs)
        $ mkCurrencySymbol
        =<< hexToByteArray r.collectionNftCs
    lockLockupEnd <-
      note (error $ "Invalid nft lockLockupEnd: " <> show r.lockLockupEnd)
        $ Slot
        <$> (BigNum.fromString $ BigInt.toString r.lockLockupEnd)
    lockingScript <-
      note (error $ "Invalid nft lockingScript: " <> r.lockingScript)
        $ wrap
        <$> scriptHashFromBech32 r.lockingScript
    author <- note (error $ "Invalid author: " <> r.author)
      $ wrap
      <<< wrap
      <$> (ed25519KeyHashFromBytes <<< wrap =<< hexToByteArray r.author)
    authorShare <- note (error $ "Invalid authorShare: " <> show r.authorShare)
      $ Nat.fromBigInt r.authorShare
    daoScript <- note (error $ "Invalid nft daoScript: " <> r.daoScript)
      $ wrap
      <$> scriptHashFromBech32 r.daoScript
    daoShare <- note (error $ "Invalid daoShare: " <> show r.daoShare)
      $ Nat.fromBigInt r.daoShare
    pure $ NftCollection
      { collectionNftCs
      , lockLockup: r.lockLockup
      , lockLockupEnd
      , lockingScript
      , author
      , authorShare
      , daoScript
      , daoShare
      }

buildMintArgs :: MintArgs -> Either Error (MintCnftParams /\ MintParams)
buildMintArgs
  { imageUri
  , tokenNameString
  , name
  , description
  , price
  } = do
  price' <- note (error $ "Invalid price: " <> show price)
    $ Nat.fromBigInt price
  let
    mintCnftParams = wrap { imageUri, tokenNameString, name, description }
    -- TODO: Put these hard coded params in a better place
    mintParams = wrap
      { authorShare: Nat.fromInt' 500
      , daoShare: Nat.fromInt' 500
      , price: price'
      , lockLockup: BigInt.fromInt 5
      , lockLockupEnd: Slot $ BigNum.fromInt 5
      , feeVaultKeys: []
      }
  pure (mintCnftParams /\ mintParams)
