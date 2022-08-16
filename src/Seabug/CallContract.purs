module Seabug.CallContract
  ( callConnectWallet
  , callGetWalletBalance
  , callMarketPlaceBuy
  , callMarketPlaceFetchNft
  , callMarketPlaceListNft
  , callMint
  ) where

import Contract.Prelude hiding (null)

import Contract.Address (Slot(Slot))
import Contract.Config (ConfigParams)
import Contract.Monad (runContract)
import Contract.Numeric.Natural (toBigInt)
import Contract.Prim.ByteArray (byteArrayToHex, hexToByteArray)
import Contract.Transaction
  ( TransactionInput(..)
  , TransactionOutput(TransactionOutput)
  , awaitTxConfirmed
  )
import Contract.Utxos (getWalletBalance)
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , Value
  , currencyMPSHash
  , flattenNonAdaAssets
  , getCurrencySymbol
  , getTokenName
  , mkCurrencySymbol
  , mkTokenName
  )
import Control.Monad.Error.Class (throwError)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Log.Level (LogLevel(..))
import Data.Nullable (Nullable, notNull, null, toNullable)
import Data.Tuple.Nested ((/\))
import Data.UInt as UInt
import Effect (Effect)
import Effect.Aff (error)
import Effect.Exception (Error)
import Partial.Unsafe (unsafePartial)
import Plutus.Conversion (fromPlutusAddress)
import Seabug.Contract.Buy (marketplaceBuy)
import Seabug.Contract.CnftMint (mintCnft)
import Seabug.Contract.Common (NftResult)
import Seabug.Contract.MarketPlaceFetchNft (marketPlaceFetchNft)
import Seabug.Contract.MarketPlaceListNft (marketPlaceListNft)
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
import Wallet (Wallet, mkNamiWalletAff, mkGeroWalletAff)

callConnectWallet
  :: forall (r :: Row Type)
   . String
  -> Effect (Promise Wallet)
callConnectWallet walletOption = case walletOption of
  "Nami" -> Promise.fromAff mkNamiWalletAff
  "Gero" -> Promise.fromAff mkGeroWalletAff
  _ -> throwError <<< error $ "Unsupported wallet: " <> walletOption

callGetWalletBalance
  :: ContractConfiguration -> Effect (Promise (Nullable Value))
callGetWalletBalance cfg = Promise.fromAff do
  contractConfig <- liftEither $ buildContractConfig cfg
  toNullable <$> runContract contractConfig getWalletBalance

callMint :: ContractConfiguration -> MintArgs -> Effect (Promise Unit)
callMint cfg args = Promise.fromAff do
  contractConfig <- liftEither $ buildContractConfig cfg
  mintCnftParams /\ mintParams <- liftEither $ buildMintArgs args
  runContract contractConfig $ do
    log "Minting cnft..."
    txHash /\ cnft <- mintCnft mintCnftParams
    log $ "Waiting for confirmation of cnft transaction: " <> show txHash
    awaitTxConfirmed txHash
    log $ "Cnft transaction confirmed: " <> show txHash
    log $ "Minted cnft: " <> show cnft
    log "Minting sgNft..."
    sgNftTxHash <- mintWithCollection cnft mintParams
    log $ "Waiting for confirmation of nft transaction: " <> show sgNftTxHash
    awaitTxConfirmed sgNftTxHash
    log $ "Nft transaction confirmed: " <> show sgNftTxHash

callMarketPlaceFetchNft
  :: ContractConfiguration
  -> TransactionInputOut
  -> Effect (Promise (Nullable ListNftResultOut))
callMarketPlaceFetchNft cfg args = Promise.fromAff do
  contractConfig <- liftEither $ buildContractConfig cfg
  txInput <- liftEither $ buildTransactionInput args
  runContract contractConfig (marketPlaceFetchNft cfg.projectId txInput) >>=
    case _ of
      Nothing -> pure null
      Just nftResult -> pure $ notNull $
        buildNftList contractConfig.networkId nftResult

-- | Calls Seabugs marketplaceBuy and takes care of converting data types.
-- | Returns a JS promise holding no data.
callMarketPlaceBuy
  :: ContractConfiguration -> BuyNftArgs -> Effect (Promise Unit)
callMarketPlaceBuy cfg args = Promise.fromAff do
  contractConfig <- liftEither $ buildContractConfig cfg
  nftData <- liftEither $ buildNftData args
  runContract contractConfig (marketplaceBuy nftData)

-- | Calls Seabugs marketPlaceListNft and takes care of converting data types.
-- | Returns a JS promise holding nft listings.
callMarketPlaceListNft
  :: ContractConfiguration -> Effect (Promise (Array ListNftResultOut))
callMarketPlaceListNft cfg = Promise.fromAff do
  contractConfig <- liftEither $ buildContractConfig cfg
  listnft <- runContract contractConfig (marketPlaceListNft cfg.projectId)
  pure $ buildNftList contractConfig.networkId <$> listnft

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

type TransactionInputOut = { transactionId :: String, inputIndex :: Int }

type ValueOut = Array
  { currencySymbol :: String, tokenName :: String, amount :: BigInt }

type ListNftResultOut =
  { input :: TransactionInputOut
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
          , marketplaceShare :: BigInt -- Share
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
  :: ContractConfiguration -> Either Error (ConfigParams ())
buildContractConfig cfg = do
  serverPort <- note (error "Invalid server port number")
    $ UInt.fromInt' cfg.serverPort
  ogmiosPort <- note (error "Invalid ogmios port number")
    $ UInt.fromInt' cfg.ogmiosPort
  datumCachePort <- note (error "Invalid datum cache port number")
    $ UInt.fromInt' cfg.datumCachePort
  networkId <- note (error "Invalid network id")
    $ intToNetworkId cfg.networkId
  logLevel <- note (error "Invalid log level")
    $ stringToLogLevel cfg.logLevel

  pure
    { ogmiosConfig:
        { port: ogmiosPort
        , host: cfg.ogmiosHost
        , secure: cfg.ogmiosSecureConn
        , path: Nothing
        }
    , datumCacheConfig:
        { port: datumCachePort
        , host: cfg.datumCacheHost
        , secure: cfg.datumCacheSecureConn
        , path: Nothing
        }
    , ctlServerConfig:
        { port: serverPort
        , host: cfg.serverHost
        , secure: cfg.serverSecureConn
        , path: Nothing
        }
    , networkId: networkId
    , logLevel: logLevel
    , extraConfig: {}
    , walletSpec: Nothing
    , customLogger: Nothing
    }

stringToLogLevel :: String -> Maybe LogLevel
stringToLogLevel "Trace" = Just Trace
stringToLogLevel "Debug" = Just Debug
stringToLogLevel "Info" = Just Info
stringToLogLevel "Warn" = Just Warn
stringToLogLevel "Error" = Just Error
stringToLogLevel _ = Nothing

buildNftList :: NetworkId -> NftResult -> ListNftResultOut
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
    { policyId: scriptHashToBech32Unsafe "policy_vkh" $ unwrap $
        currencyMPSHash m.policyId -- or the prefix should just be 'script'
    , mintPolicy: m.mintPolicy
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
    -- TODO: Put these hard coded params in a better place, see
    -- https://github.com/mlabs-haskell/seabug-contracts/issues/25
    mintParams = wrap
      { authorShare: Nat.fromInt' 1000
      , daoShare: Nat.fromInt' 1000
      , price: price'
      , lockLockup: BigInt.fromInt 5
      , lockLockupEnd: Slot $ BigNum.fromInt 5
      , feeVaultKeys: []
      }
  pure (mintCnftParams /\ mintParams)

buildTransactionInput :: TransactionInputOut -> Either Error TransactionInput
buildTransactionInput input = do
  transactionId <-
    note (error $ "Invalid transaction id: " <> input.transactionId)
      $ wrap
      <$> hexToByteArray input.transactionId
  index <- note (error $ "Invalid input index: " <> show input.inputIndex) $
    UInt.fromInt' input.inputIndex
  pure $ wrap { transactionId, index }
