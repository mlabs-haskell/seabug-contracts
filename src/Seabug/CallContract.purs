module Seabug.CallContract
  ( callMarketPlaceBuy
  , callMarketPlaceListNft
  , callMarketPlaceBuyTest
  ) where

import Contract.Prelude

import Cardano.Types.Value as Cardano.Types.Value
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
import Data.Maybe (fromJust)
import Data.Tuple.Nested ((/\))
import Data.UInt as UInt
import Effect (Effect)
import Effect.Aff (error)
import Effect.Class (liftEffect)
import Data.Log.Level (LogLevel(..))
import Effect.Exception (Error)
import Seabug.Metadata.Types (SeabugMetadata(SeabugMetadata))
import Seabug.Metadata.Share (unShare)
import Partial.Unsafe (unsafePartial)
import Plutus.FromPlutusType (fromPlutusType)
import Seabug.Contract.MarketPlaceBuy (marketplaceBuy)
import Seabug.Contract.MarketPlaceListNft (ListNftResult, marketPlaceListNft)
import Seabug.Types
  ( NftCollection(NftCollection)
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
import Types.Natural as Nat
import Wallet (mkNamiWalletAff)

-- | Exists temporarily for testing purposes
callMarketPlaceBuyTest :: String -> Effect (Promise String)
callMarketPlaceBuyTest = Promise.fromAff <<< pure

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
      addressBech32 $ unsafePartial $ fromJust $ fromPlutusType $ network /\
        output.address
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
    , tokenName: byteArrayToHex $ unwrap $ getTokenName tokenName
    , amount
    }

  convertSeabugMetaData :: SeabugMetadata -> _
  convertSeabugMetaData (SeabugMetadata m) =
    { policyId: scriptHashToBech32Unsafe "policy_vkh" $ unwrap m.policyId -- or the prefix should just be 'script'
    , mintPolicy: byteArrayToHex m.mintPolicy
    , collectionNftCS: byteArrayToHex $ Cardano.Types.Value.getCurrencySymbol $
        m.collectionNftCS
    , collectionNftTN: byteArrayToHex $ unwrap $ getTokenName m.collectionNftTN
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
        <$> (UInt.fromString $ BigInt.toString r.lockLockupEnd)
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
