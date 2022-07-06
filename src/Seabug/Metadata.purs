module Seabug.Metadata
  ( BlockfrostFetchError(..)
  , FullSeabugMetadata
  , Hash
  , getFullSeabugMetadata
  , getFullSeabugMetadataWithBackoff
  ) where

import Contract.Prelude

import Aeson as Aeson
import Affjax (printError)
import Affjax as Affjax
import Affjax.RequestHeader as Affjax.RequestHeader
import Affjax.ResponseFormat as Affjax.ResponseFormat
import Cardano.Types.Value as Cardano.Types.Value
import Contract.Prim.ByteArray (byteArrayToHex)
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , getCurrencySymbol
  , getTokenName
  , mkCurrencySymbol
  )
import Control.Alternative (guard)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (ExceptT(ExceptT), except, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Trans (asks)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut as Argonaut
import Data.Bifunctor (lmap)
import Data.Function (on)
import Data.HTTP.Method (Method(GET))
import Data.Newtype (unwrap)
import Effect.Aff (delay)
import Effect.Random (randomRange)
import Seabug.Metadata.Types (SeabugMetadata(SeabugMetadata))
import Partial.Unsafe (unsafePartial)
import Types.CborBytes (cborBytesToByteArray)

type Hash = String

type FullSeabugMetadata =
  { seabugMetadata :: SeabugMetadata
  , ipfsHash :: Hash
  }

data BlockfrostFetchError
  = BlockfrostRateLimit
  | BlockfrostOtherError String

derive instance Generic BlockfrostFetchError _

instance Show BlockfrostFetchError where
  show = genericShow

type BlockfrostFetch a = ExceptT BlockfrostFetchError
  (ReaderT { projectId :: String } Aff)
  a

-- | Tries to get the metadata for the given asset using
-- | Blockfrost. If the rate limit is hit, retries after a random
-- | delay, up to 5 times. Uses a very simple back-off mechanism.
-- | Instead of relying on this, refactor so the rate limit isn't hit.
getFullSeabugMetadataWithBackoff
  :: CurrencySymbol /\ TokenName
  -> String
  -> Aff (Either BlockfrostFetchError FullSeabugMetadata)
getFullSeabugMetadataWithBackoff asset projectId = go 1.0
  where
  go n = do
    r <- getFullSeabugMetadata asset projectId
    case r of
      Left BlockfrostRateLimit
        | n < 5.0 -> do
            let n' = n + 1.0
            log "Blockfrost rate limit hit, backing off"
            -- Wait a random amount of time in the range of [1, 3 *
            -- (attempt + 1)) seconds, this is just a heuristic based
            -- on my testing
            delay <<< wrap <<< (_ * 1000.0) =<<
              (liftEffect $ randomRange 1.0 (3.0 * n'))
            log $ "Retrying, attempt " <> show n'
            go n'
      _ -> pure r

getFullSeabugMetadata
  :: CurrencySymbol /\ TokenName
  -> String
  -> Aff (Either BlockfrostFetchError FullSeabugMetadata)
getFullSeabugMetadata a@(currSym /\ _) projectId =
  flip runReaderT { projectId } <<< runExceptT $ do
    seabugMetadata <-
      getMintingTxSeabugMetadata currSym =<< getMintingTxHash a
    ipfsHash <- getIpfsHash seabugMetadata
    pure { seabugMetadata, ipfsHash }

getIpfsHash
  :: SeabugMetadata
  -> BlockfrostFetch Hash
getIpfsHash (SeabugMetadata { collectionNftCS, collectionNftTN }) = do
  except <<< (decodeField "image" <=< decodeField "onchain_metadata")
    =<< mkGetRequest ("assets/" <> mkAsset curr collectionNftTN)
  where
  curr :: CurrencySymbol
  curr = unsafePartial $ fromJust $ mkCurrencySymbol $
    Cardano.Types.Value.getCurrencySymbol collectionNftCS

getMintingTxSeabugMetadata
  :: forall (r :: Row Type)
   . CurrencySymbol
  -> Hash
  -> BlockfrostFetch SeabugMetadata
getMintingTxSeabugMetadata currSym txHash = do
  res <- mkGetRequest $ "txs/" <> txHash <> "/metadata"
  ms <- except
    $ lmap (BlockfrostOtherError <<< show)
    $ Aeson.caseAesonArray
        (Left (Argonaut.TypeMismatch "Expected array of objects"))
        Right
        res
  except
    $ note (BlockfrostOtherError ("Unexpected JSON: " <> show res))
    $ findSeabugMetadata ms
  where
  findSeabugMetadata :: Array Aeson.Aeson -> Maybe SeabugMetadata
  findSeabugMetadata = findMap $ Aeson.caseAesonObject Nothing $ \o -> do
    label <- hush $ Aeson.getField o "label"
    guard $ label == "727"
    hush $ do
      md <- Aeson.getField o "json_metadata"
      Aeson.decodeAeson =<< Aeson.getField md currSymKey

  currSymKey :: String
  currSymKey = byteArrayToHex $ getCurrencySymbol currSym

getMintingTxHash
  :: forall (r :: Row Type)
   . CurrencySymbol /\ TokenName
  -> BlockfrostFetch Hash
getMintingTxHash a =
  except <<< decodeField "initial_mint_tx_hash"
    =<< mkGetRequest ("assets/" <> uncurry mkAsset a)

mkAsset :: CurrencySymbol -> TokenName -> String
mkAsset currSym tname =
  ((<>) `on` byteArrayToHex) (getCurrencySymbol currSym)
    (cborBytesToByteArray $ getTokenName tname)

decodeField
  :: forall (a :: Type)
   . Aeson.DecodeAeson a
  => String
  -> Aeson.Aeson
  -> Either BlockfrostFetchError a
decodeField field = do
  lmap (BlockfrostOtherError <<< show) <<<
    ( Aeson.decodeAeson
        <=< Aeson.caseAesonObject
          (Left (Argonaut.TypeMismatch "Expected Object"))
          (flip Aeson.getField field)
    )

mkGetRequest
  :: forall (r :: Row Type)
   . String
  -> BlockfrostFetch Aeson.Aeson
mkGetRequest path = do
  projectId <- lift $ asks $ _.projectId
  let
    req :: Affjax.Request String
    req = Affjax.defaultRequest
      { url = mkUrl
      , responseFormat = Affjax.ResponseFormat.string
      , method = Left GET
      , headers =
          [ Affjax.RequestHeader.RequestHeader "project_id" projectId
          ]
      }
  res <- ExceptT $ liftAff $ do
    r <- Affjax.request req <#> lmap (BlockfrostOtherError <<< printError)
    case r of
      Left e -> log $ show e
      Right _ -> pure unit
    pure r
  when (unwrap res.status == 429) $ throwError $ BlockfrostRateLimit
  except $
    lmap (BlockfrostOtherError <<< (("Error parsing JSON: " <> _) <<< show))
      (Aeson.parseJsonStringToAeson res.body)
  where
  mkUrl :: String
  mkUrl = "https://cardano-testnet.blockfrost.io/api/v0/" <> path
