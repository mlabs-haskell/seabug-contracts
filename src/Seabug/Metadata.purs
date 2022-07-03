module Seabug.Metadata
  ( FullSeabugMetadata
  , Hash
  , getFullSeabugMetadata
  ) where

import Contract.Prelude

import Aeson as Aeson
import Affjax as Affjax
import Affjax.RequestHeader as Affjax.RequestHeader
import Affjax.ResponseFormat as Affjax.ResponseFormat
import Cardano.Types.Value as Cardano.Types.Value
import Contract.Monad (Contract)
import Contract.Prim.ByteArray (byteArrayToHex)
import Contract.Transaction
  ( ClientError(ClientHttpError, ClientDecodeJsonError)
  )
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , getCurrencySymbol
  , getTokenName
  , mkCurrencySymbol
  )
import Control.Alternative (guard)
import Control.Monad.Except.Trans (ExceptT(ExceptT), except, runExceptT)
import Control.Monad.Reader.Trans (asks)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut as Argonaut
import Data.Bifunctor (bimap, lmap)
import Data.Function (on)
import Data.HTTP.Method (Method(GET))
import Data.Newtype (unwrap)
import Seabug.Metadata.Types (SeabugMetadata(SeabugMetadata))
import Partial.Unsafe (unsafePartial)
import Types.CborBytes (cborBytesToByteArray)

import Debug (traceM)

type Hash = String

type FullSeabugMetadata =
  { seabugMetadata :: SeabugMetadata
  , ipfsHash :: Hash
  }

getFullSeabugMetadata
  :: forall (r :: Row Type)
   . CurrencySymbol /\ TokenName
  -> Contract (projectId :: String | r) (Either ClientError FullSeabugMetadata)
getFullSeabugMetadata a@(currSym /\ _) = runExceptT $ do
  seabugMetadata <- getMintingTxSeabugMetadata currSym =<< getMintingTxHash a
  log $ show seabugMetadata
  ipfsHash <- getIpfsHash seabugMetadata
  pure { seabugMetadata, ipfsHash }

getIpfsHash
  :: forall (r :: Row Type)
   . SeabugMetadata
  -> ExceptT ClientError (Contract (projectId :: String | r)) Hash
getIpfsHash (SeabugMetadata { collectionNftCS, collectionNftTN }) = do
  except <<< (decodeField "image" <=< decodeFieldJson "onchain_metadata")
    =<< mkGetRequest ("assets/" <> mkAsset curr collectionNftTN)
  where
  curr :: CurrencySymbol
  curr = unsafePartial $ fromJust $ mkCurrencySymbol $
    Cardano.Types.Value.getCurrencySymbol collectionNftCS

getMintingTxSeabugMetadata
  :: forall (r :: Row Type)
   . CurrencySymbol
  -> Hash
  -> ExceptT ClientError (Contract (projectId :: String | r)) SeabugMetadata
getMintingTxSeabugMetadata currSym txHash = do
  res <- mkGetRequest $ "txs/" <> txHash <> "/metadata"
  ms <- except
    $ lmap ClientDecodeJsonError
    $ Aeson.caseAesonArray
        (Left (Argonaut.TypeMismatch "Expected array of objects"))
        Right
        (Aeson.jsonToAeson res)
  except
    $ note (ClientDecodeJsonError (Argonaut.UnexpectedValue res))
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
  -> ExceptT ClientError (Contract (projectId :: String | r)) Hash
getMintingTxHash a =
  except <<< decodeFieldJson "initial_mint_tx_hash"
    =<< mkGetRequest ("assets/" <> uncurry mkAsset a)

mkAsset :: CurrencySymbol -> TokenName -> String
mkAsset currSym tname =
  ((<>) `on` byteArrayToHex) (getCurrencySymbol currSym)
    (getTokenName tname)

decodeField
  :: forall (a :: Type)
   . Aeson.DecodeAeson a
  => String
  -> Aeson.Aeson
  -> Either ClientError a
decodeField field = do
  traceM $ show field
  lmap ClientDecodeJsonError <<<
    ( Aeson.decodeAeson
        <=< Aeson.caseAesonObject
          (Left (Argonaut.TypeMismatch "Expected Object"))
          (flip Aeson.getField field)
    )

decodeFieldJson
  :: forall (a :: Type)
   . Aeson.DecodeAeson a
  => String
  -> Argonaut.Json
  -> Either ClientError a
decodeFieldJson field = decodeField field <<< Aeson.jsonToAeson

mkGetRequest
  :: forall (r :: Row Type)
   . String
  -> ExceptT ClientError (Contract (projectId :: String | r)) Argonaut.Json
mkGetRequest path = do
  projectId <- lift $ asks $ _.projectId <<< unwrap
  let
    req :: Affjax.Request Argonaut.Json
    req = Affjax.defaultRequest
      { url = mkUrl
      , responseFormat = Affjax.ResponseFormat.json
      , method = Left GET
      , headers =
          [ Affjax.RequestHeader.RequestHeader "project_id" projectId
          ]
      }
  ExceptT $ liftAff $ Affjax.request req <#> bimap ClientHttpError _.body
  where
  mkUrl :: String
  mkUrl = "https://cardano-testnet.blockfrost.io/api/v0/" <> path
