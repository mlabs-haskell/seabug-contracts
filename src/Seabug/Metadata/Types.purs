module Seabug.Metadata.Types
  ( SeabugMetadata(..)
  , SeabugMetadataDelta(..)
  , decodeSeabugMetadataAeson
  , metadataBytesString
  ) where

import Contract.Prelude

import Aeson (Aeson, JsonDecodeError(..), caseAesonObject, getField, (.:))
import Contract.Prim.ByteArray (ByteArray, byteArrayToHex)
import Contract.Value (CurrencySymbol, getCurrencySymbol, mkCurrencySymbol)
import Data.BigInt (fromInt) as BigInt
import Data.Either (Either(Left), note)
import Data.Generic.Rep (class Generic)
import Data.Map (toUnfoldable) as Map
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (class Newtype, wrap)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), stripPrefix)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested ((/\))
import FromData (class FromData, fromData)
import Metadata.FromMetadata (class FromMetadata, fromMetadata)
import Metadata.Helpers (unsafeMkKey, lookupKey, lookupMetadata)
import Metadata.MetadataType (class MetadataType, metadataLabel)
import Metadata.ToMetadata (class ToMetadata, toMetadata)
import Partial.Unsafe (unsafePartial)
import Plutus.Types.AssocMap (Map(Map)) as AssocMap
import Seabug.Metadata.Share (Share, mkShare)
import Serialization.Hash
  ( ScriptHash
  , ed25519KeyHashFromBytes
  , scriptHashFromBytes
  )
import ToData (class ToData, toData)
import Type.Proxy (Proxy(Proxy))
import Types.Natural (Natural)
import Types.PlutusData (PlutusData(Map))
import Types.PubKeyHash (PubKeyHash)
import Types.RawBytes (RawBytes, hexToRawBytes)
import Types.Scripts (ValidatorHash)
import Types.TokenName (TokenName, mkTokenName)
import Types.TransactionMetadata (TransactionMetadatum(MetadataMap))

newtype SeabugMetadata = SeabugMetadata
  { policyId :: CurrencySymbol
  , mintPolicy :: String
  , collectionNftCS :: CurrencySymbol
  , collectionNftTN :: TokenName
  , lockingScript :: ValidatorHash
  , authorPkh :: PubKeyHash
  , authorShare :: Share
  , marketplaceScript :: ValidatorHash
  , marketplaceShare :: Share
  , ownerPkh :: PubKeyHash
  , ownerPrice :: Natural
  }

derive instance Generic SeabugMetadata _
derive instance Newtype SeabugMetadata _
derive instance Eq SeabugMetadata

instance Show SeabugMetadata where
  show = genericShow

instance MetadataType SeabugMetadata where
  metadataLabel _ = wrap (BigInt.fromInt 727)

instance ToMetadata SeabugMetadata where
  toMetadata (SeabugMetadata meta) = toMetadata
    [ meta.policyId /\
        [ "mintPolicy" /\ toMetadata meta.mintPolicy
        , "collectionNftCS" /\ toMetadata meta.collectionNftCS
        , "collectionNftTN" /\ toMetadata meta.collectionNftTN
        , "lockingScript" /\ toMetadata meta.lockingScript
        , "authorPkh" /\ toMetadata meta.authorPkh
        , "authorShare" /\ toMetadata meta.authorShare
        , "marketplaceScript" /\ toMetadata meta.marketplaceScript
        , "marketplaceShare" /\ toMetadata meta.marketplaceShare
        , "ownerPkh" /\ toMetadata meta.ownerPkh
        , "ownerPrice" /\ toMetadata meta.ownerPrice
        ]
    ]

instance FromMetadata SeabugMetadata where
  fromMetadata (MetadataMap mp) = do
    policyId /\ contents <- case Map.toUnfoldable mp of
      [ policyId /\ contents ] ->
        Tuple <$> fromMetadata policyId <*> pure contents
      _ -> Nothing
    mintPolicy <-
      lookupMetadata "mintPolicy" contents >>= fromMetadata
    collectionNftCS <-
      lookupMetadata "collectionNftCS" contents >>= fromMetadata
    collectionNftTN <-
      lookupMetadata "collectionNftTN" contents >>= fromMetadata
    lockingScript <-
      lookupMetadata "lockingScript" contents >>= fromMetadata
    authorPkh <-
      lookupMetadata "authorPkh" contents >>= fromMetadata
    authorShare <-
      lookupMetadata "authorShare" contents >>= fromMetadata
    marketplaceScript <-
      lookupMetadata "marketplaceScript" contents >>= fromMetadata
    marketplaceShare <-
      lookupMetadata "marketplaceShare" contents >>= fromMetadata
    ownerPkh <-
      lookupMetadata "ownerPkh" contents >>= fromMetadata
    ownerPrice <-
      lookupMetadata "ownerPrice" contents >>= fromMetadata
    pure $ SeabugMetadata
      { policyId
      , mintPolicy
      , collectionNftCS
      , collectionNftTN
      , lockingScript
      , authorPkh
      , authorShare
      , marketplaceScript
      , marketplaceShare
      , ownerPkh
      , ownerPrice
      }
  fromMetadata _ = Nothing

instance ToData SeabugMetadata where
  toData (SeabugMetadata meta) = unsafePartial $ toData $ AssocMap.Map
    [ unsafeMkKey "727" /\ AssocMap.Map
        [ meta.policyId /\ AssocMap.Map
            [ unsafeMkKey "mintPolicy" /\ toData meta.mintPolicy
            , unsafeMkKey "collectionNftCS" /\ toData meta.collectionNftCS
            , unsafeMkKey "collectionNftTN" /\ toData meta.collectionNftTN
            , unsafeMkKey "lockingScript" /\ toData meta.lockingScript
            , unsafeMkKey "authorPkh" /\ toData meta.authorPkh
            , unsafeMkKey "authorShare" /\ toData meta.authorShare
            , unsafeMkKey "marketplaceScript" /\ toData meta.marketplaceScript
            , unsafeMkKey "marketplaceShare" /\ toData meta.marketplaceShare
            , unsafeMkKey "ownerPkh" /\ toData meta.ownerPkh
            , unsafeMkKey "ownerPrice" /\ toData meta.ownerPrice
            ]
        ]
    ]

instance FromData SeabugMetadata where
  fromData sm = unsafePartial do
    policyId /\ contents <- lookupKey "727" sm >>= case _ of
      Map [ policyId /\ contents ] ->
        Tuple <$> fromData policyId <*> fromData contents
      _ -> Nothing
    mintPolicy <- lookupKey "mintPolicy" contents >>= fromData
    collectionNftCS <- lookupKey "collectionNftCS" contents >>= fromData
    collectionNftTN <- lookupKey "collectionNftTN" contents >>= fromData
    lockingScript <- lookupKey "lockingScript" contents >>= fromData
    authorPkh <- lookupKey "authorPkh" contents >>= fromData
    authorShare <- lookupKey "authorShare" contents >>= fromData
    marketplaceScript <- lookupKey "marketplaceScript" contents >>= fromData
    marketplaceShare <- lookupKey "marketplaceShare" contents >>= fromData
    ownerPkh <- lookupKey "ownerPkh" contents >>= fromData
    ownerPrice <- lookupKey "ownerPrice" contents >>= fromData
    pure $ SeabugMetadata
      { policyId
      , mintPolicy
      , collectionNftCS
      , collectionNftTN
      , lockingScript
      , authorPkh
      , authorShare
      , marketplaceScript
      , marketplaceShare
      , ownerPkh
      , ownerPrice
      }

-- | Convert a byte array into a string as represented in the metadata
-- | json, i.e. hex encoded with "0x" prepended.
metadataBytesString :: ByteArray -> String
metadataBytesString = ("0x" <> _) <<< byteArrayToHex

-- | Attempt to decode seabug metadata at the key specified by the
-- | passed in `CurrencySymbol` (the `policyId`)
decodeSeabugMetadataAeson
  :: CurrencySymbol -> Aeson -> Either JsonDecodeError SeabugMetadata
decodeSeabugMetadataAeson policyId =
  caseAesonObject (Left (TypeMismatch "Expected object"))
    $ (_ .: policyIdField)
    >=> caseAesonObject (Left (TypeMismatch "Expected object")) parseMd
  where
  policyIdField = metadataBytesString $ getCurrencySymbol policyId

  parseMd o = do
    collectionNftCS <-
      ( note (TypeMismatch "Invalid CurrencySymbol")
          <<< mkCurrencySymbol
          <<< unwrap
          <=< decodeMetadataBytes
      )
        =<< getField o "collectionNftCS"
    collectionNftTN <-
      ( note (TypeMismatch "expected ASCII-encoded `TokenName`")
          <<< mkTokenName
          <<< unwrap
          <=< decodeMetadataBytes
      )
        =<< getField o "collectionNftTN"
    lockingScript <-
      map wrap
        <<< decodeScriptHash
        =<< getField o "lockingScript"
    authorPkh <- decodePkh =<< getField o "authorPkh"
    authorShare <- decodeShare =<< getField o "authorShare"
    marketplaceScript <- map wrap <<< decodeScriptHash
      =<< getField o "marketplaceScript"
    marketplaceShare <- decodeShare =<< getField o "marketplaceShare"
    ownerPkh <- decodePkh =<< getField o "ownerPkh"
    ownerPrice <- getField o "ownerPrice"
    mintPolicy <- getField o "mintPolicy"
    pure $ SeabugMetadata
      { -- Not used in the endpoints where we parse the metadata, so we
        -- can set a dummy value
        policyId
      , mintPolicy
      , collectionNftCS
      , collectionNftTN
      , lockingScript
      , authorPkh
      , authorShare
      , marketplaceScript
      , marketplaceShare
      , ownerPkh
      , ownerPrice
      }

  decodePkh :: String -> Either JsonDecodeError PubKeyHash
  decodePkh =
    map wrap
      <<< note (TypeMismatch "Invalid Ed25519KeyHash")
      <<< ed25519KeyHashFromBytes
      <=< decodeMetadataBytes

  decodeShare :: Int -> Either JsonDecodeError Share
  decodeShare = note (TypeMismatch "Expected int between 0 and 10000")
    <<< mkShare

  decodeScriptHash :: String -> Either JsonDecodeError ScriptHash
  decodeScriptHash =
    note (TypeMismatch "Expected hex-encoded script hash")
      <<< scriptHashFromBytes
      <=< decodeMetadataBytes

  decodeMetadataBytes :: String -> Either JsonDecodeError RawBytes
  decodeMetadataBytes =
    note (TypeMismatch "Invalid hex string in bytes field") <<< hexToRawBytes
      <=< note (TypeMismatch "Expected 0x prefix in bytes field")
      <<< stripPrefix (Pattern "0x")

newtype SeabugMetadataDelta = SeabugMetadataDelta
  { policyId :: CurrencySymbol
  , ownerPkh :: PubKeyHash
  , ownerPrice :: Natural
  }

derive instance Generic SeabugMetadataDelta _
derive instance Newtype SeabugMetadataDelta _
derive instance Eq SeabugMetadataDelta

instance Show SeabugMetadataDelta where
  show = genericShow

instance MetadataType SeabugMetadataDelta where
  metadataLabel _ = metadataLabel (Proxy :: Proxy SeabugMetadata)

instance ToMetadata SeabugMetadataDelta where
  toMetadata (SeabugMetadataDelta meta) = toMetadata
    [ meta.policyId /\
        [ "ownerPkh" /\ toMetadata meta.ownerPkh
        , "ownerPrice" /\ toMetadata meta.ownerPrice
        ]
    ]

instance FromMetadata SeabugMetadataDelta where
  fromMetadata (MetadataMap mp) = do
    policyId /\ contents <- case Map.toUnfoldable mp of
      [ policyId /\ contents ] ->
        Tuple <$> fromMetadata policyId <*> pure contents
      _ -> Nothing
    ownerPkh <- lookupMetadata "ownerPkh" contents >>= fromMetadata
    ownerPrice <- lookupMetadata "ownerPrice" contents >>= fromMetadata
    pure $ SeabugMetadataDelta { policyId, ownerPkh, ownerPrice }
  fromMetadata _ = Nothing

instance ToData SeabugMetadataDelta where
  toData (SeabugMetadataDelta meta) = unsafePartial $ toData $ AssocMap.Map
    [ unsafeMkKey "727" /\ AssocMap.Map
        [ meta.policyId /\ AssocMap.Map
            [ unsafeMkKey "ownerPkh" /\ toData meta.ownerPkh
            , unsafeMkKey "ownerPrice" /\ toData meta.ownerPrice
            ]
        ]
    ]

instance FromData SeabugMetadataDelta where
  fromData sm = unsafePartial do
    policyId /\ contents <- lookupKey "727" sm >>= case _ of
      Map [ policyId /\ contents ] ->
        Tuple <$> fromData policyId <*> fromData contents
      _ -> Nothing
    ownerPkh <- lookupKey "ownerPkh" contents >>= fromData
    ownerPrice <- lookupKey "ownerPrice" contents >>= fromData
    pure $ SeabugMetadataDelta { policyId, ownerPkh, ownerPrice }
