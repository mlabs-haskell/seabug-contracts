module Seabug.Types
  ( MarketplaceDatum(..)
  , LockDatum(..)
  , MintAct(..)
  , MintParams(..)
  , NftCollection(..)
  , NftData(..)
  , NftId(..)
  , MintCnftParams(..)
  , class Hashable
  , hash
  ) where

import Contract.Prelude

import Contract.Address (PaymentPubKeyHash, PubKeyHash)
import Contract.Aeson as Aeson
import Contract.Monad (Contract)
import Contract.Numeric.Natural (Natural, toBigInt)
import Contract.PlutusData
  ( class FromData
  , class ToData
  , PlutusData(Constr)
  , fromData
  , toData
  )
import Contract.Prim.ByteArray (ByteArray, byteArrayFromIntArrayUnsafe)
import Contract.Scripts (ValidatorHash)
import Contract.Time (Slot)
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , getCurrencySymbol
  , getTokenName
  )
import Data.Argonaut as Json
import Data.BigInt (BigInt, fromInt, toInt)
import Hashing (blake2b256Hash)
import Partial.Unsafe (unsafePartial)
import Serialization.Hash (ed25519KeyHashToBytes, scriptHashToBytes)

newtype MintCnftParams = MintCnftParams
  { imageUri :: String
  -- | The token name of the collection nft. Will be base64 encoded
  , tokenNameString :: String
  , name :: String
  , description :: String
  }

derive instance Generic MintCnftParams _
derive instance Newtype MintCnftParams _
derive newtype instance Eq MintCnftParams

instance Show MintCnftParams where
  show = genericShow

-- Field names have been simplified due to row polymorphism. Please let me know
-- if the field names must be exact.
-- | Parameters that need to be submitted when minting a new NFT.
newtype MintParams = MintParams
  { -- | Shares retained by author.
    authorShare :: Natural
  , daoShare :: Natural
  , -- | Listing price of the NFT, in Lovelace.
    price :: Natural
  , lockLockup :: BigInt
  , lockLockupEnd :: Slot
  , feeVaultKeys :: Array PubKeyHash -- TODO: check if this is needed
  }

derive instance Generic MintParams _
derive instance Newtype MintParams _
derive newtype instance Eq MintParams

instance Show MintParams where
  show = genericShow

instance FromData MintParams where
  fromData (Constr n [ as, ds, pr, ll, lle, fvk ]) | n == zero =
    MintParams <$>
      ( { authorShare: _
        , daoShare: _
        , price: _
        , lockLockup: _
        , lockLockupEnd: _
        , feeVaultKeys: _
        }
          <$> fromData as
          <*> fromData ds
          <*> fromData pr
          <*> fromData ll
          <*> fromData lle
          <*> fromData fvk
      )
  fromData _ = Nothing

instance ToData MintParams where
  toData
    ( MintParams
        { authorShare
        , daoShare
        , price
        , lockLockup
        , lockLockupEnd
        , feeVaultKeys
        }
    ) =
    Constr zero
      [ toData authorShare
      , toData daoShare
      , toData price
      , toData lockLockup
      , toData lockLockupEnd
      , toData feeVaultKeys
      ]

newtype NftId = NftId
  { collectionNftTn :: TokenName
  , price :: Natural
  , owner :: PaymentPubKeyHash
  }

derive instance Generic NftId _
derive instance Newtype NftId _
derive newtype instance Eq NftId
derive newtype instance Ord NftId

instance Show NftId where
  show = genericShow

instance FromData NftId where
  fromData (Constr n [ cnt, pr, own ]) | n == zero =
    NftId <$>
      ( { collectionNftTn: _, price: _, owner: _ }
          <$> fromData cnt
          <*> fromData pr
          <*> fromData own
      )
  fromData _ = Nothing

instance ToData NftId where
  toData (NftId { collectionNftTn, price, owner }) =
    Constr zero [ toData collectionNftTn, toData price, toData owner ]

-- Field names have been simplified due to row polymorphism. Please let me know
-- if the field names must be exact.
newtype NftCollection = NftCollection
  { collectionNftCs :: CurrencySymbol
  , lockLockup :: BigInt
  , lockLockupEnd :: Slot
  , lockingScript :: ValidatorHash
  , author :: PaymentPubKeyHash
  , authorShare :: Natural
  , daoScript :: ValidatorHash
  , daoShare :: Natural
  }

derive instance Generic NftCollection _
derive instance Newtype NftCollection _
derive newtype instance Eq NftCollection
derive newtype instance Ord NftCollection

-- Note the renaming of fields from their Plutus equivalents, e.g.
-- "nftCollection'collectionNftCs" to "collectionNftCs".
instance Aeson.DecodeAeson NftCollection where
  decodeAeson j =
    Aeson.caseAesonObject
      (Left $ Json.TypeMismatch "Expected Json Object")
      ( \o ->
          do
            collectionNftCs <- Aeson.getField o "nftCollection'collectionNftCs"
            lockLockupEnd <- Aeson.getField o "nftCollection'lockLockupEnd"
            lockingScript <- Aeson.getField o "nftCollection'lockingScript"
            author <- Aeson.getField o "nftCollection'author"
            authorShare <- Aeson.getField o "nftCollection'authorShare"
            daoScript <- Aeson.getField o "nftCollection'daoScript"
            daoShare <- Aeson.getField o "nftCollection'daoShare"
            lockLockup <- Aeson.getField o "nftCollection'lockLockup"
            pure $ NftCollection
              { collectionNftCs
              , lockLockup
              , lockLockupEnd
              , lockingScript
              , author
              , authorShare
              , daoScript
              , daoShare
              }
      )
      j

instance Show NftCollection where
  show = genericShow

newtype NftData = NftData
  { nftCollection :: NftCollection
  , nftId :: NftId
  }

derive instance Generic NftData _
derive instance Newtype NftData _
derive newtype instance Eq NftData
derive newtype instance Ord NftData

instance Show NftData where
  show = genericShow

newtype SetPriceParams = SetPriceParams
  { -- | Token which price is set.
    nftData :: NftData
  , -- | New price, in Lovelace.
    price :: Natural
  }

derive instance Generic SetPriceParams _
derive instance Newtype SetPriceParams _
derive newtype instance Eq SetPriceParams

instance Show SetPriceParams where
  show = genericShow

newtype ChangeOwnerParams = ChangeOwnerParams
  { -- | Token which owner is set.
    nftData :: NftData
  , -- | New Owner
    owner :: PaymentPubKeyHash
  }

derive instance Generic ChangeOwnerParams _
derive instance Newtype ChangeOwnerParams _
derive newtype instance Eq ChangeOwnerParams

instance Show ChangeOwnerParams where
  show = genericShow

data MintAct
  = MintToken NftId
  | ChangePrice NftId Natural
  | ChangeOwner NftId PaymentPubKeyHash
  | BurnToken NftId

derive instance Generic MintAct _

instance Show MintAct where
  show = genericShow

instance ToData MintAct where
  toData (MintToken nft) = Constr zero [ toData nft ]
  toData (ChangePrice nft price) = Constr one [ toData nft, toData price ]
  toData (ChangeOwner nft pkh) = Constr (fromInt 2) [ toData nft, toData pkh ]
  toData (BurnToken nft) = Constr (fromInt 3) [ toData nft ]

instance FromData MintAct where
  fromData (Constr n [ nft ])
    | n == zero = MintToken <$> fromData nft
    | n == fromInt 3 = BurnToken <$> fromData nft
  fromData (Constr n [ nft, m ])
    | n == one = ChangePrice <$> fromData nft <*> fromData m
    | n == fromInt 2 = ChangeOwner <$> fromData nft <*> fromData m
  fromData _ = Nothing

data LockAct
  = Unstake PaymentPubKeyHash Natural
  | Restake PaymentPubKeyHash Natural

derive instance Generic LockAct _

instance Show LockAct where
  show = genericShow

instance ToData LockAct where
  toData (Unstake pkh n) = Constr zero [ toData pkh, toData n ]
  toData (Restake pkh n) = Constr one [ toData pkh, toData n ]

instance FromData LockAct where
  fromData (Constr n [ pkh, m ])
    | n == zero = Unstake <$> fromData pkh <*> fromData m
    | n == one = Restake <$> fromData pkh <*> fromData m
  fromData _ = Nothing

newtype LockDatum = LockDatum
  { sgNft :: CurrencySymbol
  , entered :: Slot
  , underlyingTn :: TokenName
  }

derive instance Generic LockDatum _
derive instance Newtype LockDatum _
derive newtype instance Eq LockDatum

instance Show LockDatum where
  show = genericShow

instance ToData LockDatum where
  toData (LockDatum { sgNft, entered, underlyingTn }) =
    Constr zero [ toData sgNft, toData entered, toData underlyingTn ]

instance FromData LockDatum where
  fromData (Constr n [ cs, s, tn ])
    | n == zero = LockDatum <$>
        ( { sgNft: _, entered: _, underlyingTn: _ }
            <$> fromData cs
            <*> fromData s
            <*> fromData tn
        )
  fromData _ = Nothing

newtype MarketplaceDatum = MarketplaceDatum
  { getMarketplaceDatum :: CurrencySymbol /\ TokenName }

derive instance Generic MarketplaceDatum _
derive instance Newtype MarketplaceDatum _
derive newtype instance Eq MarketplaceDatum
derive newtype instance Ord MarketplaceDatum

instance Show MarketplaceDatum where
  show = genericShow

instance ToData MarketplaceDatum where
  toData (MarketplaceDatum { getMarketplaceDatum }) = toData getMarketplaceDatum

instance FromData MarketplaceDatum where
  fromData (Constr n [ cs, tn ]) | n == zero = do
    curr <- fromData cs
    name <- fromData tn
    pure $ MarketplaceDatum { getMarketplaceDatum: curr /\ name }
  fromData _ = Nothing

-- The Contract Maybe is because we use a Haskell server
class Hashable (a :: Type) where
  hash
    :: forall (r :: Row Type)
     . a
    -> Contract r (Maybe ByteArray) -- Plutus BuiltinByteString

instance Hashable ByteArray where
  hash = map Just <<< liftAff <<< blake2b256Hash

instance Hashable Natural where
  hash = hash <<< toBin <<< toBigInt
    where
    toBin :: BigInt -> ByteArray
    toBin n = toBin' n mempty

    threshold :: BigInt
    threshold = fromInt 256

    -- This function is generally unsafe but our usage below is safe because
    -- either, it is below 256 or we have used modulo arithmetic.
    toInt' :: BigInt -> Int
    toInt' = unsafePartial fromJust <<< toInt

    -- Should be safe to use `byteArrayFromIntArrayUnsafe` since in both
    -- cases, n' < 256.
    toBin' :: BigInt -> ByteArray -> ByteArray
    toBin' n' rest
      | n' < threshold = byteArrayFromIntArrayUnsafe [ toInt' n' ] <> rest
      | otherwise =
          toBin'
            (n' `div` threshold)
            ( byteArrayFromIntArrayUnsafe [ toInt' $ n' `mod` threshold ] <>
                rest
            )

instance Hashable CurrencySymbol where
  hash = hash <<< getCurrencySymbol

instance Hashable TokenName where
  hash = hash <<< getTokenName

instance Hashable ValidatorHash where
  hash = hash <<< unwrap <<< scriptHashToBytes <<< unwrap

instance Hashable PaymentPubKeyHash where
  hash = hash <<< unwrap <<< ed25519KeyHashToBytes <<< unwrap <<< unwrap

instance (Hashable a, Hashable b) => Hashable (a /\ b) where
  hash (a /\ b) = ((<>) <$> hash a <*> hash b) >>= maybe (pure Nothing) hash

instance Hashable NftId where
  hash (NftId { collectionNftTn, price, owner }) =
    op3 <$> hash price <*> hash owner <*> hash collectionNftTn
      >>= maybe (pure Nothing) hash
    where
    op3 :: forall (a :: Type). Semigroup a => a -> a -> a -> a
    op3 a b c = a <> b <> c
