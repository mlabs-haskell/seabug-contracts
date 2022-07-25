module Seabug.MintingPolicy
  ( mkMintingPolicy
  , mkTokenName
  , unappliedMintingPolicy
  ) where

import Contract.Prelude

import Contract.Monad (Contract, liftContractE)
import Contract.PlutusData (toData)
import Contract.Scripts (MintingPolicy, applyArgs)
import Contract.Value (TokenName)
import Contract.Value as Value
import Data.Argonaut (Json, JsonDecodeError)
import QueryM as QueryM
import Seabug.Helpers (jsonReader)
import Seabug.Types (NftCollection(..), NftId, hash)

mkTokenName :: forall (r :: Row Type). NftId -> Contract r (Maybe TokenName)
mkTokenName nftId = hash nftId <#> maybe Nothing Value.mkTokenName

mkMintingPolicy
  :: forall (r :: Row Type)
   . NftCollection
  -> Contract r (Either QueryM.ClientError MintingPolicy)
mkMintingPolicy
  ( NftCollection
      { collectionNftCs
      , lockingScript
      , author
      , authorShare
      , daoScript
      , daoShare
      }
  ) = do
  p <- liftContractE unappliedMintingPolicy
  applyArgs p
    [ toData collectionNftCs
    , toData lockingScript
    , toData author
    , toData authorShare
    , toData daoScript
    , toData daoShare
    ]

unappliedMintingPolicy :: Either JsonDecodeError MintingPolicy
unappliedMintingPolicy = jsonReader "mintingPolicy" _mintingPolicy

foreign import _mintingPolicy :: Json
