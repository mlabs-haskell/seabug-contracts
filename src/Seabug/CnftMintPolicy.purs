module Seabug.CnftMintPolicy (mkCnftMintingPolicy) where

import Contract.Prelude

import Contract.Monad (Contract, liftContractE)
import Contract.PlutusData (toData)
import Contract.Scripts (MintingPolicy, applyArgs)
import Contract.Transaction (TransactionInput)
import Data.Argonaut (Json, JsonDecodeError)
import QueryM as QueryM
import Seabug.Helpers (jsonReader)

mkCnftMintingPolicy
  :: forall (r :: Row Type)
   . TransactionInput
  -> Contract r (Either QueryM.ClientError MintingPolicy)
mkCnftMintingPolicy oref = do
  p <- liftContractE cnftMintingPolicy
  applyArgs p [ toData oref ]

cnftMintingPolicy :: Either JsonDecodeError MintingPolicy
cnftMintingPolicy = jsonReader "mintingPolicy" _cnftMintingPolicy

foreign import _cnftMintingPolicy :: Json
