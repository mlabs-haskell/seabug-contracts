module Seabug.Lock where

import Contract.Prelude

import Contract.Address (Slot)
import Contract.Monad (Contract, liftContractE)
import Contract.PlutusData (toData)
import Contract.Scripts (Validator, applyArgs)
import Contract.Value (CurrencySymbol)
import Data.Argonaut (Json, JsonDecodeError)
import Data.BigInt (BigInt)
import QueryM as QueryM
import Seabug.Helpers (jsonReader)

mkLockScript
  :: forall (r :: Row Type)
   . CurrencySymbol
  -> BigInt
  -> Slot
  -> Contract r (Either QueryM.ClientError Validator)
mkLockScript collectionNftCs lockup lockupEnd = do
  script <- liftContractE unappliedLockScript
  applyArgs script [ toData collectionNftCs, toData lockup, toData lockupEnd ]

unappliedLockScript :: Either JsonDecodeError Validator
unappliedLockScript = jsonReader "validator" _unappliedLockScript

foreign import _unappliedLockScript :: Json
