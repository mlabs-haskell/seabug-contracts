module Seabug.Lock where

import Contract.Prelude

import Contract.Address (Slot)
import Contract.Monad (Contract, liftContractE, liftedE)
import Contract.PlutusData (toData)
import Contract.Scripts (Validator, applyArgs)
import Contract.Value (CurrencySymbol)
import Data.Argonaut (Json, JsonDecodeError)
import Data.BigInt (BigInt)
import Seabug.Helpers (jsonReader)

mkLockScript
  :: forall (r :: Row Type)
   . CurrencySymbol
  -> BigInt
  -> Slot
  -> Contract r Validator
mkLockScript collectionNftCs lockup lockupEnd = do
  script <- liftContractE unappliedLockScript
  liftedE $ applyArgs script
    [ toData collectionNftCs, toData lockup, toData lockupEnd ]

unappliedLockScript :: Either JsonDecodeError Validator
unappliedLockScript = jsonReader "validator" _unappliedLockScript

foreign import _unappliedLockScript :: Json
