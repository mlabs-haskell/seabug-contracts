module Seabug.CnftMintPolicy
  ( mkCnftMintingPolicy
  , main
  ) where

import Contract.Prelude

import Contract.Monad
  ( Contract
  , defaultTestnetContractConfig
  , launchAff_
  , liftContractE
  , liftedE
  , runContract_
  )
import Contract.PlutusData (toData)
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Scripts (MintingPolicy, applyArgs)
import Contract.Transaction (TransactionInput)
import Contract.Value (scriptCurrencySymbol)
import Data.Argonaut (Json, JsonDecodeError)
import Data.UInt (fromInt)
import QueryM as QueryM
import Seabug.Helpers (jsonReader)

mkCnftMintingPolicy
  :: forall (r :: Row Type)
   . TransactionInput
  -> Contract r (Either QueryM.ClientError MintingPolicy)
mkCnftMintingPolicy oref = do
  p <- liftContractE cnftMintingPolicy
  applyArgs p [ toData oref ]

main :: Effect Unit
main = launchAff_ $ do
  cfg <- defaultTestnetContractConfig
  runContract_ cfg $ do
    policy <- liftedE $ mkCnftMintingPolicy
      ( wrap
          { transactionId: wrap $ hexToByteArrayUnsafe
              "00e52d6fdb45e529dda8cfaa4e7f04dc8b94deffd1bd54196193cc2c5c49e418"
          , index: fromInt 5
          }
      )
    log $ show policy
    log <<< show =<< (liftAff $ scriptCurrencySymbol policy)

cnftMintingPolicy :: Either JsonDecodeError MintingPolicy
cnftMintingPolicy = jsonReader "mintingPolicy" _cnftMintingPolicy

foreign import _cnftMintingPolicy :: Json
