module Main (main) where

import Contract.Prelude

import Contract.Address (ownPaymentPubKeyHash)
import Contract.Monad (defaultTestnetContractConfig, runContract_)
import Effect.Aff (launchAff_)

main :: Effect Unit
main = launchAff_ $ do
  cfg <- defaultTestnetContractConfig
  runContract_ cfg
    $ log
    <<< show
    =<< ownPaymentPubKeyHash
