module Main (main) where

import Contract.Prelude

import Contract.Address (ownPaymentPubKeyHash)
import Contract.Monad (defaultContractConfig, runContract_)
import Effect.Aff (launchAff_)

main :: Effect Unit
main = launchAff_ $ do
  cfg <- defaultContractConfig
  runContract_ cfg
    $ log
    <<< show
    =<< ownPaymentPubKeyHash
