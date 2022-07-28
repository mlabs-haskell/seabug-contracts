module Main (main) where

import Contract.Prelude

import Contract.Address (ownPaymentPubKeyHash)
import Contract.Config (testnetConfig)
import Contract.Monad (runContract)
import Effect.Aff (launchAff_)

main :: Effect Unit
main = launchAff_ $ do
  runContract testnetConfig
    $ log
    <<< show
    =<< ownPaymentPubKeyHash
