module Test.Contract.Buy (suite) where

import Contract.Prelude

import Contract.Test.Plutip (runPlutipContract, withKeyWallet, withStakeKey)
import Data.BigInt as BigInt
import Mote (only, test)
import Seabug.Contract.MarketPlaceBuy (marketplaceBuy)
import Test.Contract.Util
  ( callMintCnft
  , callMintSgNft
  , mintParams1
  , plutipConfig
  , privateStakeKey
  )
import TestM (TestPlanM)

suite :: TestPlanM Unit
suite =
  only $ test "Buy" do
    let
      distribution =
        ( withStakeKey privateStakeKey
            [ BigInt.fromInt 1_000_000_000
            , BigInt.fromInt 2_000_000_000
            ]
        ) /\
          ( withStakeKey privateStakeKey
              [ BigInt.fromInt 1_000_000_000
              , BigInt.fromInt 2_000_000_000
              ]
          )
    runPlutipContract plutipConfig distribution \(alice /\ bob) -> do
      withKeyWallet alice do
        cnft <- callMintCnft
        _ /\ nftData <- callMintSgNft cnft mintParams1
        withKeyWallet bob do
          marketplaceBuy nftData
