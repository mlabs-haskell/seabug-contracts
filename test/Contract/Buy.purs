module Test.Contract.Buy (suite) where

import Contract.Prelude

import Contract.Address
  ( getNetworkId
  , getWalletAddress
  , ownPaymentPubKeyHash
  , payPubKeyHashEnterpriseAddress
  )
import Contract.Monad (liftContractM, liftedM)
import Contract.Numeric.Natural as Nat
import Contract.Test.Plutip (runPlutipContract, withKeyWallet, withStakeKey)
import Contract.Transaction (awaitTxConfirmed)
import Data.BigInt as BigInt
import Mote (only, test)
import Seabug.Contract.Buy (marketplaceBuy')
import Seabug.Contract.Util (ReturnBehaviour(..))
import Seabug.MarketPlace (marketplaceValidatorAddr)
import Test.Contract.Util
  ( assertContract
  , assertLovelaceDecAtAddr
  , assertLovelaceIncAtAddr
  , callMintCnft
  , callMintSgNft
  , checkNftAtAddress
  , findUtxoWithNft
  , mintParams1
  , plutipConfig
  , privateStakeKey
  , withAssertions
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
    runPlutipContract plutipConfig distribution \(seller /\ buyer) -> do
      networkId <- getNetworkId
      sellerPayAddr <- withKeyWallet seller do
        sellerPkh <- liftedM "Cannot get seller pkh" ownPaymentPubKeyHash
        liftContractM "Could not get seller payment address" $
          payPubKeyHashEnterpriseAddress networkId sellerPkh
      oldSgNft /\ nftData <- withKeyWallet seller do
        cnft <- callMintCnft
        callMintSgNft cnft mintParams1
      withKeyWallet buyer do
        buyerAddr <- liftedM "Could not get buyer addr" getWalletAddress
        mpScriptAddr <- marketplaceValidatorAddr
        let
          minBuyerLoss = Nat.toBigInt (unwrap mintParams1).price
          minMpGain = BigInt.fromInt $ 10 * 1000000
          minSellerGain = BigInt.fromInt $ 90 * 1000000
        withAssertions
          [ assertLovelaceDecAtAddr "Buyer" buyerAddr minBuyerLoss
          , assertLovelaceIncAtAddr "Marketplace" mpScriptAddr minMpGain
          , assertLovelaceIncAtAddr "Seller" sellerPayAddr minSellerGain
          ]
          do
            txHash /\ txData <- marketplaceBuy' ToMarketPlace nftData
            awaitTxConfirmed txHash
            newSgNftUtxo <-
              liftedM "Marketplace script did not contain new sgNft"
                $ findUtxoWithNft txData.newAsset mpScriptAddr
            assertContract "Marketplace script contained old sgNft"
              =<< not
              <$> checkNftAtAddress oldSgNft mpScriptAddr

        pure unit
