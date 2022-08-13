module Test.Contract.Minting (suite) where

import Contract.Prelude

import Contract.Address
  ( getNetworkId
  , getWalletAddress
  , validatorHashEnterpriseAddress
  )
import Contract.Monad (liftContractM, liftedM)
import Contract.PlutusData (fromData, getDatumByHash)
import Contract.Test.Plutip (runPlutipContract, withKeyWallet, withStakeKey)
import Contract.Transaction (TransactionOutput(..))
import Data.BigInt as BigInt
import Mote (test)
import Seabug.MarketPlace (marketplaceValidatorAddr)
import Seabug.Types (MarketplaceDatum(..))
import Test.Contract.Util
  ( assertContract
  , callMintCnft
  , callMintSgNft
  , checkNftAtAddress
  , findUtxoWithNft
  , mintParams1
  , plutipConfig
  , privateStakeKey
  )
import TestM (TestPlanM)

suite :: TestPlanM Unit
suite =
  test "Minting" do
    let
      distribution =
        ( withStakeKey privateStakeKey
            [ BigInt.fromInt 1_000_000_000
            , BigInt.fromInt 2_000_000_000
            ]
        )
    runPlutipContract plutipConfig distribution \alice ->
      withKeyWallet alice do
        cnft <- callMintCnft
        aliceAddr <- liftedM "Could not get addr" getWalletAddress
        assertContract "Could not find cnft at user address" =<<
          checkNftAtAddress cnft aliceAddr

        sgNft /\ nftData <- callMintSgNft cnft mintParams1

        scriptAddr <- marketplaceValidatorAddr
        TransactionOutput sgNftUtxo <-
          liftedM "Could not find sgNft at marketplace address" $
            findUtxoWithNft sgNft scriptAddr

        lockScriptAddr <- liftedM "Could not get locking script addr"
          $ validatorHashEnterpriseAddress
          <$> getNetworkId
          <*> pure (unwrap nftData # _.nftCollection # unwrap # _.lockingScript)
        assertContract "Could not find cnft at locking address" =<<
          checkNftAtAddress cnft lockScriptAddr

        -- TODO: Don't test the datums directly, test it via
        -- integration with the other contracts
        sgNftDatumHash <- liftContractM "sgNft utxo does not have datum hash"
          sgNftUtxo.dataHash
        rawMpDatum <- liftedM "Could not get sgNft utxo's datum" $
          getDatumByHash sgNftDatumHash
        MarketplaceDatum { getMarketplaceDatum: mpDatum } <-
          liftContractM "Could not parse sgNft utxo's datum"
            $ fromData
            $ unwrap rawMpDatum
        assertContract "Marketplace datum did not hold sgNft's info"
          (mpDatum == sgNft)
