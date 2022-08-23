module Test.Contract.Minting (suite) where

import Contract.Prelude

import Contract.Address (getWalletAddress, scriptHashAddress)
import Contract.Chain (currentSlot)
import Contract.Monad (liftContractE, liftedM)
import Contract.Test.Plutip (runPlutipContract, withKeyWallet, withStakeKey)
import Data.BigInt as BigInt
import Mote (only, test)
import Seabug.Contract.Util (getSeabugMetadata)
import Seabug.MarketPlace (marketplaceValidatorAddr)
import Seabug.Types (LockDatum(..), MarketplaceDatum(..))
import Test.Contract.Util
  ( assertContract
  , assertTxHasMetadata
  , callMintCnft
  , callMintSgNft
  , checkNftAtAddress
  , assertOutputHasDatum
  , findUtxoWithNft
  , mintParams1
  , plutipConfig
  , privateStakeKey1
  )
import TestM (TestPlanM)

suite :: TestPlanM Unit
suite =
  only $ test "Minting" do
    let
      distribution =
        ( withStakeKey privateStakeKey1
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

        expectedEntered <- currentSlot
        { sgNft, nftData, txHash } <- callMintSgNft cnft mintParams1

        scriptAddr <- marketplaceValidatorAddr
        sgNftUtxo <-
          liftedM "Could not find sgNft at marketplace address" $
            findUtxoWithNft sgNft scriptAddr

        let
          nftColl = unwrap nftData # _.nftCollection # unwrap
          lockScriptAddr = scriptHashAddress nftColl.lockingScript
        cnftUtxo <-
          liftedM "Could not find cnft at locking address" $
            findUtxoWithNft cnft lockScriptAddr

        assertOutputHasDatum "cnft"
          ( LockDatum
              { sgNft: fst sgNft
              , entered: expectedEntered
              , underlyingTn: snd cnft
              }
          )
          ( \(LockDatum exp) (LockDatum act) -> exp.sgNft == act.sgNft
              && exp.underlyingTn
              == act.underlyingTn
          )
          cnftUtxo
        assertOutputHasDatum "sgNft"
          (MarketplaceDatum { getMarketplaceDatum: sgNft })
          (==)
          sgNftUtxo

        expectedSeabugMetadata <- liftContractE $
          getSeabugMetadata nftData (fst sgNft)
        assertTxHasMetadata "sgNft" txHash expectedSeabugMetadata

        pure unit
