module Test.Contract.Sell (suite) where

import Contract.Prelude

import Contract.Address (getWalletAddress)
import Contract.Monad (Contract, liftedM)
import Contract.Test.Plutip (runPlutipContract, withKeyWallet, withStakeKey)
import Contract.Transaction (awaitTxConfirmed)
import Contract.Wallet (KeyWallet)
import Data.BigInt as BigInt
import Mote (group, test)
import Seabug.Contract.Buy (marketplaceBuy')
import Seabug.Contract.MarketPlaceSell (marketPlaceSell)
import Seabug.Contract.Util (ReturnBehaviour(ToCaller))
import Seabug.MarketPlace (marketplaceValidatorAddr)
import Seabug.Types (MarketplaceDatum(..))
import Test.Contract.Util
  ( assertContract
  , assertOutputHasDatum
  , callMintCnft
  , callMintSgNft'
  , checkNftAtAddress
  , findUtxoWithNft
  , mintParams1
  , plutipConfig
  , privateStakeKey1
  , privateStakeKey2
  )
import TestM (TestPlanM)

suite :: TestPlanM Unit
suite =
  group "Sell (Allow artist/users to sell NFT tokens)" do
    test "Successful Sell and rebuy" do
      withMinterAndBuyer \minter buyer -> do
        nftData <- withKeyWallet minter do
          cnft <- callMintCnft pure
          minterAddr <- liftedM "Could not get addr" getWalletAddress
          assertContract "Could not find cnft at minter address" =<<
            checkNftAtAddress cnft minterAddr

          { sgNft, nftData } <- callMintSgNft' ToCaller cnft mintParams1 pure

          assertContract "Could not find sgnft at minter address" =<<
            checkNftAtAddress sgNft minterAddr

          log "Selling nft to market"
          sellTxHash <- marketPlaceSell sgNft
          awaitTxConfirmed sellTxHash
          log $ "Sell transaction confirmed: " <> show sellTxHash

          scriptAddr <- marketplaceValidatorAddr
          sgNftUtxo <- liftedM "Could not find sgNft at marketplace address" $
            findUtxoWithNft sgNft scriptAddr

          assertOutputHasDatum "sgNft"
            (MarketplaceDatum { getMarketplaceDatum: sgNft })
            (==)
            sgNftUtxo

          pure nftData

        withKeyWallet buyer do
          buyerAddr <- liftedM "Could not get addr" getWalletAddress
          log "Buying listed nft"
          buyTxHash /\ txData <- marketplaceBuy' ToCaller nftData
          awaitTxConfirmed buyTxHash
          log $ "Buy transaction confirmed: " <> show buyTxHash

          assertContract "Could not find sgnft at buyer address" =<<
            checkNftAtAddress txData.newAsset buyerAddr

withMinterAndBuyer
  :: forall (a :: Type). (KeyWallet -> KeyWallet -> Contract () a) -> Aff a
withMinterAndBuyer f =
  runPlutipContract plutipConfig
    ( withStakeKey privateStakeKey1
        [ BigInt.fromInt 1_000_000_000
        , BigInt.fromInt 2_000_000_000
        ]
        /\ withStakeKey privateStakeKey2
          [ BigInt.fromInt 1_000_000_000
          , BigInt.fromInt 2_000_000_000
          ]
    )
    \(minter /\ buyer) -> f minter buyer
