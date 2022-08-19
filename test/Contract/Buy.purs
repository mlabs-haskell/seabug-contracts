module Test.Contract.Buy (suite) where

import Contract.Prelude

import Contract.Address
  ( Address
  , getNetworkId
  , getWalletAddress
  , ownPaymentPubKeyHash
  , payPubKeyHashEnterpriseAddress
  )
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.Numeric.Natural as Nat
import Contract.Test.Plutip (runPlutipContract, withKeyWallet, withStakeKey)
import Contract.Transaction
  ( Transaction(..)
  , TransactionHash
  , TransactionOutput(..)
  , TxBody(..)
  , awaitTxConfirmed
  , getTxByHash
  )
import Contract.Value (CurrencySymbol, TokenName, getLovelace)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Mote (group, only, test)
import Plutus.Conversion (toPlutusCoin)
import Record (merge)
import Seabug.Contract.Buy (marketplaceBuy')
import Seabug.Contract.Util (ReturnBehaviour(..), SeabugTxData)
import Seabug.MarketPlace (marketplaceValidatorAddr)
import Seabug.Types (MintParams(..))
import Test.Contract.Util
  ( class WrappingAssertion
  , ContractWrapAssertion
  , assertContract
  , assertLovelaceDecAtAddr
  , assertLovelaceIncAtAddr'
  , callMintCnft
  , callMintSgNft
  , checkNftAtAddress
  , findUtxoWithNft
  , mintParams1
  , mintParams2
  , mintParams3
  , mintParams4
  , plutipConfig
  , privateStakeKey1
  , privateStakeKey2
  , valueToLovelace
  , withAssertions
  , wrapAndAssert
  )
import TestM (TestPlanM)

type BuyTestData = BuyTestData' ()

type BuyTestData' r =
  { sellerPayAddr :: Address -- The enterprise address of the seller
  , buyerAddr :: Address -- The address used by the buyer
  , authorPayAddr :: Address -- The enterprise address of the author
  , mpScriptAddr :: Address -- The address of the marketplace script
  , mintParams :: MintParams -- The params used to mint the bought nft
  , sgNft :: CurrencySymbol /\ TokenName -- The nft being bought
  | r
  }

type PostBuyTestData = BuyTestData'
  ( txData :: SeabugTxData -- The data of the buy transaction
  , txHash :: TransactionHash -- The hash of the buy transaction
  )

type ExpectedShares =
  { minMpGain :: BigInt
  , minSellerGain :: BigInt
  , minAuthorGain :: BigInt
  }

type BuyTestConfig assertions =
  { mintParams :: MintParams
  , expectedShares :: ExpectedShares
  , retBehaviour :: ReturnBehaviour
  , assertions :: assertions
  }

buyTestConfig1 :: BuyTestConfig _
buyTestConfig1 =
  { mintParams: mintParams1
  , expectedShares:
      { minMpGain: BigInt.fromInt $ 10 * 1000000
      , minSellerGain: BigInt.fromInt $ 90 * 1000000
      , minAuthorGain: BigInt.fromInt $ 90 * 1000000
      }
  , retBehaviour: ToMarketPlace
  , assertions: nftToMarketPlaceAssert
  }

buyTestConfig2 :: BuyTestConfig _
buyTestConfig2 =
  { mintParams: mintParams2
  , expectedShares:
      { minMpGain: BigInt.fromInt 0
      , minSellerGain: BigInt.fromInt $ 100 * 1000000
      , minAuthorGain: BigInt.fromInt $ 100 * 1000000
      }
  , retBehaviour: ToMarketPlace
  , assertions: nftToMarketPlaceAssert
  }

buyTestConfig3 :: BuyTestConfig _
buyTestConfig3 =
  { mintParams: mintParams3
  , expectedShares:
      { minMpGain: BigInt.fromInt $ 10 * 1000000
      , minSellerGain: BigInt.fromInt $ 90 * 1000000
      , minAuthorGain: BigInt.fromInt $ 90 * 1000000
      }
  , retBehaviour: ToMarketPlace
  , assertions: nftToMarketPlaceAssert
  }

buyTestConfig4 :: BuyTestConfig _
buyTestConfig4 = buyTestConfig2 { mintParams = mintParams4 }

suite :: TestPlanM Unit
suite =
  only $ group "Buy" do
    test "Seller is author, no low prices, nft to marketplace" $
      mkBuyTest buyTestConfig1
    test "Seller is author, no low prices, nft to buyer" $
      mkBuyTest buyTestConfig1
        { retBehaviour = ToCaller
        , assertions = nftToBuyerAssert
        }
    test "Seller is author, low marketplace share, nft to marketplace" $
      mkBuyTest buyTestConfig2
    test "Seller is author, low marketplace share, nft to buyer" $
      mkBuyTest buyTestConfig2
        { retBehaviour = ToCaller
        , assertions = nftToBuyerAssert
        }
    test "Seller is author, low author share, nft to marketplace" $
      mkBuyTest buyTestConfig3
    only $ test "Seller is author, low author share, nft to buyer" $
      mkBuyTest buyTestConfig3
        { retBehaviour = ToCaller
        , assertions = nftToBuyerAssert
        }
    test
      "Seller is author, low author and marketplace shares, nft to marketplace"
      $
        mkBuyTest buyTestConfig4
    only
      $ test
          "Seller is author, low author and marketplace shares, nft to buyer"
      $
        mkBuyTest buyTestConfig4
          { retBehaviour = ToCaller
          , assertions = nftToBuyerAssert
          }

nftToMarketPlaceAssert :: PostBuyTestData -> Array (Contract () Unit)
nftToMarketPlaceAssert o@{ mpScriptAddr } =
  [ assertAddrHasNewAsset mpScriptAddr o
  , assertAddrLacksOldAsset mpScriptAddr o
  ]

nftToBuyerAssert :: PostBuyTestData -> Array (Contract () Unit)
nftToBuyerAssert o@{ buyerAddr, mpScriptAddr } =
  [ assertAddrHasNewAsset buyerAddr o, assertAddrLacksOldAsset mpScriptAddr o ]

mkBuyTest
  :: forall f
   . WrappingAssertion f () PostBuyTestData
  => BuyTestConfig f
  -> Aff Unit
mkBuyTest { mintParams, expectedShares, retBehaviour, assertions } =
  runBuyTest mintParams retBehaviour
    (\b -> mkShareAssertions expectedShares b /\ assertions)

assertAddrHasNewAsset :: Address -> PostBuyTestData -> Contract () Unit
assertAddrHasNewAsset addr { txData } =
  assertContract "Address did not contain new sgNft"
    =<< checkNftAtAddress txData.newAsset addr

assertAddrLacksOldAsset :: Address -> PostBuyTestData -> Contract () Unit
assertAddrLacksOldAsset addr { txData } =
  assertContract "Address contained old sgNft"
    =<< not
    <$> checkNftAtAddress txData.oldAsset addr

mkShareAssertions
  :: ExpectedShares
  -> BuyTestData
  -> Array (ContractWrapAssertion () PostBuyTestData)
mkShareAssertions
  e@{ minSellerGain, minAuthorGain }
  b@{ sellerPayAddr, authorPayAddr } =
  [ assertLovelaceIncAtAddr' "Author" authorPayAddr minAuthorGain
  , assertLovelaceIncAtAddr' "Seller" sellerPayAddr minSellerGain
  , buyerMarketplaceShareAssert e b
  ]

buyerMarketplaceShareAssert
  :: ExpectedShares -> BuyTestData -> ContractWrapAssertion () PostBuyTestData
buyerMarketplaceShareAssert
  { minMpGain }
  { buyerAddr, mpScriptAddr, mintParams: MintParams mintParams, sgNft }
  contract = do
  (TransactionOutput mpNftUtxo) <- liftedM "Could not find sgNft utxo"
    $ findUtxoWithNft sgNft mpScriptAddr
  let
    mpInit = valueToLovelace mpNftUtxo.amount

    getBuyerExpectedLoss :: PostBuyTestData -> Contract () BigInt
    getBuyerExpectedLoss { txHash } = do
      (Transaction { body: TxBody { fee } }) <-
        liftedM "Could not fetch buy transaction" $ getTxByHash txHash
      let mpRemainder = mpInit - getLovelace (toPlutusCoin fee)
      pure $ (Nat.toBigInt mintParams.price) - mpRemainder
  contract `wrapAndAssert`
    [ assertLovelaceIncAtAddr' "Marketplace" mpScriptAddr (minMpGain - mpInit)
    , assertLovelaceDecAtAddr "Buyer" buyerAddr getBuyerExpectedLoss
    ]

runBuyTest
  :: forall (f :: Type)
   . WrappingAssertion f () PostBuyTestData
  => MintParams
  -> ReturnBehaviour
  -> (BuyTestData -> f)
  -> Aff Unit
runBuyTest mintParams retBehaviour getAssertions = do
  let
    distribution =
      ( withStakeKey privateStakeKey1
          [ BigInt.fromInt 1_000_000_000
          , BigInt.fromInt 2_000_000_000
          ]
      ) /\
        ( withStakeKey privateStakeKey2
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
    sgNft /\ nftData <- withKeyWallet seller do
      cnft <- callMintCnft
      callMintSgNft cnft mintParams
    withKeyWallet buyer do
      buyerAddr <- liftedM "Could not get buyer addr" getWalletAddress
      mpScriptAddr <- marketplaceValidatorAddr
      let
        buyTestData =
          { authorPayAddr: sellerPayAddr
          , sellerPayAddr
          , buyerAddr
          , mpScriptAddr
          , mintParams
          , sgNft: sgNft
          }
      void $ withAssertions (getAssertions buyTestData) do
        txHash /\ txData <- marketplaceBuy' retBehaviour nftData
        awaitTxConfirmed txHash
        pure $ merge buyTestData { txData, txHash }
