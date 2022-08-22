module Test.Contract.Buy (suite) where

import Contract.Prelude

import Contract.Address (Address, getWalletAddress)
import Contract.Monad (Contract, liftedM)
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
import Data.Array ((:))
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Mote (group, only, skip, test)
import Plutus.Conversion (toPlutusCoin)
import Record (merge)
import Seabug.Contract.Buy (marketplaceBuy')
import Seabug.Contract.Util (ReturnBehaviour(..), SeabugTxData, modify)
import Seabug.MarketPlace (marketplaceValidatorAddr)
import Seabug.Types (MintParams(..))
import Test.Contract.Util
  ( class WrappingAssertion
  , ContractWrapAssertion
  , assertContract
  , assertLossAtAddr
  , assertGainAtAddr'
  , callMintCnft
  , callMintSgNft
  , checkNftAtAddress
  , findUtxoWithNft
  , mintParams1
  , mintParams2
  , mintParams3
  , mintParams4
  , mintParams5
  , mintParams6
  , mintParams7
  , mintParams8
  , plutipConfig
  , privateStakeKey1
  , privateStakeKey2
  , privateStakeKey3
  , valueToLovelace
  , walletEnterpriseAddress
  , withAssertions
  , wrapAndAssert
  )
import Test.Spec.Assertions (expectError)
import TestM (TestPlanM)

type BuyTestData = BuyTestData' ()

type BuyTestData' (r :: Row Type) =
  { sellerPayAddr :: Address -- The enterprise address of the seller
  , buyerAddr :: Address -- The address used by the buyer
  , authorPayAddr :: Address -- The enterprise address of the author
  , mpScriptAddr :: Address -- The address of the marketplace script
  , mintParams :: MintParams -- The params used to mint the bought nft
  , sgNft :: CurrencySymbol /\ TokenName -- The nft being bought
  , nftToBuyer :: Boolean -- Whether the nft is being sent directly to the buyer
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

type BuyTestConfig (assertions :: Type) =
  { mintParams :: MintParams
  , expectedShares :: ExpectedShares
  , retBehaviour :: ReturnBehaviour
  , authorIsSeller :: Boolean
  , assertions :: assertions
  , testName :: String
  , skip :: Boolean
  , only :: Boolean
  , shouldError :: Boolean
  }

type BasicBuyAssertGroup = PostBuyTestData -> Array (Contract () Unit)

buyTestConfig1 :: BuyTestConfig BasicBuyAssertGroup
buyTestConfig1 =
  { mintParams: mintParams1
  , expectedShares:
      { minMpGain: BigInt.fromInt $ 10 * 1000000
      , minSellerGain: BigInt.fromInt $ 90 * 1000000
      , minAuthorGain: BigInt.fromInt $ 90 * 1000000
      }
  , retBehaviour: ToMarketPlace
  , authorIsSeller: true
  , assertions: nftToMarketPlaceAssert
  , testName: "no low shares"
  , skip: false
  , only: false
  , shouldError: false
  }

buyTestConfig2 :: BuyTestConfig BasicBuyAssertGroup
buyTestConfig2 = buyTestConfig1
  { mintParams = mintParams2
  , expectedShares
      { minMpGain = BigInt.fromInt 0
      , minSellerGain = BigInt.fromInt $ 100 * 1000000
      , minAuthorGain = BigInt.fromInt $ 100 * 1000000
      }
  , testName = "low marketplace share"
  }

buyTestConfig3 :: BuyTestConfig BasicBuyAssertGroup
buyTestConfig3 = buyTestConfig1
  { mintParams = mintParams3
  , expectedShares
      { minMpGain = BigInt.fromInt $ 10 * 1000000
      , minSellerGain = BigInt.fromInt $ 90 * 1000000
      , minAuthorGain = BigInt.fromInt $ 90 * 1000000
      }
  , testName = "low author share"
  }

buyTestConfig4 :: BuyTestConfig BasicBuyAssertGroup
buyTestConfig4 = buyTestConfig2
  { mintParams = mintParams4, testName = "low author and marketplace shares" }

buyTestConfig5 :: BuyTestConfig BasicBuyAssertGroup
buyTestConfig5 = buyTestConfig1
  { mintParams = mintParams5
  , testName = "price too low for min ada requirement"
  , shouldError = true
  }

buyTestConfig6 :: BuyTestConfig BasicBuyAssertGroup
buyTestConfig6 = buyTestConfig1
  { mintParams = mintParams6
  , testName = "fractional shares (.5)"
  , expectedShares
      { minMpGain = BigInt.fromInt $ 5_000_000
      , minSellerGain = BigInt.fromInt $ 45_000_005
      , minAuthorGain = BigInt.fromInt $ 45_000_005
      }
  }

buyTestConfig7 :: BuyTestConfig BasicBuyAssertGroup
buyTestConfig7 = buyTestConfig6
  { mintParams = mintParams7
  , testName = "fractional shares (.1)"
  , expectedShares
      { minSellerGain = BigInt.fromInt $ 45_000_001
      , minAuthorGain = BigInt.fromInt $ 45_000_001
      }
  }

buyTestConfig8 :: BuyTestConfig BasicBuyAssertGroup
buyTestConfig8 = buyTestConfig6
  { mintParams = mintParams8
  , testName = "fractional shares (.9)"
  , expectedShares
      { minSellerGain = BigInt.fromInt $ 45_000_009
      , minAuthorGain = BigInt.fromInt $ 45_000_009
      }
  }

addNftToBuyerVariants
  :: Array (BuyTestConfig BasicBuyAssertGroup)
  -> Array (BuyTestConfig BasicBuyAssertGroup)
addNftToBuyerVariants = Array.uncons >>> case _ of
  Nothing -> []
  Just { head: conf, tail: confs } ->
    conf
      : conf
          { retBehaviour = ToCaller
          , assertions = nftToBuyerAssert
          , testName = conf.testName <> ", nft to buyer"
          }
      : addNftToBuyerVariants confs

authorNotSellerVariant
  :: BuyTestConfig BasicBuyAssertGroup
  -> (ExpectedShares -> ExpectedShares)
  -> BuyTestConfig BasicBuyAssertGroup
authorNotSellerVariant conf updateShares =
  conf
    { expectedShares = updateShares conf.expectedShares
    , authorIsSeller = false
    , testName = conf.testName <> ", author is not seller"
    }

suite :: TestPlanM Unit
suite =
  only $ group "Buy" do
    let
      tests =
        [ buyTestConfig5
        -- Specify rounding behaviour
        , buyTestConfig6
        , authorNotSellerVariant buyTestConfig6 _
            { minSellerGain = BigInt.fromInt $ 40_000_005
            , minAuthorGain = BigInt.fromInt $ 5_000_000
            }
        , buyTestConfig7
        , authorNotSellerVariant buyTestConfig7 _
            { minSellerGain = BigInt.fromInt $ 40_000_001
            , minAuthorGain = BigInt.fromInt $ 5_000_000
            }
        , buyTestConfig8
        , authorNotSellerVariant buyTestConfig8 _
            { minSellerGain = BigInt.fromInt $ 40_000_009
            , minAuthorGain = BigInt.fromInt $ 5_000_000
            }
        ] <>
          addNftToBuyerVariants
            [ buyTestConfig1
            , authorNotSellerVariant buyTestConfig1 _
                { minSellerGain = BigInt.fromInt $ 80 * 1000000
                , minAuthorGain = BigInt.fromInt $ 10 * 1000000
                }
            , buyTestConfig2
            , authorNotSellerVariant buyTestConfig2 _
                { minSellerGain = BigInt.fromInt $ 90 * 1000000
                , minAuthorGain = BigInt.fromInt $ 10 * 1000000
                }
            , buyTestConfig3
            , authorNotSellerVariant buyTestConfig3 _
                { minSellerGain = BigInt.fromInt $ 90 * 1000000
                , minAuthorGain = BigInt.fromInt $ 0
                }
            , buyTestConfig4
            , authorNotSellerVariant buyTestConfig4 _
                { minAuthorGain = BigInt.fromInt $ 0 }
            ]
    for_ tests mkBuyTest

mkBuyTest
  :: forall f
   . WrappingAssertion f () PostBuyTestData
  => BuyTestConfig f
  -> TestPlanM Unit
mkBuyTest
  conf@{ mintParams, expectedShares, retBehaviour, assertions, authorIsSeller } =
  (if conf.skip then skip else if conf.only then only else identity)
    $ test conf.testName
    $ (if conf.shouldError then expectError else identity)
    $ runBuyTest mintParams retBehaviour authorIsSeller
        (\b -> mkShareAssertions expectedShares b /\ assertions)

nftToMarketPlaceAssert :: PostBuyTestData -> Array (Contract () Unit)
nftToMarketPlaceAssert o@{ mpScriptAddr } =
  [ assertAddrHasNewAsset mpScriptAddr o
  , assertAddrLacksOldAsset mpScriptAddr o
  ]

nftToBuyerAssert :: PostBuyTestData -> Array (Contract () Unit)
nftToBuyerAssert o@{ buyerAddr, mpScriptAddr } =
  [ assertAddrHasNewAsset buyerAddr o, assertAddrLacksOldAsset mpScriptAddr o ]

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
  [ assertGainAtAddr' "Author" authorPayAddr minAuthorGain
  , assertGainAtAddr' "Seller" sellerPayAddr minSellerGain
  , buyerMarketplaceShareAssert e b
  ]

buyerMarketplaceShareAssert
  :: ExpectedShares -> BuyTestData -> ContractWrapAssertion () PostBuyTestData
buyerMarketplaceShareAssert
  { minMpGain }
  { buyerAddr
  , mpScriptAddr
  , mintParams: MintParams mintParams
  , sgNft
  , nftToBuyer
  }
  contract = do
  (TransactionOutput mpNftUtxo) <- liftedM "Could not find sgNft utxo"
    $ findUtxoWithNft sgNft mpScriptAddr
  let
    mpInit = valueToLovelace mpNftUtxo.amount
    price = Nat.toBigInt mintParams.price

    getBuyerExpectedLoss :: PostBuyTestData -> Contract () BigInt
    getBuyerExpectedLoss { txHash } = do
      (Transaction { body: TxBody { fee } }) <-
        liftedM "Could not fetch buy transaction" $ getTxByHash txHash
      let
        feeLovelace = getLovelace (toPlutusCoin fee)
        mpRemainder = mpInit - feeLovelace
      pure $ if nftToBuyer then price - mpRemainder else price + feeLovelace

    mpExp = if nftToBuyer then (minMpGain - mpInit) else minMpGain
  contract `wrapAndAssert`
    [ assertGainAtAddr' "Marketplace" mpScriptAddr mpExp
    , assertLossAtAddr "Buyer" buyerAddr getBuyerExpectedLoss
    ]

runBuyTest
  :: forall (f :: Type)
   . WrappingAssertion f () PostBuyTestData
  => MintParams
  -> ReturnBehaviour
  -> Boolean
  -> (BuyTestData -> f)
  -> Aff Unit
runBuyTest mintParams retBehaviour authorIsSeller getAssertions = do
  let
    distribution =
      ( withStakeKey privateStakeKey1
          [ BigInt.fromInt 1_000_000_000
          , BigInt.fromInt 2_000_000_000
          ]
      )
        /\
          ( withStakeKey privateStakeKey2
              [ BigInt.fromInt 1_000_000_000
              , BigInt.fromInt 2_000_000_000
              ]
          )
        /\
          ( withStakeKey privateStakeKey3
              [ BigInt.fromInt 1_000_000_000
              , BigInt.fromInt 2_000_000_000
              ]
          )
  runPlutipContract plutipConfig distribution \(author /\ seller /\ buyer) -> do
    authorPayAddr <- walletEnterpriseAddress "author" author
    sellerPayAddr <- walletEnterpriseAddress "seller" seller
    initialSgNft /\ initialNftData <- withKeyWallet author do
      cnft <- callMintCnft
      callMintSgNft cnft mintParams
    sgNft /\ nftData <-
      if authorIsSeller then pure $ initialSgNft /\ initialNftData
      else withKeyWallet seller do
        txHash /\ txData <- marketplaceBuy' ToMarketPlace initialNftData
        awaitTxConfirmed txHash
        pure $ txData.newAsset /\ modify (_ { nftId = txData.newNft })
          initialNftData
    withKeyWallet buyer do
      buyerAddr <- liftedM "Could not get buyer addr" getWalletAddress
      mpScriptAddr <- marketplaceValidatorAddr
      let
        buyTestData =
          { authorPayAddr
          , sellerPayAddr:
              if authorIsSeller then authorPayAddr else sellerPayAddr
          , buyerAddr
          , mpScriptAddr
          , mintParams
          , sgNft
          , nftToBuyer: case retBehaviour of
              ToCaller -> true
              _ -> false
          }
      void $ withAssertions (getAssertions buyTestData) do
        txHash /\ txData <- marketplaceBuy' retBehaviour nftData
        awaitTxConfirmed txHash
        pure $ merge buyTestData { txData, txHash }
