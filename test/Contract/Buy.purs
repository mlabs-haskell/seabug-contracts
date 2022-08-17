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
import Contract.Transaction (awaitTxConfirmed)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Mote (group, only, skip, test)
import Seabug.Contract.Buy (marketplaceBuy')
import Seabug.Contract.Util
  ( ReturnBehaviour(..)
  , SeabugTxData
  , minAdaOnlyUTxOValue
  )
import Seabug.MarketPlace (marketplaceValidatorAddr)
import Seabug.Types (MintParams)
import Test.Contract.Util
  ( ContractWrapAssertion
  , assertContract
  , assertLovelaceDecAtAddr
  , assertLovelaceIncAtAddr
  , callMintCnft
  , callMintSgNft
  , checkNftAtAddress
  , mintParams1
  , mintParams2
  , mintParams3
  , mintParams4
  , plutipConfig
  , privateStakeKey1
  , privateStakeKey2
  , withAssertions
  )
import TestM (TestPlanM)

type BuyTestData =
  { sellerPayAddr :: Address -- The enterprise address of the seller
  , buyerAddr :: Address -- The address used by the buyer
  , authorPayAddr :: Address -- The enterprise address of the author
  , mpScriptAddr :: Address -- The address of the marketplace script
  , mintParams :: MintParams -- The params used to mint the bought nft
  }

type PostBuyTestData =
  { buyTestData :: BuyTestData
  , txData :: SeabugTxData -- The data of the buy transaction
  }

type ExpectedShares =
  { minMpGain :: BigInt
  , minSellerGain :: BigInt
  , minAuthorGain :: BigInt
  }

type BuyTestConfig =
  { mintParams :: MintParams
  , expectedShares :: ExpectedShares
  , retBehaviour :: ReturnBehaviour
  , postBuyAssertions :: PostBuyTestData -> Array (Contract () Unit)
  }

buyTestConfig1 :: BuyTestConfig
buyTestConfig1 =
  { mintParams: mintParams1
  , expectedShares:
      { minMpGain: BigInt.fromInt $ 10 * 1000000
      , minSellerGain: BigInt.fromInt $ 90 * 1000000
      , minAuthorGain: BigInt.fromInt $ 90 * 1000000
      }
  , retBehaviour: ToMarketPlace
  , postBuyAssertions: nftToMarketPlaceAssert
  }

buyTestConfig2 :: BuyTestConfig
buyTestConfig2 =
  { mintParams: mintParams2
  , expectedShares:
      { minMpGain: BigInt.fromInt 0
      , minSellerGain: BigInt.fromInt $ 100 * 1000000
      , minAuthorGain: BigInt.fromInt $ 100 * 1000000
      }
  , retBehaviour: ToMarketPlace
  , postBuyAssertions: nftToMarketPlaceAssert
  }

buyTestConfig3 :: BuyTestConfig
buyTestConfig3 =
  { mintParams: mintParams3
  , expectedShares:
      { minMpGain: BigInt.fromInt $ 10 * 1000000
      , minSellerGain: BigInt.fromInt $ 90 * 1000000
      , minAuthorGain: BigInt.fromInt $ 90 * 1000000
      }
  , retBehaviour: ToMarketPlace
  , postBuyAssertions: nftToMarketPlaceAssert
  }

buyTestConfig4 :: BuyTestConfig
buyTestConfig4 = buyTestConfig2 { mintParams = mintParams4 }

suite :: TestPlanM Unit
suite =
  only $ group "Buy" do
    test "Seller is author, no low prices, nft to marketplace" $
      mkBuyTest buyTestConfig1
    test "Seller is author, no low prices, nft to buyer" $
      mkBuyTest buyTestConfig1
        { retBehaviour = ToCaller
        , postBuyAssertions = nftToBuyerAssert
        }
    test "Seller is author, low marketplace share, nft to marketplace" $
      mkBuyTest buyTestConfig2
    test "Seller is author, low marketplace share, nft to buyer" $
      mkBuyTest buyTestConfig2
        { retBehaviour = ToCaller
        , postBuyAssertions = nftToBuyerAssert
        }
    test "Seller is author, low author share, nft to marketplace" $
      mkBuyTest buyTestConfig3
    test
      "Seller is author, low author and marketplace shares, nft to marketplace"
      $
        mkBuyTest buyTestConfig4

nftToMarketPlaceAssert :: PostBuyTestData -> Array (Contract () Unit)
nftToMarketPlaceAssert o@{ buyTestData: { mpScriptAddr } } =
  [ assertAddrHasNewAsset mpScriptAddr o
  , assertAddrLacksOldAsset mpScriptAddr o
  ]

nftToBuyerAssert :: PostBuyTestData -> Array (Contract () Unit)
nftToBuyerAssert o@{ buyTestData: { buyerAddr, mpScriptAddr } } =
  [ assertAddrHasNewAsset buyerAddr o, assertAddrLacksOldAsset mpScriptAddr o ]

mkBuyTest :: BuyTestConfig -> Aff Unit
mkBuyTest { mintParams, expectedShares, retBehaviour, postBuyAssertions } =
  runBuyTest mintParams retBehaviour (mkShareAssertions expectedShares)
    postBuyAssertions

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
  :: forall (r :: Row Type)
   . ExpectedShares
  -> BuyTestData
  -> Array (ContractWrapAssertion r)
mkShareAssertions
  { minMpGain, minSellerGain, minAuthorGain }
  { sellerPayAddr, buyerAddr, authorPayAddr, mpScriptAddr, mintParams } =
  let
    minBuyerLoss = Nat.toBigInt (unwrap mintParams).price
  in
    [ assertLovelaceIncAtAddr "Author" authorPayAddr minAuthorGain
    , assertLovelaceIncAtAddr "Seller" sellerPayAddr minSellerGain
    , assertLovelaceDecAtAddr "Buyer" buyerAddr minBuyerLoss
    , assertLovelaceIncAtAddr "Marketplace" mpScriptAddr minMpGain
    ]

runBuyTest
  :: forall (r :: Row Type)
   . MintParams
  -> ReturnBehaviour
  -> (BuyTestData -> Array (ContractWrapAssertion ()))
  -> (PostBuyTestData -> Array (Contract () Unit))
  -> Aff Unit
runBuyTest mintParams retBehaviour getAssertions getAfterAssertions = do
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
    _ /\ nftData <- withKeyWallet seller do
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
          }
      withAssertions (getAssertions buyTestData) do
        txHash /\ txData <- marketplaceBuy' retBehaviour nftData
        awaitTxConfirmed txHash
        sequence_ (getAfterAssertions { buyTestData, txData })

      pure unit
