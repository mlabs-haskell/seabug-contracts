module Test.Contract.Buy (suite) where

import Contract.Prelude

import Contract.Address (Address, getWalletAddress)
import Contract.Monad (Contract, liftContractE, liftedM)
import Contract.Numeric.Natural as Nat
import Contract.PlutusData (Datum(..), toData)
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
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Mote (group, only, skip, test)
import Plutus.Conversion (toPlutusCoin)
import Record (merge)
import Seabug.Contract.Buy (marketplaceBuy')
import Seabug.Contract.Util
  ( ReturnBehaviour(..)
  , SeabugTxData
  , getSeabugMetadata
  , minAdaOnlyUTxOValue
  , modify
  )
import Seabug.MarketPlace (marketplaceValidatorAddr)
import Seabug.Types (MintParams(..), NftData)
import Test.Contract.Util
  ( class WrappingAssertion
  , ContractWrapAssertion
  , assertContract
  , assertGainAtAddr'
  , assertLossAtAddr
  , assertTxHasMetadata
  , callMintCnft
  , callMintSgNft
  , checkNftAtAddress
  , checkOutputHasDatum
  , findUtxo
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
  , preBuyNft :: NftData -- The data of the nft being bought
  | r
  }

type PostBuyTestData = BuyTestData'
  ( txData :: SeabugTxData -- The data of the buy transaction
  , txHash :: TransactionHash -- The hash of the buy transaction
  , postBuyNft :: NftData -- The data of the nft after it was bought
  )

type ExpectedShares =
  { mpGain :: BigInt
  , sellerGain :: BigInt
  , authorGain :: BigInt
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
      { mpGain: BigInt.fromInt $ 10 * 1000000
      , sellerGain: BigInt.fromInt $ 80 * 1000000
      , authorGain: BigInt.fromInt $ 10 * 1000000
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
      { mpGain = BigInt.fromInt 0
      , sellerGain = BigInt.fromInt $ 90 * 1000000
      , authorGain = BigInt.fromInt $ 10 * 1000000
      }
  , testName = "low marketplace share"
  }

buyTestConfig3 :: BuyTestConfig BasicBuyAssertGroup
buyTestConfig3 = buyTestConfig1
  { mintParams = mintParams3
  , expectedShares
      { mpGain = BigInt.fromInt $ 10 * 1000000
      , sellerGain = BigInt.fromInt $ 90 * 1000000
      , authorGain = BigInt.fromInt 0
      }
  , testName = "low author share"
  }

buyTestConfig4 :: BuyTestConfig BasicBuyAssertGroup
buyTestConfig4 = buyTestConfig2
  { mintParams = mintParams4
  , testName = "low author and marketplace shares"
  , expectedShares
      { mpGain = BigInt.fromInt 0
      , sellerGain = BigInt.fromInt $ 100 * 1000000
      , authorGain = BigInt.fromInt 0
      }
  }

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
      { mpGain = BigInt.fromInt 5_000_000
      , sellerGain = BigInt.fromInt 40_000_005
      , authorGain = BigInt.fromInt 5_000_000
      }
  }

buyTestConfig7 :: BuyTestConfig BasicBuyAssertGroup
buyTestConfig7 = buyTestConfig6
  { mintParams = mintParams7
  , testName = "fractional shares (.1)"
  , expectedShares
      { sellerGain = BigInt.fromInt 40_000_001
      , authorGain = BigInt.fromInt 5_000_000
      }
  }

buyTestConfig8 :: BuyTestConfig BasicBuyAssertGroup
buyTestConfig8 = buyTestConfig6
  { mintParams = mintParams8
  , testName = "fractional shares (.9)"
  , expectedShares
      { sellerGain = BigInt.fromInt 40_000_009
      , authorGain = BigInt.fromInt 5_000_000
      }
  }

addVariants
  :: (BuyTestConfig BasicBuyAssertGroup -> BuyTestConfig BasicBuyAssertGroup)
  -> Array (BuyTestConfig BasicBuyAssertGroup)
  -> Array (BuyTestConfig BasicBuyAssertGroup)
addVariants vary = Array.foldMap (\x -> [ x, vary x ])

addNftToBuyerVariants
  :: Array (BuyTestConfig BasicBuyAssertGroup)
  -> Array (BuyTestConfig BasicBuyAssertGroup)
addNftToBuyerVariants = addVariants \conf ->
  conf
    { retBehaviour = ToCaller
    , assertions = nftToBuyerAssert
    , testName = conf.testName <> ", nft to buyer"
    }

addAuthorNotSellerVariants
  :: Array (BuyTestConfig BasicBuyAssertGroup)
  -> Array (BuyTestConfig BasicBuyAssertGroup)
addAuthorNotSellerVariants = addVariants \conf ->
  conf
    { authorIsSeller = false
    , testName = conf.testName <> ", author is not seller"
    }

suite :: TestPlanM Unit
suite =
  group "Buy" do
    let
      tests =
        [ buyTestConfig5 ]
          <> addAuthorNotSellerVariants
            [
              -- Specify rounding behaviour
              buyTestConfig6
            , buyTestConfig7
            , buyTestConfig8
            ]
          <>
            (addNftToBuyerVariants <<< addAuthorNotSellerVariants)
              [ buyTestConfig1
              , buyTestConfig2
              , buyTestConfig3
              , buyTestConfig4
              ]
    for_ tests mkBuyTest

mkBuyTest
  :: forall f
   . WrappingAssertion f () PostBuyTestData
  => BuyTestConfig f
  -> TestPlanM Unit
mkBuyTest
  conf@{ mintParams, retBehaviour, assertions, authorIsSeller } =
  (if conf.skip then skip else if conf.only then only else identity)
    $ test conf.testName
    $ (if conf.shouldError then expectError else identity)
    $ runBuyTest mintParams retBehaviour authorIsSeller
        \b -> mkUtxoAssertions conf b /\ assertions /\ buyTxMetadataAssert

-- | Assert that the buy tx metadata is correct
buyTxMetadataAssert :: PostBuyTestData -> Contract () Unit
buyTxMetadataAssert { txHash, txData: { newAsset }, postBuyNft } = do
  expectedSeabugMetadata <- liftContractE $
    getSeabugMetadata postBuyNft (fst newAsset)
  assertTxHasMetadata "Buy" txHash expectedSeabugMetadata

-- | Invariants for when the nft is sent to the marketplace after a
-- | buy
nftToMarketPlaceAssert :: PostBuyTestData -> Array (Contract () Unit)
nftToMarketPlaceAssert o@{ mpScriptAddr } =
  [ assertAddrHasNewAsset mpScriptAddr o
  , assertAddrLacksOldAsset mpScriptAddr o
  ]

-- | Invariants for when the nft is sent to the buyer after a buy
nftToBuyerAssert :: PostBuyTestData -> Array (Contract () Unit)
nftToBuyerAssert o@{ buyerAddr, mpScriptAddr } =
  [ assertAddrHasNewAsset buyerAddr o, assertAddrLacksOldAsset mpScriptAddr o ]

-- | Assert that the address holds the post-buy updated nft
assertAddrHasNewAsset :: Address -> PostBuyTestData -> Contract () Unit
assertAddrHasNewAsset addr { txData } =
  assertContract "Address did not contain new sgNft"
    =<< checkNftAtAddress txData.newAsset addr

-- | Assert that the address does not hold the pre-buy nft
assertAddrLacksOldAsset :: Address -> PostBuyTestData -> Contract () Unit
assertAddrLacksOldAsset addr { txData } =
  assertContract "Address contained old sgNft"
    =<< not
    <$> checkNftAtAddress txData.oldAsset addr

-- | Build assertions for the invariants surrounding the utxos of the
-- | buy transaction, including: correct distribution of
-- | royalties/shares, and correct payment utxos with datums
mkUtxoAssertions
  :: forall f
   . WrappingAssertion f () PostBuyTestData
  => BuyTestConfig f
  -> BuyTestData
  -> ContractWrapAssertion () PostBuyTestData
mkUtxoAssertions
  { authorIsSeller, expectedShares: e@{ sellerGain, authorGain } }
  b@{ sellerPayAddr, authorPayAddr } =
  let
    assertSellerChange = assertGainAtAddr' "Seller" sellerPayAddr
      $ if authorIsSeller then sellerGain + authorGain else sellerGain
    assertAuthorChange =
      if authorIsSeller then identity
      else assertGainAtAddr' "Author" authorPayAddr authorGain
    assertSellerPayment = assertPaymentUtxo "Seller" sellerPayAddr sellerGain
    assertAuthorPayment =
      if authorIsSeller then const (pure unit)
      else assertPaymentUtxo "Author" authorPayAddr authorGain
  in
    withAssertions
      $
        [ assertSellerChange
        , assertAuthorChange
        , buyerMarketplaceUtxoAssert e b
        ]
      /\
        [ assertAuthorPayment
        , assertSellerPayment
        ]

-- | Makes a check for the invariants of the buyer's and the
-- | marketplace's utxos surrounding a buy. This is separated into its
-- | own function to handle the special case of the buyer not paying
-- | the full price, described here:
-- | https://github.com/mlabs-haskell/seabug-contracts/pull/41#issue-1322730466
buyerMarketplaceUtxoAssert
  :: ExpectedShares -> BuyTestData -> ContractWrapAssertion () PostBuyTestData
buyerMarketplaceUtxoAssert
  { mpGain }
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

    mpExp = if nftToBuyer then (mpGain - mpInit) else mpGain
  wrapAndAssert contract
    $
      [ assertGainAtAddr' "Marketplace" mpScriptAddr mpExp
      , assertLossAtAddr "Buyer" buyerAddr getBuyerExpectedLoss
      ]
    /\
      [ assertPaymentUtxo "Marketplace" mpScriptAddr mpGain
      ]

-- | Assert that the given address has a utxo with the given lovelace
-- | amount and the correct payment datum.
assertPaymentUtxo
  :: String -> Address -> BigInt -> PostBuyTestData -> Contract () Unit
assertPaymentUtxo name addr payment { txData: { oldAsset } }
  | payment < minAdaOnlyUTxOValue = pure unit
  | otherwise =
      assertContract (name <> " did not have payment utxo with datum")
        =<< isJust
        <$> findUtxo addr \o@(TransactionOutput utxo) ->
          checkOutputHasDatum name (Datum $ toData oldAsset) (==) o <#>
            (_ && valueToLovelace utxo.amount == payment)

-- | With three actors: Author, Seller, and Buyer, first have Author
-- | mint a token, second optionally have Seller buy that token, and
-- | third have Buyer buy the token. The third step is wrapped in the
-- | given assertions.
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
    { sgNft: initialSgNft, nftData: initialNftData } <- withKeyWallet author do
      cnft <- callMintCnft pure
      callMintSgNft cnft mintParams pure
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
          , preBuyNft: nftData
          }
      void $ withAssertions (getAssertions buyTestData) do
        txHash /\ txData <- marketplaceBuy' retBehaviour nftData
        awaitTxConfirmed txHash
        pure $ merge buyTestData
          { txData
          , txHash
          , postBuyNft: modify (_ { nftId = txData.newNft }) nftData
          }
