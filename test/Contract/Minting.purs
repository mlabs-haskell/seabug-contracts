module Test.Contract.Minting (suite) where

import Contract.Prelude

import Contract.Address
  ( Slot(..)
  , getNetworkId
  , getWalletAddress
  , validatorHashEnterpriseAddress
  )
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.Numeric.Natural as Nat
import Contract.PlutusData (fromData, getDatumByHash)
import Contract.Scripts (Validator, validatorHash)
import Contract.Test.Plutip (runPlutipContract, withKeyWallet, withStakeKey)
import Contract.Transaction (TransactionOutput(..), awaitTxConfirmed)
import Contract.Value (CurrencySymbol, TokenName)
import Data.BigInt as BigInt
import Mote (test)
import Seabug.Contract.CnftMint (mintCnft)
import Seabug.Contract.Mint (mintWithCollection')
import Seabug.Lock (mkLockScript)
import Seabug.MarketPlace (marketplaceValidatorAddr)
import Seabug.Types (MarketplaceDatum(..), MintCnftParams(..))
import Test.Contract.Util
  ( assertContract
  , checkNftAtAddress
  , findUtxoWithNft
  , plutipConfig
  , privateStakeKey
  )
import TestM (TestPlanM)
import Types.BigNum as BigNum

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

        sgNft /\ lockScript <- callMintSgNft cnft

        scriptAddr <- marketplaceValidatorAddr
        TransactionOutput sgNftUtxo <-
          liftedM "Could not find sgNft at marketplace address" $
            findUtxoWithNft sgNft scriptAddr

        lockScriptAddr <- liftedM "Could not get locking script addr"
          $ validatorHashEnterpriseAddress
          <$> getNetworkId
          <*>
            ( liftedM "Could not get locking script hash" $ liftAff $
                validatorHash lockScript
            )
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

callMintCnft
  ∷ forall (r :: Row Type). Contract r (CurrencySymbol /\ TokenName)
callMintCnft = do
  log "Minting cnft..."
  txHash /\ cnft <- mintCnft $
    MintCnftParams
      { imageUri:
          "ipfs://k2cwuebwvb6kdiwob6sb2yqnz38r0yv72q1xijbts9ep5lq3nm8rw3i4"
      , tokenNameString: "abcdef"
      , name: "Piaggio Ape"
      , description: "Seabug Testing"
      }
  log $ "Waiting for confirmation of cnft transaction: " <> show txHash
  awaitTxConfirmed txHash
  log $ "Cnft transaction confirmed: " <> show txHash
  log $ "Minted cnft: " <> show cnft
  pure cnft

callMintSgNft
  :: forall (r :: Row Type)
   . Tuple CurrencySymbol TokenName
  -> Contract r ((CurrencySymbol /\ TokenName) /\ Validator)
callMintSgNft cnft = do
  let
    lockLockup = BigInt.fromInt 5
    lockLockupEnd = Slot $ BigNum.fromInt 5
  log "Minting sgNft..."
  sgNftTxHash /\ sgNft <- mintWithCollection' cnft
    $ wrap
        { authorShare: Nat.fromInt' 1000
        , daoShare: Nat.fromInt' 1000
        , price: Nat.fromInt' $ 100 * 1000000
        , lockLockup
        , lockLockupEnd
        , feeVaultKeys: []
        }
  log $ "Waiting for confirmation of nft transaction: " <> show
    sgNftTxHash
  awaitTxConfirmed sgNftTxHash
  log $ "Nft transaction confirmed: " <> show sgNftTxHash
  lockScript <- mkLockScript (fst cnft) lockLockup lockLockupEnd
  pure $ sgNft /\ lockScript
