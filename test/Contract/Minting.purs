module Test.Contract.Minting (suite) where

import Contract.Prelude

import Contract.Address (getWalletAddress, scriptHashAddress)
import Contract.Chain (currentSlot)
import Contract.Log (logError')
import Contract.Monad (Contract, liftContractE, liftedM)
import Contract.Scripts (validatorHash)
import Contract.Test.Plutip (runPlutipContract, withKeyWallet, withStakeKey)
import Contract.TxConstraints (TxConstraint(..), TxConstraints(..))
import Contract.Utxos (utxosAt)
import Contract.Value (lovelaceValueOf)
import Contract.Wallet (KeyWallet)
import Data.BigInt as BigInt
import Data.FoldableWithIndex (findWithIndex)
import Mote (group, test)
import Seabug.Contract.Util (getSeabugMetadata, minAdaOnlyUTxOValue, modify)
import Seabug.Lock (mkLockScript)
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
import Test.Spec.Assertions (expectError)
import TestM (TestPlanM)

suite :: TestPlanM Unit
suite =
  group "Minting" do
    test "Cnft mint fail: expect \"UTxO not consumed\" trace" $ expectError $
      withMinter \_ -> do
        addr <- liftedM "Could not get addr" getWalletAddress
        utxos <- liftedM "Cannot get user utxos" $ map unwrap <$> utxosAt addr
        callMintCnft \txc@(TxConstraints { constraints }) -> do
          constraints' <- for constraints $
            case _ of
              MustSpendPubKeyOutput oref -> do
                oref' <- findWithIndex (\i _ -> i /= oref) utxos # case _ of
                  Nothing -> do
                    logError'
                      "Could not find a utxo different from the constraint"
                    pure oref
                  Just { index: oref' } -> pure oref'
                pure $ MustSpendPubKeyOutput oref'
              x -> pure x
          pure $ modify (_ { constraints = constraints' }) txc

    group "SgNft mint fail" do
      test "expect \"Exactly one NFT must be minted\" trace" $
        expectError do
          withMinter \_ -> do
            cnft <- callMintCnft pure
            callMintSgNft cnft mintParams1
              \txc@(TxConstraints { constraints }) -> do
                constraints' <- for constraints $
                  case _ of
                    (MustMintValue h r t i)
                      | i == one -> pure $
                          MustMintValue h r t (BigInt.fromInt 2)
                    x -> pure x
                pure $ modify (_ { constraints = constraints' }) txc
      test "expect \"Underlying NFT must be locked\" trace" $
        expectError do
          withMinter \_ -> do
            let
              mp = mintParams1
              mp' = unwrap mp
            cnft <- callMintCnft pure
            lockingScript <- mkLockScript (fst cnft) mp'.lockLockup
              mp'.lockLockupEnd
            lockingScriptHash <- liftedM "Could not get locking script hash"
              $ liftAff
              $ validatorHash lockingScript
            callMintSgNft cnft mp
              \txc@(TxConstraints { constraints }) -> do
                constraints' <- for constraints $
                  case _ of
                    -- Modify the constraint sending the underlying
                    -- nft to the locking script to not send it
                    (MustPayToScript h d _)
                      | h == lockingScriptHash -> pure $
                          MustPayToScript h d
                            (lovelaceValueOf minAdaOnlyUTxOValue)
                    x -> pure x
                pure $ modify (_ { constraints = constraints' }) txc

    test "Successful mint" do
      withMinter \_ -> do
        cnft <- callMintCnft pure
        aliceAddr <- liftedM "Could not get addr" getWalletAddress
        assertContract "Could not find cnft at user address" =<<
          checkNftAtAddress cnft aliceAddr

        expectedEntered <- currentSlot
        { sgNft, nftData, txHash } <- callMintSgNft cnft mintParams1 pure

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

withMinter
  :: forall (a :: Type). (KeyWallet -> Contract () a) -> Aff a
withMinter f =
  runPlutipContract plutipConfig
    ( withStakeKey privateStakeKey1
        [ BigInt.fromInt 1_000_000_000
        , BigInt.fromInt 2_000_000_000
        ]
    )
    \minter -> withKeyWallet minter (f minter)
