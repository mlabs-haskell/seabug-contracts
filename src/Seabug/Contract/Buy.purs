module Seabug.Contract.Buy (marketplaceBuy, mkBuyTxData) where

import Contract.Prelude

import Cardano.Types.Value (CurrencySymbol)
import Contract.Address (getNetworkId, ownPaymentPubKeyHash)
import Contract.Monad (Contract, liftContractE, liftContractM, liftedE, liftedM)
import Contract.Numeric.Natural (toBigInt)
import Contract.PlutusData
  ( Datum(Datum)
  , Redeemer(Redeemer)
  , toData
  , unitRedeemer
  )
import Contract.ScriptLookups
  ( ScriptLookups
  , UnattachedUnbalancedTx
  , mkUnbalancedTx
  )
import Contract.ScriptLookups
  ( mintingPolicy
  , validator
  , ownPaymentPubKeyHash
  , typedValidatorLookups
  , unspentOutputs
  ) as ScriptLookups
import Contract.Scripts (typedValidatorEnterpriseAddress)
import Contract.Transaction
  ( TransactionOutput(TransactionOutput)
  , balanceAndSignTxE
  , submit
  )
import Contract.TxConstraints
  ( TxConstraint
  , TxConstraints
  , mustMintValueWithRedeemer
  , mustPayToScript
  , mustPayWithDatumToPubKey
  , mustSpendScriptOutput
  )
import Contract.Utxos (utxosAt)
import Contract.Value (TokenName)
import Contract.Value as Value
import Contract.Wallet (getWalletAddress)
import Data.Array (find) as Array
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt, fromInt)
import Data.Map (insert, toUnfoldable)
import Plutus.Types.Transaction (UtxoM)
import Seabug.Contract.Util (minAdaOnlyUTxOValue, seabugTxToMarketTx)
import Seabug.MarketPlace (marketplaceValidator)
import Seabug.Metadata.Share (maxShare)
import Seabug.MintingPolicy (mkMintingPolicy, mkTokenName)
import Seabug.Types
  ( MarketplaceDatum(MarketplaceDatum)
  , MintAct(ChangeOwner)
  , NftData(..)
  , NftId(NftId)
  )
import Serialization.Types (PlutusData)
import Types.Transaction (TransactionInput)

mkBuyTxData
  :: forall (r :: Row Type)
   . NftData
  -> Maybe UtxoM
  -> Contract r SeabugTxData
mkBuyTxData nftData mScriptUtxos = do
  pkh <- liftedM "buy: Cannot get PaymentPubKeyHash"
    ownPaymentPubKeyHash

  txData <- mkChangeNftIdTxData "buy" (ChangeOwner _ pkh)
    (modify $ _ { owner = pkh })
    nftData
    mScriptUtxos
  let
    nftCollection = unwrap (unwrap nftData).nftCollection
    nftPrice = (unwrap txData.newNft).price

    getShare :: BigInt -> BigInt
    getShare share = (toBigInt nftPrice * share) `div` fromInt maxShare

    shareToSubtract :: BigInt -> BigInt
    shareToSubtract v
      | v < minAdaOnlyUTxOValue = zero
      | otherwise = v

    filterLowValue
      :: BigInt
      -> (Value.Value -> TxConstraints Unit Unit)
      -> TxConstraints Unit Unit
    filterLowValue v t
      | v < minAdaOnlyUTxOValue = mempty
      | otherwise = t (Value.lovelaceValueOf v)

    authorShare = getShare $ toBigInt nftCollection.authorShare
    daoShare = getShare $ toBigInt nftCollection.daoShare
    ownerShare = toBigInt nftPrice
      - shareToSubtract authorShare
      - shareToSubtract daoShare
    datum = Datum $ toData $ curr /\ oldName

    constraints =
      filterLowValue
        daoShare
        (mustPayToScript nftCollection.daoScript datum)
        <> filterLowValue
          authorShare
          (mustPayWithDatumToPubKey nftCollection.author datum)
        <> filterLowValue
          ownerShare
          (mustPayWithDatumToPubKey pkh datum)
        <> txData.constraints

  pure $ txData { constraints = constraints }

marketplaceBuy :: forall (r :: Row Type). NftData -> Contract r Unit
marketplaceBuy = seabugTxToMarketTx "marketplaceBuy" mkBuyTxData
