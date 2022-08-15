module Seabug.Contract.Buy (marketplaceBuy, mkBuyTxData) where

import Contract.Prelude

import Contract.Address (ownPaymentPubKeyHash)
import Contract.Monad (Contract, liftedM)
import Contract.Numeric.Natural (toBigInt)
import Contract.PlutusData
  ( Datum(Datum)
  , toData
  )
import Contract.TxConstraints
  ( TxConstraints
  , mustPayToScript
  , mustPayWithDatumToPubKey
  )
import Contract.Value as Value
import Data.BigInt (BigInt, fromInt)
import Plutus.Types.Transaction (UtxoM)
import Seabug.Contract.Util
  ( SeabugTxData
  , ReturnBehaviour(ToCaller)
  , minAdaOnlyUTxOValue
  , seabugTxToMarketTx
  , mkChangeNftIdTxData
  , modify
  )
import Seabug.Metadata.Share (maxShare)
import Seabug.Types
  ( MintAct(ChangeOwner)
  , NftData
  )

mkBuyTxData
  :: forall (r :: Row Type)
   . NftData
  -> Maybe UtxoM
  -> Contract r SeabugTxData
mkBuyTxData nftData mScriptUtxos = do
  pkh <- liftedM "buy: Cannot get PaymentPubKeyHash"
    ownPaymentPubKeyHash

  txData <- mkChangeNftIdTxData "buy" (flip ChangeOwner pkh)
    (modify $ _ { owner = pkh })
    nftData
    mScriptUtxos
  let
    nftData' = unwrap nftData
    nftCollection = unwrap nftData'.nftCollection
    nftId = unwrap nftData'.nftId
    nftPrice = nftId.price

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
    datum = Datum $ toData txData.oldAsset

    constraints =
      filterLowValue
        daoShare
        (mustPayToScript nftCollection.daoScript datum)
        <> filterLowValue
          authorShare
          (mustPayWithDatumToPubKey nftCollection.author datum)
        <> filterLowValue
          ownerShare
          (mustPayWithDatumToPubKey nftId.owner datum)
        <> txData.constraints

  pure $ txData { constraints = constraints }

marketplaceBuy :: forall (r :: Row Type). NftData -> Contract r Unit
marketplaceBuy = seabugTxToMarketTx "marketplaceBuy" ToCaller mkBuyTxData
