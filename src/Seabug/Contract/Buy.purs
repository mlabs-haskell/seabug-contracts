module Seabug.Contract.Buy
  ( marketplaceBuy
  , marketplaceBuy'
  , mkBuyTxData
  ) where

import Contract.Prelude

import Contract.Address (ownPaymentPubKeyHash)
import Contract.Monad (Contract, liftedM, throwContractError)
import Contract.Numeric.Natural (toBigInt)
import Contract.PlutusData (Datum(Datum), toData)
import Contract.Transaction (TransactionHash)
import Contract.TxConstraints
  ( TxConstraints
  , mustPayToScript
  , mustPayWithDatumToPubKey
  )
import Contract.Value as Value
import Data.BigInt (BigInt, fromInt)
import Plutus.Types.Transaction (UtxoM)
import Seabug.Contract.Util
  ( ReturnBehaviour(..)
  , SeabugTxData
  , minUTxOValue
  , mkChangeNftIdTxData
  , modify
  , seabugTxToMarketTx
  )
import Seabug.Metadata.Share (maxShare)
import Seabug.Types (MintAct(ChangeOwner), NftData)

mkBuyTxData
  :: forall (r :: Row Type)
   . NftData
  -> Maybe UtxoM
  -> Contract r SeabugTxData
mkBuyTxData nftData mScriptUtxos = do
  pkh <- liftedM "buy: Cannot get PaymentPubKeyHash"
    ownPaymentPubKeyHash

  let
    nftData' = unwrap nftData
    nftCollection = unwrap nftData'.nftCollection
    nftId = unwrap nftData'.nftId
    nftPrice = nftId.price

  when (nftId.owner == pkh) $ throwContractError "NFT is already owned by buyer"

  txData <- mkChangeNftIdTxData "buy" (flip ChangeOwner pkh)
    (modify $ _ { owner = pkh })
    nftData
    mScriptUtxos
  let
    getShare :: BigInt -> BigInt
    getShare share = (toBigInt nftPrice * share) `div` fromInt maxShare

    shareToSubtract :: BigInt -> BigInt
    shareToSubtract v
      | v < minUTxOValue = zero
      | otherwise = v

    filterLowValue
      :: BigInt
      -> (Value.Value -> TxConstraints Void Void)
      -> TxConstraints Void Void
    filterLowValue v t
      | v < minUTxOValue = mempty
      | otherwise = t (Value.lovelaceValueOf v)

    authorShare = getShare $ toBigInt nftCollection.authorShare
    daoShare = getShare $ toBigInt nftCollection.daoShare
    ownerShare = toBigInt nftPrice
      - shareToSubtract authorShare
      - shareToSubtract daoShare
    datum = Datum $ toData txData.oldAsset

    constraints :: TxConstraints Void Void
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
marketplaceBuy = void <<< marketplaceBuy' ToCaller

marketplaceBuy'
  :: forall (r :: Row Type)
   . ReturnBehaviour
  -> NftData
  -> Contract r (TransactionHash /\ SeabugTxData)
marketplaceBuy' retBehaviour = seabugTxToMarketTx "marketplaceBuy" retBehaviour
  mkBuyTxData
