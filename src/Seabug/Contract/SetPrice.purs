module Seabug.Contract.SetPrice where

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
import Seabug.Contract.Util (minAdaOnlyUTxOValue, setSeabugMetadata)
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

mkSetPriceTxData
  :: forall (r :: Row Type)
   . Natural
  -> NftData
  -> Maybe UtxoM
  -> Contract r SeabugTxData
mkSetPriceTxData newPrice =
  mkChangeNftIdTxData "setPrice" (ChangePrice _ newPrice) $ modify $ _
    { price = newPrice }

marketplaceSetPrice :: forall (r :: Row Type). NftData -> Contract r Unit
marketplaceSetPrice = seabugTxToMarketTx "marketplaceSetPrice" mkSetPriceTxData
