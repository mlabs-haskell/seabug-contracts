module Seabug.Contract.SetPrice
  ( marketplaceSetPrice
  , marketplaceSetPrice'
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Numeric.Natural (Natural)
import Contract.Transaction (TransactionHash)
import Plutus.Types.Transaction (UtxoM)
import Seabug.Contract.Util
  ( SeabugTxData
  , ReturnBehaviour(ToMarketPlace)
  , mkChangeNftIdTxData
  , modify
  , seabugTxToMarketTx
  )
import Seabug.Types (MintAct(ChangePrice), NftData)

mkSetPriceTxData
  :: forall (r :: Row Type)
   . Natural
  -> NftData
  -> Maybe UtxoM
  -> Contract r SeabugTxData
mkSetPriceTxData newPrice =
  mkChangeNftIdTxData "setPrice" (flip ChangePrice newPrice)
    $ modify _ { price = newPrice }

marketplaceSetPrice
  :: forall (r :: Row Type). Natural -> NftData -> Contract r Unit
marketplaceSetPrice price nft =
  void $ marketplaceSetPrice' price nft

marketplaceSetPrice'
  :: forall (r :: Row Type)
   . Natural
  -> NftData
  -> Contract r (TransactionHash /\ SeabugTxData)
marketplaceSetPrice' =
  seabugTxToMarketTx "marketplaceSetPrice" ToMarketPlace <<< mkSetPriceTxData
