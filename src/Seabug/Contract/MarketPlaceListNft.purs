-- | Helper to list the utxo with relevant NFT at the market validator script
module Seabug.Contract.MarketPlaceListNft
  ( ListNftResult
  , marketPlaceListNft
  ) where

import Contract.Prelude

import Contract.Address (getNetworkId, typedValidatorEnterpriseAddress)
import Contract.Monad (Contract, liftContractE, liftedM)
import Contract.PlutusData (fromData, getDatumsByHashes)
import Contract.Transaction
  ( TransactionInput
  , TransactionOutput(TransactionOutput)
  )
import Contract.Utxos (utxosAt)
import Contract.Value (valueOf)
import Control.Alternative (guard)
import Control.Monad.Maybe.Trans (MaybeT(MaybeT), runMaybeT)
import Control.Monad.Reader (asks)
import Control.Parallel (parTraverse)
import Data.Array (catMaybes, mapMaybe)
import Data.Map as Map
import Seabug.MarketPlace (marketplaceValidator)
import Seabug.Metadata (FullSeabugMetadata, getFullSeabugMetadata)
import Seabug.Types (MarketplaceDatum(MarketplaceDatum))

type ListNftResult =
  { input :: TransactionInput
  , output :: TransactionOutput
  , metadata :: FullSeabugMetadata
  }

-- | Lists the utxos at the script address that contain a datum of type
-- | `MarketplaceDatum` with unit value. It currently doesn't have any logic
-- | on matching `CurrencySymbol` and `TokenName`.
marketPlaceListNft
  :: forall (r :: Row Type)
   . Contract (projectId :: String | r) (Array ListNftResult)
marketPlaceListNft = do
  marketplaceValidator' <- unwrap <$> liftContractE marketplaceValidator
  networkId <- getNetworkId
  projectId <- asks $ unwrap >>> _.projectId
  scriptAddr <-
    liftedM "marketPlaceListNft: Cannot convert validator hash to address"
      $ pure
      $ typedValidatorEnterpriseAddress networkId
      $ wrap marketplaceValidator'
  scriptUtxos <- Map.toUnfoldable <<< unwrap <$>
    liftedM "marketPlaceListNft: Cannot get script Utxos"
      (utxosAt (unwrap scriptAddr).address)
  datums <- getDatumsByHashes
    $ mapMaybe (snd >>> unwrap >>> _.dataHash) scriptUtxos
  withMetadata <- liftAff $ (flip parTraverse) scriptUtxos $
    \(input /\ output@(TransactionOutput out)) ->
      runMaybeT $ do
        datumHash <- MaybeT $ pure $ out.dataHash
        plutusData <- MaybeT $ pure $ Map.lookup datumHash datums
        MarketplaceDatum { getMarketplaceDatum: curr /\ name } <-
          MaybeT $ pure $ fromData $ unwrap plutusData
        guard $ valueOf out.amount curr name == one
        metadata <- MaybeT $ map hush $
          getFullSeabugMetadata (curr /\ name) projectId
        pure { input, output, metadata }
  pure $ catMaybes withMetadata
