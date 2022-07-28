-- | Helper to list the utxo with relevant NFT at the market validator script
module Seabug.Contract.MarketPlaceListNft
  ( marketPlaceListNft
  ) where

import Contract.Prelude

import Contract.Address (getNetworkId, typedValidatorEnterpriseAddress)
import Contract.Monad
  ( Contract
  , asksConfig
  , liftContractE
  , liftContractM
  , liftedM
  )
import Contract.Numeric.Natural as Natural
import Contract.PlutusData (fromData, getDatumsByHashes)
import Contract.Transaction (TransactionOutput(TransactionOutput))
import Contract.Utxos (utxosAt)
import Contract.Value (valueOf)
import Control.Alternative (guard)
import Control.Monad.Maybe.Trans (MaybeT(MaybeT), runMaybeT)
import Control.Parallel (parTraverse)
import Data.Array (catMaybes, mapMaybe)
import Data.Map as Map
import Seabug.Contract.Common (NftResult)
import Seabug.Contract.Util (minAdaOnlyUTxOValue)
import Seabug.MarketPlace (marketplaceValidator)
import Seabug.Metadata (getFullSeabugMetadataWithBackoff)
import Seabug.Types (MarketplaceDatum(MarketplaceDatum))

-- | Lists the utxos at the script address that contain a datum of type
-- | `MarketplaceDatum` with unit value. It currently doesn't have any logic
-- | on matching `CurrencySymbol` and `TokenName`.
marketPlaceListNft
  :: forall (r :: Row Type)
   . Contract (projectId :: String | r) (Array NftResult)
marketPlaceListNft = do
  marketplaceValidator' <- unwrap <$> liftContractE marketplaceValidator
  networkId <- getNetworkId
  projectId <- asksConfig $ _.projectId
  scriptAddr <-
    liftContractM "marketPlaceListNft: Cannot convert validator hash to address"
      $ typedValidatorEnterpriseAddress networkId
      $ wrap marketplaceValidator'
  scriptUtxos <- Map.toUnfoldable <<< unwrap <$>
    liftedM "marketPlaceListNft: Cannot get script Utxos"
      (utxosAt scriptAddr)
  datums <- getDatumsByHashes
    $ mapMaybe (snd >>> unwrap >>> _.dataHash) scriptUtxos
  withMetadata <- liftAff $ (flip parTraverse) scriptUtxos $
    \(input /\ output@(TransactionOutput out)) ->
      runMaybeT $ do
        MarketplaceDatum { getMarketplaceDatum: curr /\ name } <-
          MaybeT $ pure $ (fromData <<< unwrap)
            =<< (_ `Map.lookup` datums)
            =<< out.dataHash
        guard $ valueOf out.amount curr name == one
        metadata <- MaybeT $ map hush $
          getFullSeabugMetadataWithBackoff (curr /\ name) projectId
        -- TODO: this is a temporary solution to only show NFTs known
        -- to work. This filter catches a couple new nfts whose price
        -- I put too low. The old nfts are caught above because their
        -- metadata won't be parsed.
        guard $ (unwrap metadata.seabugMetadata # _.ownerPrice) >=
          (Natural.fromBigInt' minAdaOnlyUTxOValue)
        pure { input, output, metadata }
  pure $ catMaybes withMetadata
