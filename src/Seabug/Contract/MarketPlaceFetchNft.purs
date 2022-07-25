-- | Contract to fetch a single NFT
module Seabug.Contract.MarketPlaceFetchNft
  ( marketPlaceFetchNft
  ) where

import Contract.Prelude

import Contract.Monad (Contract, liftContractM, liftedE, liftedM, logWarn')
import Contract.PlutusData (fromData, getDatumByHash)
import Contract.Transaction (TransactionInput, TransactionOutput(..))
import Contract.Utxos (getUtxo)
import Control.Monad.Reader (asks)
import Seabug.Contract.Common (NftResult)
import Seabug.Metadata (getFullSeabugMetadataWithBackoff)
import Seabug.Types (MarketplaceDatum(..))

-- | Fetch the info for a single NFT identified by a utxo
-- | (`TransactionInput`). Returns `Nothing` if the given transaction
-- | input has been spent (for example if the NFT has been bought).
marketPlaceFetchNft
  :: forall (r :: Row Type)
   . TransactionInput
  -> Contract (projectId :: String | r) (Maybe NftResult)
marketPlaceFetchNft ref = do
  getUtxo ref >>= case _ of
    Nothing -> do
      logWarn' "Could not find NFT utxo, it may have been spent"
      pure Nothing
    Just output@(TransactionOutput nftTxOut) -> do
      datumHash <- liftContractM "Datum hash not available for NFT"
        nftTxOut.dataHash
      MarketplaceDatum { getMarketplaceDatum: datum } <-
        liftedM "Could not get datum for NFT" $ getDatumByHash datumHash <#>
          (_ >>= unwrap >>> fromData)
      projectId <- asks $ unwrap >>> _.projectId
      metadata <- liftedE $ liftAff $
        getFullSeabugMetadataWithBackoff datum projectId
      pure $ Just { input: ref, output, metadata }