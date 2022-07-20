-- | Helper to list the utxo with relevant NFT at the market validator script
module Seabug.Contract.MarketPlaceListNft
  ( ListNftResult
  , marketPlaceListNft
  ) where

import Contract.Prelude

import Contract.Address (getNetworkId, typedValidatorEnterpriseAddress)
import Contract.Monad (Contract, liftContractE, liftedM)
import Contract.PlutusData (fromData, getDatumsByHashes)
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
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
import Seabug.Metadata (FullSeabugMetadata, getFullSeabugMetadataWithBackoff)
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
        -- TODO: this is a temporary solution to only show NFTs
        -- known to work. When minting an NFT, the sgNft's
        -- transaction hash should be added here
        guard $ any
          ( \txHash -> (unwrap input # _.transactionId) ==
              (wrap $ hexToByteArrayUnsafe txHash)
          )
          [ "db55d6708d1d38c3a85b498c89cf34ef1bcf40295092fcbff2550daefe289cd1"
          , "ee692437895d27c92f34f6fb43ccc8ed14fec0a4ea2274073dcf07a8cf0662a6"
          ]
        metadata <- MaybeT $ map hush $
          getFullSeabugMetadataWithBackoff (curr /\ name) projectId
        pure { input, output, metadata }
  pure $ catMaybes withMetadata
