module Seabug.Contract.Util (setSeabugMetadata) where

import Contract.Prelude

import Contract.AuxiliaryData (setTxMetadata)
import Contract.Monad (Contract, liftContractM)
import Contract.Numeric.Natural (toBigInt)
import Contract.ScriptLookups (UnattachedUnbalancedTx)
import Contract.Value (CurrencySymbol)
import Contract.Value as Value
import Data.BigInt as BigInt
import Seabug.Metadata.Share (mkShare)
import Seabug.Metadata.Types (SeabugMetadata(..))
import Seabug.Types (NftData(..))

-- | Set metadata on the transaction for the given NFT
setSeabugMetadata
  :: forall (r :: Row Type)
   . NftData
  -> CurrencySymbol -- | The currency symbol of the self-governed nft
  -> UnattachedUnbalancedTx
  -> Contract r UnattachedUnbalancedTx
setSeabugMetadata (NftData nftData) sgNftCurr tx = do
  let
    nftCollection = unwrap nftData.nftCollection
    nftId = unwrap nftData.nftId
    natToShare nat = liftContractM "Invalid share"
      $ mkShare
      =<< BigInt.toInt (toBigInt nat)
    policyId = Value.currencyMPSHash sgNftCurr
  authorShareValidated <- natToShare nftCollection.authorShare
  marketplaceShareValidated <- natToShare nftCollection.daoShare
  setTxMetadata tx $ SeabugMetadata
    { policyId
    , mintPolicy: mempty
    , collectionNftCS: nftCollection.collectionNftCs
    , lockingScript: nftCollection.lockingScript
    , collectionNftTN: nftId.collectionNftTn
    , authorPkh: unwrap nftCollection.author
    , authorShare: authorShareValidated
    , marketplaceScript: nftCollection.daoScript
    , marketplaceShare: marketplaceShareValidated
    , ownerPkh: unwrap nftId.owner
    , ownerPrice: nftId.price
    }