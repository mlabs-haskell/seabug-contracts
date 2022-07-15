module Seabug.Contract.Mint where

import Contract.Prelude

import Contract.Address
  ( getNetworkId
  , ownPaymentPubKeyHash
  , payPubKeyHashEnterpriseAddress
  )
import Contract.Monad (Contract, liftContractM, liftedE, liftedM)
import Contract.Prim.ByteArray (hexToByteArray)
import Contract.Utxos (utxosAt)
import Contract.Value (mkTokenName, scriptCurrencySymbol, singleton)
import Data.Array (head)
import Data.Map as Map
import Seabug.CNFTMintPolicy (mkCnftMintingPolicy)
import Seabug.Types (MintCnftParams(..))

mintCnft
  :: forall (r :: Row Type)
   . MintCnftParams
  -> Contract r Unit
mintCnft (MintCnftParams params) = do
  owner <- liftedM "Cannot get PaymentPubKeyHash" ownPaymentPubKeyHash
  networkId <- getNetworkId
  addr <- liftContractM "Cannot get user address" $
    payPubKeyHashEnterpriseAddress networkId owner
  utxos <- liftedM "Cannot get user utxos" $ utxosAt addr
  oref /\ _ <- liftContractM "Cannot find user utxo"
    $ head
    $ Map.toUnfoldableUnordered (unwrap utxos)
  policy <- liftedE $ mkCnftMintingPolicy oref
  curr <- liftedM "Could not get currency symbol" $ liftAff $
    scriptCurrencySymbol policy
  -- TODO: figure out how to encode the token name
  tn <- liftContractM "Invalid token name"
    $ mkTokenName
    =<< hexToByteArray params.tokenNameString
  let value = singleton curr tn one
  undefined
-- let
--   nftValue = singleton curr tn one
--   lookups = mconcat
--     [ Lookups.mintingPolicy policy, Lookups.unspentOutputs (unwrap utxos) ]

--   constraints :: Constraints.TxConstraints Unit Unit
--   constraints = mconcat
--     [ Constraints.mustMintValueWithRedeemer (wrap $ toData $ MintToken nft)
--         nftValue
--     , Constraints.mustPayToScript marketplaceValidator'.validatorHash
--         ( wrap $ toData $ MarketplaceDatum $
--             { getMarketplaceDatum: curr /\ tn }
--         )
--         nftValue
--     , Constraints.mustPayToScript lockingScriptHash
--         ( wrap $ toData $ LockDatum
--             { sgNft: curr
--             , entered: currentSlot
--             , underlyingTn: collectionNftTn
--             }
--         ) $ singleton collectionNftCs collectionNftTn one
--     , Constraints.mustValidateIn $ from now
--     ]
-- unbalancedTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
-- unbalancedTxWithMetadata <- setSeabugMetadata
--   (NftData { nftId: nft, nftCollection: collection })
--   unbalancedTx
-- signedTx <- liftedE $ balanceAndSignTxE unbalancedTxWithMetadata
-- transactionHash <- submit signedTx
-- log $ "Mint transaction successfully submitted with hash: " <> show
--   transactionHash
