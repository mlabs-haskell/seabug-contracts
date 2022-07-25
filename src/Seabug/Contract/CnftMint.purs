module Seabug.Contract.CnftMint where

import Contract.Prelude

import Contract.Address
  ( getNetworkId
  , ownPaymentPubKeyHash
  , ownStakePubKeyHash
  , payPubKeyHashBaseAddress
  )
import Contract.AuxiliaryData (setTxMetadata)
import Contract.Monad (Contract, liftContractM, liftedE, liftedM)
import Contract.Prim.ByteArray (hexToByteArray)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (mintingPolicyHash)
import Contract.Transaction (TransactionHash, balanceAndSignTxE, submit)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , mkTokenName
  , scriptCurrencySymbol
  , singleton
  )
import Data.Array (head)
import Data.Map as Map
import Data.NonEmpty as NonEmpty
import Metadata.Cip25.Cip25String (mkCip25String)
import Metadata.Cip25.V2 (Cip25Metadata(..), Cip25MetadataEntry(..))
import Seabug.CnftMintPolicy (mkCnftMintingPolicy)
import Seabug.Types (MintCnftParams(..))

-- | Mint a collection NFT
mintCnft
  :: forall (r :: Row Type)
   . MintCnftParams
  -> Contract r (TransactionHash /\ (CurrencySymbol /\ TokenName))
mintCnft (MintCnftParams params) = do
  owner <- liftedM "Cannot get PaymentPubKeyHash" ownPaymentPubKeyHash
  ownerStake <- liftedM "Cannot get StakePubKeyHash" ownStakePubKeyHash
  networkId <- getNetworkId
  addr <- liftContractM "Cannot get user address" $
    payPubKeyHashBaseAddress networkId owner ownerStake
  utxos <- liftedM "Cannot get user utxos" $ utxosAt addr
  oref /\ _ <- liftContractM "Cannot find user utxo"
    $ head
    $ Map.toUnfoldableUnordered (unwrap utxos)
  policy <- liftedE $ mkCnftMintingPolicy oref
  curr <- liftedM "Could not get currency symbol" $ liftAff $
    scriptCurrencySymbol policy
  -- TODO: figure out how to encode the token name (base64 maybe), see
  -- https://github.com/mlabs-haskell/seabug-contracts/issues/26
  tn <- liftContractM "Invalid token name"
    $ mkTokenName
    =<< hexToByteArray params.tokenNameString
  let
    value = singleton curr tn one
    lookups = mconcat
      [ Lookups.mintingPolicy policy
      , Lookups.unspentOutputs $ unwrap utxos
      ]

    constraints :: Constraints.TxConstraints Unit Unit
    constraints = mconcat
      [ Constraints.mustMintValue value
      , Constraints.mustSpendPubKeyOutput oref
      , Constraints.mustPayToPubKeyAddress owner ownerStake value
      ]
  unbalancedTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  policyHash <- liftedM "Could not get minting policy hash" $ liftAff $
    mintingPolicyHash policy
  name <- liftContractM "Invalid CIP25 NFT name. The name is probably too long."
    $ mkCip25String params.name
  unbalancedTxWithMetadata <- setTxMetadata unbalancedTx $ Cip25Metadata
    [ Cip25MetadataEntry
        { policyId: policyHash
        , assetName: wrap tn
        , name
        , image: params.imageUri
        , mediaType: Nothing
        , description: Just params.description
        , files: []
        }
    ]
  signedTx <- liftedE $ balanceAndSignTxE unbalancedTxWithMetadata
  transactionHash <- submit signedTx
  log $ "CNFT Mint transaction successfully submitted with hash: "
    <> show transactionHash
  pure (transactionHash /\ curr /\ tn)
