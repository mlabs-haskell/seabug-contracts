module Seabug.Contract.Mint where

import Contract.Prelude

import Contract.Address
  ( Slot
  , getNetworkId
  , ownPaymentPubKeyHash
  , ownStakePubKeyHash
  , payPubKeyHashBaseAddress
  )
import Contract.Chain (ChainTip(..), Tip(..), getTip)
import Contract.Monad (Contract, liftContractE, liftContractM, liftedE, liftedM)
import Contract.PlutusData (toData)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (validatorHash)
import Contract.Time (from, getEraSummaries, getSystemStart, slotToPosixTime)
import Contract.Transaction (TransactionHash, balanceAndSignTxE, submit)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , scriptCurrencySymbol
  , singleton
  )
import Seabug.Contract.Util (setSeabugMetadata)
import Seabug.Lock (mkLockScript)
import Seabug.MarketPlace (marketplaceValidator)
import Seabug.MintingPolicy as MintingPolicy
import Seabug.Types
  ( LockDatum(..)
  , MarketplaceDatum(..)
  , MintAct(..)
  , MintParams(..)
  , NftCollection(..)
  , NftData(..)
  , NftId(..)
  )
import Types.BigNum as BigNum

-- | TODO: Is slot 0 for TipAtGenesis okay?
slotFromTip :: Tip -> Slot
slotFromTip TipAtGenesis = wrap $ BigNum.zero
slotFromTip (Tip (ChainTip { slot })) = slot

-- | Mint the self-governed NFT for the given collection.
mintWithCollection
  :: forall (r :: Row Type)
   . CurrencySymbol /\ TokenName
  -> MintParams
  -> Contract r TransactionHash
mintWithCollection
  (collectionNftCs /\ collectionNftTn)
  ( MintParams
      { price, lockLockup, lockLockupEnd, authorShare, daoShare }
  ) = do
  owner <- liftedM "Cannot get PaymentPubKeyHash" ownPaymentPubKeyHash
  ownerStake <- liftedM "Cannot get StakePubKeyHash" ownStakePubKeyHash
  networkId <- getNetworkId
  addr <- liftContractM "Cannot get user address" $
    payPubKeyHashBaseAddress networkId owner ownerStake
  utxos <- liftedM "Cannot get user utxos" $ utxosAt addr
  currentSlot <- slotFromTip <$> getTip
  marketplaceValidator' <- unwrap <$> liftContractE marketplaceValidator
  lockingScript <- liftedE $ mkLockScript collectionNftCs lockLockup
    lockLockupEnd
  lockingScriptHash <- liftedM "Could not get locking script hash" $ liftAff $
    validatorHash lockingScript
  let
    nft = NftId { collectionNftTn, price, owner }
    collection = NftCollection
      { collectionNftCs
      , lockLockup
      , lockLockupEnd
      , lockingScript: lockingScriptHash
      , author: owner
      , authorShare
      , daoScript: marketplaceValidator'.validatorHash
      , daoShare
      }
  policy <- liftedE $ MintingPolicy.mkMintingPolicy collection
  curr <- liftedM "Could not get currency symbol" $ liftAff $
    scriptCurrencySymbol policy
  tn <- liftedM "Could not get token name" $ MintingPolicy.mkTokenName nft
  eraSummaries <- getEraSummaries
  systemStart <- getSystemStart
  now <- liftedE $ liftAff $ liftEffect $
    slotToPosixTime eraSummaries systemStart currentSlot
  let
    nftValue = singleton curr tn one
    lookups = mconcat
      [ Lookups.mintingPolicy policy, Lookups.unspentOutputs (unwrap utxos) ]

    constraints :: Constraints.TxConstraints Unit Unit
    constraints = mconcat
      [ Constraints.mustMintValueWithRedeemer (wrap $ toData $ MintToken nft)
          nftValue
      , Constraints.mustPayToScript marketplaceValidator'.validatorHash
          ( wrap $ toData $ MarketplaceDatum $
              { getMarketplaceDatum: curr /\ tn }
          )
          nftValue
      , Constraints.mustPayToScript lockingScriptHash
          ( wrap $ toData $ LockDatum
              { sgNft: curr
              , entered: currentSlot
              , underlyingTn: collectionNftTn
              }
          ) $ singleton collectionNftCs collectionNftTn one
      , Constraints.mustValidateIn $ from now
      ]
  unbalancedTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  unbalancedTxWithMetadata <- setSeabugMetadata
    (NftData { nftId: nft, nftCollection: collection })
    curr
    unbalancedTx
  signedTx <- liftedE $ balanceAndSignTxE unbalancedTxWithMetadata
  transactionHash <- submit signedTx
  log $ "Mint transaction successfully submitted with hash: " <> show
    transactionHash
  pure transactionHash
