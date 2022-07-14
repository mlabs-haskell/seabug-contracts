module Seabug.Contract.Mint where

import Contract.Prelude

import Contract.Address
  ( NetworkId(..)
  , Slot
  , ownPaymentPubKeyHash
  , payPubKeyHashEnterpriseAddress
  )
import Contract.Chain (ChainTip(..), Tip(..), getTip)
import Contract.Monad (Contract, liftContractE, liftContractM, liftedE, liftedM)
import Contract.PlutusData (toData)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (validatorHash)
import Contract.Time (from, getEraSummaries, getSystemStart, slotToPosixTime)
import Contract.Transaction (balanceAndSignTxE, submit)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , scriptCurrencySymbol
  , singleton
  )
import Seabug.Lock (mkLockScript)
import Seabug.MarketPlace (marketplaceValidator)
import Seabug.Token as Token
import Seabug.Types
  ( LockDatum(..)
  , MarketplaceDatum(..)
  , MintAct(..)
  , MintParams(..)
  , NftCollection(..)
  , NftId(..)
  )
import Types.BigNum as BigNum

-- | TODO: Is slot 0 for TipAtGenesis okay?
slotFromTip :: Tip -> Slot
slotFromTip TipAtGenesis = wrap $ BigNum.zero
slotFromTip (Tip (ChainTip { slot })) = slot

mintWithCollection
  :: forall (r :: Row Type)
   . CurrencySymbol /\ TokenName
  -> MintParams
  -> Contract r Unit
mintWithCollection
  (collectionNftCs /\ collectionNftTn)
  ( MintParams
      { price, lockLockup, lockLockupEnd, authorShare, daoShare, feeVaultKeys }
  ) = do
  owner <- liftedM "Cannot get PaymentPubKeyHash" ownPaymentPubKeyHash
  addr <- liftContractM "Cannot get user address" $
    payPubKeyHashEnterpriseAddress TestnetId owner
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
  -- TODO: check if we should be using Token or MintingPolicy, they
  -- seem to be the same script
  unappliedMintingPolicy <- liftContractE Token.unappliedMintingPolicy
  policy <- liftedM "Could not get minting policy" $ Token.policy collection
    unappliedMintingPolicy
  curr <- liftedM "Could not get currency symbol" $ liftAff $
    scriptCurrencySymbol policy
  tn <- liftedM "Could not get token name" $ Token.mkTokenName nft
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
  unBalancedTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  signedTx <- liftedE $ balanceAndSignTxE unBalancedTx
  transactionHash <- submit signedTx
  log $ "Mint transaction successfully submitted with hash: " <> show
    transactionHash
