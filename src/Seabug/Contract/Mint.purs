module Seabug.Contract.Mint
  ( mintWithCollection
  , mintWithCollection'
  , mintWithCollectionTest
  ) where

import Contract.Prelude

import Contract.Address
  ( ownPaymentPubKeyHash
  , getWalletAddress
  )
import Contract.AuxiliaryData (setTxMetadata)
import Contract.Chain (currentSlot, currentTime)
import Contract.Monad (Contract, liftContractE, liftedE, liftedM)
import Contract.PlutusData (toData)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (validatorHash)
import Contract.Time (from)
import Contract.Transaction (TransactionHash, balanceAndSignTxE, submit)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , scriptCurrencySymbol
  , singleton
  )
import Seabug.Contract.Util
  ( ReturnBehaviour(ToMarketPlace)
  , getSeabugMetadata
  , payBehaviour
  )
import Seabug.Lock (mkLockScript)
import Seabug.MarketPlace (marketplaceValidator)
import Seabug.MintingPolicy as MintingPolicy
import Seabug.Types
  ( LockDatum(..)
  , MintAct(..)
  , MintParams(..)
  , NftCollection(..)
  , NftData(..)
  , NftId(..)
  )

mintWithCollectionTest
  :: forall (r :: Row Type)
   . ReturnBehaviour
  -> CurrencySymbol /\ TokenName
  -> MintParams
  -> ( Constraints.TxConstraints Void Void
       -> Contract r (Constraints.TxConstraints Void Void)
     )
  -> Contract r (TransactionHash /\ (CurrencySymbol /\ TokenName) /\ NftData)
mintWithCollectionTest
  retBehaviour
  (collectionNftCs /\ collectionNftTn)
  ( MintParams
      { price, lockLockup, lockLockupEnd, authorShare, daoShare }
  )
  modConstraints = do
  owner <- liftedM "Cannot get PaymentPubKeyHash" ownPaymentPubKeyHash
  addr <- liftedM "Cannot get address" getWalletAddress
  utxos <- liftedM "Cannot get user utxos" $ utxosAt addr
  marketplaceValidator' <- unwrap <$> marketplaceValidator
  lockingScript <- mkLockScript collectionNftCs lockLockup lockLockupEnd
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
  entered <- currentSlot
  now <- currentTime
  let
    nftValue = singleton curr tn one

    lookups :: Lookups.ScriptLookups Void
    lookups = mconcat
      [ Lookups.mintingPolicy policy, Lookups.unspentOutputs (unwrap utxos) ]

    constraints :: Constraints.TxConstraints Void Void
    constraints = mconcat
      [ Constraints.mustMintValueWithRedeemer (wrap $ toData $ MintToken nft)
          nftValue
      , payBehaviour retBehaviour marketplaceValidator'.validatorHash
          (curr /\ tn)
      , Constraints.mustPayToScript lockingScriptHash
          ( wrap $ toData $ LockDatum
              { sgNft: curr
              , entered
              , underlyingTn: collectionNftTn
              }
          ) $ singleton collectionNftCs collectionNftTn one
      , Constraints.mustValidateIn $ from now
      ]
  unbalancedTx <- liftedE $ Lookups.mkUnbalancedTx lookups
    =<< modConstraints constraints
  let nftData = NftData { nftId: nft, nftCollection: collection }
  metadata <- liftContractE $ getSeabugMetadata nftData curr
  unbalancedTxWithMetadata <- setTxMetadata unbalancedTx metadata
  signedTx <- liftedE $ balanceAndSignTxE unbalancedTxWithMetadata
  transactionHash <- submit signedTx
  log $ "Mint transaction successfully submitted with hash: "
    <> show transactionHash
  pure $ transactionHash /\ (curr /\ tn) /\ nftData

-- | Mint the self-governed NFT for the given collection, and return
-- | sgNft's asset class and nft data.
mintWithCollection'
  :: forall (r :: Row Type)
   . CurrencySymbol /\ TokenName
  -> MintParams
  -> Contract r (TransactionHash /\ (CurrencySymbol /\ TokenName) /\ NftData)
mintWithCollection' c p = mintWithCollectionTest ToMarketPlace c p pure

-- | Mint the self-governed NFT for the given collection.
mintWithCollection
  :: forall (r :: Row Type)
   . CurrencySymbol /\ TokenName
  -> MintParams
  -> Contract r TransactionHash
mintWithCollection c p = fst <$> mintWithCollection' c p
