module Seabug.Contract.MarketPlaceBuy
  ( marketplaceBuy
  , marketplaceBuy'
  ) where

import Contract.Prelude

import Contract.Address (ownPaymentPubKeyHash)
import Contract.Monad (Contract, liftContractM, liftedE, liftedM)
import Contract.Numeric.Natural (toBigInt)
import Contract.PlutusData
  ( Datum(Datum)
  , Redeemer(Redeemer)
  , toData
  , unitRedeemer
  )
import Contract.ScriptLookups
  ( ScriptLookups
  , mintingPolicy
  , ownPaymentPubKeyHash
  , typedValidatorLookups
  , unspentOutputs
  , validator
  ) as ScriptLookups
import Contract.ScriptLookups (UnattachedUnbalancedTx, mkUnbalancedTx)
import Contract.Transaction
  ( TransactionHash
  , TransactionOutput(TransactionOutput)
  , balanceAndSignTxE
  , submit
  )
import Contract.TxConstraints
  ( TxConstraints
  , mustMintValueWithRedeemer
  , mustPayToScript
  , mustPayWithDatumToPubKey
  , mustSpendScriptOutput
  )
import Contract.Utxos (utxosAt)
import Contract.Value (CurrencySymbol, TokenName)
import Contract.Value as Value
import Contract.Wallet (getWalletAddress)
import Data.Array (find) as Array
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt, fromInt)
import Data.Map (insert, toUnfoldable)
import Seabug.Contract.Util (minAdaOnlyUTxOValue, setSeabugMetadata)
import Seabug.MarketPlace (marketplaceValidator, marketplaceValidatorAddr)
import Seabug.Metadata.Share (maxShare)
import Seabug.MintingPolicy (mkMintingPolicy, mkTokenName)
import Seabug.Types
  ( MarketplaceDatum(MarketplaceDatum)
  , MintAct(ChangeOwner)
  , NftData(..)
  , NftId(NftId)
  )

-- | Attempts to submit a transaction where the current user purchases
-- | the passed NFT.
marketplaceBuy :: forall (r :: Row Type). NftData -> Contract r Unit
marketplaceBuy = void <<< marketplaceBuy'

-- | Attempts to submit a transaction where the current user purchases
-- | the passed NFT, returns the transaction hash and the updated
-- | sgNft.
marketplaceBuy'
  :: forall (r :: Row Type)
   . NftData
  -> Contract r (TransactionHash /\ (CurrencySymbol /\ TokenName))
marketplaceBuy' nftData = do
  unattachedBalancedTx /\ curr /\ newName <- mkMarketplaceBuyTx nftData
  signedTx <- liftedE
    ( lmap
        ( \e ->
            "marketplaceBuy: Cannot balance, reindex redeemers, attach datums/redeemers\
            \ and sign: " <> show e
        )
        <$> balanceAndSignTxE unattachedBalancedTx
    )
  transactionHash <- submit signedTx
  log $ "marketplaceBuy: Transaction successfully submitted with hash: "
    <> show transactionHash
  log $ "marketplaceBuy: Buy successful: " <> show (curr /\ newName)
  pure $ transactionHash /\ (curr /\ newName)

-- https://github.com/mlabs-haskell/plutus-use-cases/blob/927eade6aa9ad37bf2e9acaf8a14ae2fc304b5ba/mlabs/src/Mlabs/EfficientNFT/Contract/MarketplaceBuy.hs
-- rev: 2c9ce295ccef4af3f3cb785982dfe554f8781541
mkMarketplaceBuyTx
  :: forall (r :: Row Type)
   . NftData
  -> Contract r
       (UnattachedUnbalancedTx /\ Value.CurrencySymbol /\ Value.TokenName)
mkMarketplaceBuyTx (NftData nftData) = do
  let nftCollection = unwrap nftData.nftCollection
  pkh <- liftedM "marketplaceBuy: Cannot get PaymentPubKeyHash"
    ownPaymentPubKeyHash
  policy <- liftedE $ mkMintingPolicy $ wrap nftCollection

  curr <- liftedM "marketplaceBuy: Cannot get CurrencySymbol"
    $ liftAff
    $ Value.scriptCurrencySymbol policy

  marketplaceValidator' <- unwrap <$> marketplaceValidator
  let
    nft = nftData.nftId
    nft' = unwrap nft
    newNft = NftId nft' { owner = pkh }
  scriptAddr <- marketplaceValidatorAddr
  oldName <- liftedM "marketplaceBuy: Cannot hash old token" $ mkTokenName nft
  newName <- liftedM "marketplaceBuy: Cannot hash new token"
    $ mkTokenName newNft
  let
    oldNftValue = Value.singleton curr oldName $ negate one
    newNftValue = Value.singleton curr newName one
    nftPrice = nft'.price
    valHash = marketplaceValidator'.validatorHash
    mintRedeemer = Redeemer $ toData $ ChangeOwner nft pkh

    containsNft :: forall (a :: Type). (a /\ TransactionOutput) -> Boolean
    containsNft (_ /\ TransactionOutput out) =
      Value.valueOf out.amount curr oldName == one

    getShare :: BigInt -> BigInt
    getShare share = (toBigInt nftPrice * share) `div` fromInt maxShare

    shareToSubtract :: BigInt -> BigInt
    shareToSubtract v
      | v < minAdaOnlyUTxOValue = zero
      | otherwise = v

    filterLowValue
      :: BigInt
      -> (Value.Value -> TxConstraints Void Void)
      -> TxConstraints Void Void
    filterLowValue v t
      | v < minAdaOnlyUTxOValue = mempty
      | otherwise = t (Value.lovelaceValueOf v)

    authorShare = getShare $ toBigInt nftCollection.authorShare
    daoShare = getShare $ toBigInt nftCollection.daoShare
    ownerShare = Value.lovelaceValueOf
      $ toBigInt nftPrice
      - shareToSubtract authorShare
      - shareToSubtract daoShare
    datum = Datum $ toData $ curr /\ oldName
  userAddr <- liftedM "marketplaceBuy: Cannot get user addr" getWalletAddress
  userUtxos <-
    liftedM "marketplaceBuy: Cannot get user Utxos" $ utxosAt userAddr
  scriptUtxos <-
    liftedM "marketplaceBuy: Cannot get script Utxos" $ utxosAt scriptAddr
  utxo /\ utxoIndex <-
    liftContractM "marketplaceBuy: NFT not found on marketplace"
      $ Array.find containsNft
      $ toUnfoldable
      $ unwrap scriptUtxos
  let
    utxosForTx = insert utxo utxoIndex $ unwrap userUtxos

    lookup :: ScriptLookups.ScriptLookups Void
    lookup = mconcat
      [ ScriptLookups.mintingPolicy policy
      , ScriptLookups.typedValidatorLookups $ wrap marketplaceValidator'
      , ScriptLookups.validator marketplaceValidator'.validator
      , ScriptLookups.unspentOutputs utxosForTx
      , ScriptLookups.ownPaymentPubKeyHash pkh
      ]

    constraints :: TxConstraints Void Void
    constraints =
      filterLowValue
        daoShare
        (mustPayToScript nftCollection.daoScript datum)
        <> filterLowValue
          authorShare
          (mustPayWithDatumToPubKey nftCollection.author datum)
        <> mconcat
          [ mustMintValueWithRedeemer mintRedeemer (newNftValue <> oldNftValue)
          , mustSpendScriptOutput utxo unitRedeemer
          , mustPayWithDatumToPubKey nft'.owner datum ownerShare
          , mustPayToScript
              valHash
              ( Datum $ toData $
                  MarketplaceDatum { getMarketplaceDatum: curr /\ newName }
              )
              newNftValue
          ]
  txDatumsRedeemerTxIns <- liftedE $ mkUnbalancedTx lookup constraints
  txWithMetadata <-
    setSeabugMetadata (wrap nftData { nftId = newNft }) curr
      txDatumsRedeemerTxIns
  pure $ txWithMetadata /\ curr /\ newName
