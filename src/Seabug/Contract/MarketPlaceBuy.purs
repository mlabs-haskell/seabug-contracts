module Seabug.Contract.MarketPlaceBuy
  ( marketplaceBuy
  , mkMarketplaceTx
  -- TODO: move this function
  , setSeabugMetadata
  ) where

import Contract.Prelude

import Contract.Address (getNetworkId, ownPaymentPubKeyHash)
import Contract.AuxiliaryData (setTxMetadata)
import Contract.Monad (Contract, liftContractE, liftContractM, liftedE, liftedM)
import Contract.Numeric.Natural (toBigInt)
import Contract.PlutusData
  ( Datum(Datum)
  , Redeemer(Redeemer)
  , toData
  , unitRedeemer
  )
import Contract.ScriptLookups (UnattachedUnbalancedTx, mkUnbalancedTx)
import Contract.ScriptLookups
  ( mintingPolicy
  , validator
  , ownPaymentPubKeyHash
  , typedValidatorLookups
  , unspentOutputs
  ) as ScriptLookups
import Contract.Scripts (typedValidatorEnterpriseAddress)
import Contract.Transaction
  ( TransactionOutput(TransactionOutput)
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
import Contract.Value (CurrencySymbol)
import Contract.Value as Value
import Contract.Wallet (getWalletAddress)
import Data.Array (find) as Array
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt, fromInt)
import Data.BigInt as BigInt
import Data.Map (insert, toUnfoldable)
import Data.String.Common (joinWith)
import Seabug.MarketPlace (marketplaceValidator)
import Seabug.Metadata.Share (mkShare)
import Seabug.Metadata.Types (SeabugMetadata(..))
import Seabug.MintingPolicy (mkMintingPolicy, mkTokenName)
import Seabug.Types
  ( MarketplaceDatum(MarketplaceDatum)
  , MintAct(ChangeOwner)
  , NftData(..)
  , NftId(NftId)
  )

minAdaOnlyUTxOValue :: BigInt
minAdaOnlyUTxOValue = fromInt 1_000_000

-- TODO docstring
marketplaceBuy :: forall (r :: Row Type). NftData -> Contract r Unit
marketplaceBuy nftData = do
  unattachedBalancedTx /\ curr /\ newName <- mkMarketplaceTx nftData
  -- `balanceAndSignTx` does the following:
  -- 1) Balance a transaction
  -- 2) Reindex `Spend` redeemers after finalising transaction inputs.
  -- 3) Attach datums and redeemers to transaction.
  -- 3) Sign tx, returning the Cbor-hex encoded `ByteArray`.
  signedTx <- liftedE
    ( lmap
        ( \e ->
            "marketplaceBuy: Cannot balance, reindex redeemers, attach datums/redeemers\
            \ and sign: " <> show e
        )
        <$> balanceAndSignTxE unattachedBalancedTx
    )
  -- Submit transaction using Cbor-hex encoded `ByteArray`
  transactionHash <- submit signedTx
  log $ "marketplaceBuy: Transaction successfully submitted with hash: "
    <> show transactionHash
  log $ "marketplaceBuy: Buy successful: " <> show (curr /\ newName)

-- https://github.com/mlabs-haskell/plutus-use-cases/blob/927eade6aa9ad37bf2e9acaf8a14ae2fc304b5ba/mlabs/src/Mlabs/EfficientNFT/Contract/MarketplaceBuy.hs
-- rev: 2c9ce295ccef4af3f3cb785982dfe554f8781541
-- The `MintingPolicy` may be decoded as Json, although I'm not sure as we don't
-- have `mkMintingPolicyScript`. Otherwise, it's an policy that hasn't been
-- applied to arguments. See `Seabug.Token.policy`
-- TODO docstring
mkMarketplaceTx
  :: forall (r :: Row Type)
   . NftData
  -> Contract r
       (UnattachedUnbalancedTx /\ Value.CurrencySymbol /\ Value.TokenName)
mkMarketplaceTx (NftData nftData) = do
  let nftCollection = unwrap nftData.nftCollection
  pkh <- liftedM "marketplaceBuy: Cannot get PaymentPubKeyHash"
    ownPaymentPubKeyHash
  log $ "policy args: " <> joinWith "; "
    [ "collectionNftCs: " <> show nftCollection.collectionNftCs
    , "lockingScript: " <> show nftCollection.lockingScript
    , "author: " <> show nftCollection.author
    , "authorShare: " <> show nftCollection.authorShare
    , "daoScript: " <> show nftCollection.daoScript
    , "daoShare: " <> show nftCollection.daoShare
    ]
  policy <- liftedE $ mkMintingPolicy $ wrap nftCollection

  curr <- liftedM "marketplaceBuy: Cannot get CurrencySymbol"
    $ liftAff
    $ Value.scriptCurrencySymbol policy
  -- curr <- liftContractM "marketplaceBuy: Cannot get CurrencySymbol"
  --   $ mkCurrencySymbol
  --   $ Value.getCurrencySymbol currSym

  -- Read in the typed validator:
  marketplaceValidator' <- unwrap <$> liftContractE marketplaceValidator
  networkId <- getNetworkId
  let
    nft = nftData.nftId
    nft' = unwrap nft
    newNft = NftId nft' { owner = pkh }
  scriptAddr <-
    liftedM "marketplaceBuy: Cannot convert validator hash to address"
      $ pure
      $ typedValidatorEnterpriseAddress networkId
      $ wrap marketplaceValidator'
  oldName <- liftedM "marketplaceBuy: Cannot hash old token" $ mkTokenName nft
  newName <- liftedM "marketplaceBuy: Cannot hash new token"
    $ mkTokenName newNft
  log $ "curr: " <> show curr
  log $ "oldName: " <> show oldName
  log $ "newName: " <> show newName
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
    getShare share = (toBigInt nftPrice * share) `div` fromInt 10_000

    shareToSubtract :: BigInt -> BigInt
    shareToSubtract v
      | v < minAdaOnlyUTxOValue = zero
      | otherwise = v

    filterLowValue
      :: BigInt
      -> (Value.Value -> TxConstraints Unit Unit)
      -> TxConstraints Unit Unit
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
  log $ "scriptUtxos: " <> show scriptUtxos
  utxo /\ utxoIndex <-
    liftContractM "marketplaceBuy: NFT not found on marketplace"
      $ Array.find containsNft
      $ toUnfoldable
      $ unwrap scriptUtxos
  let
    utxosForTx = insert utxo utxoIndex $ unwrap userUtxos
    lookup = mconcat
      [ ScriptLookups.mintingPolicy policy
      , ScriptLookups.typedValidatorLookups $ wrap marketplaceValidator'
      , ScriptLookups.validator marketplaceValidator'.validator
      , ScriptLookups.unspentOutputs utxosForTx
      , ScriptLookups.ownPaymentPubKeyHash pkh
      ]

    minAdaVal = Value.lovelaceValueOf $ fromInt 2_000_000

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
              ( newNftValue <> minAdaVal
              )
          ]
  log $ "utxosTx: " <> show utxosForTx
  -- Created unbalanced tx which stripped datums and redeemers with tx inputs,
  -- the datums and redeemers will be reattached using a server with redeemers
  -- reindexed also.
  txDatumsRedeemerTxIns <- liftedE $ mkUnbalancedTx lookup constraints
  txWithMetadata <-
    setSeabugMetadata (wrap nftData { nftId = newNft }) curr
      txDatumsRedeemerTxIns
  pure $ txWithMetadata /\ curr /\ newName

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
