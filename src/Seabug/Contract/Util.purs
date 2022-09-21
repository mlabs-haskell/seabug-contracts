module Seabug.Contract.Util
  ( ReturnBehaviour(..)
  , SeabugTxData
  , getSeabugMetadata
  , minUTxOValue
  , mkChangeNftIdTxData
  , modify
  , payBehaviour
  , seabugTxToMarketTx
  ) where

import Contract.Prelude

import Contract.Address (getNetworkId)
import Contract.AuxiliaryData (setTxMetadata)
import Contract.Monad (Contract, liftContractE, liftContractM, liftedE, liftedM)
import Contract.Numeric.Natural (toBigInt)
import Contract.PlutusData
  ( Datum(Datum)
  , Redeemer(Redeemer)
  , toData
  , unitRedeemer
  )
import Contract.ScriptLookups (ScriptLookups, mkUnbalancedTx)
import Contract.ScriptLookups
  ( ScriptLookups
  , mintingPolicy
  , typedValidatorLookups
  , unspentOutputs
  , validator
  ) as ScriptLookups
import Contract.Scripts (typedValidatorEnterpriseAddress)
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
  , mustSpendScriptOutput
  )
import Contract.Utxos (utxosAt)
import Contract.Value (CurrencySymbol)
import Contract.Value as Value
import Contract.Wallet (getWalletAddress)
import Data.Array (find) as Array
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Map (singleton, toUnfoldable) as Map
import Plutus.Types.Transaction (UtxoM)
import Seabug.MarketPlace (marketplaceValidator)
import Seabug.Metadata.Share (mkShare)
import Seabug.Metadata.Types (SeabugMetadata(..))
import Seabug.MintingPolicy (mkMintingPolicy, mkTokenName)
import Seabug.Types
  ( MarketplaceDatum(MarketplaceDatum)
  , MintAct
  , NftData(..)
  , NftId
  )
import Types.Scripts (ValidatorHash)
import Types.Transaction (TransactionInput)

type SeabugTxData =
  { constraints :: TxConstraints Void Void
  , lookups :: ScriptLookups Void
  , oldAsset :: Value.CurrencySymbol /\ Value.TokenName
  , newAsset :: Value.CurrencySymbol /\ Value.TokenName
  , inputUtxo :: TransactionInput
  , newNft :: NftId
  }

-- TODO, remove this, update Data.Newtype
modify :: forall t a. Newtype t a => (a -> a) -> t -> t
modify fn t = wrap (fn (unwrap t))

data ReturnBehaviour = ToMarketPlace | ToCaller

payBehaviour
  :: ReturnBehaviour
  -> ValidatorHash
  -> (Value.CurrencySymbol /\ Value.TokenName)
  -> TxConstraints Void Void
payBehaviour ToCaller _ _ = mempty -- Balancing will return the token to the caller
payBehaviour ToMarketPlace valHash asset =
  mustPayToScript
    valHash
    ( Datum $ toData $
        MarketplaceDatum { getMarketplaceDatum: asset }
    )
    ( Value.singleton (fst asset) (snd asset) one <> Value.lovelaceValueOf
        minUTxOValue
    )

-- | Build and submit a transaction involving a given nft, specifying
-- | if the nft should be sent to the current user or the marketplace.
seabugTxToMarketTx
  :: forall (r :: Row Type)
   . String
  -> ReturnBehaviour
  -> (NftData -> Maybe UtxoM -> Contract r SeabugTxData)
  -> NftData
  -> Contract r (TransactionHash /\ SeabugTxData)
seabugTxToMarketTx name retBehaviour mkTxData nftData = do
  marketplaceValidator' <- unwrap <$> marketplaceValidator
  networkId <- getNetworkId
  scriptAddr <-
    liftContractM (name <> ": Cannot convert validator hash to address")
      $ typedValidatorEnterpriseAddress networkId
      $ wrap marketplaceValidator'
  scriptUtxos <-
    liftedM (name <> ": Cannot get script Utxos") $ utxosAt scriptAddr

  txData <- mkTxData nftData (Just scriptUtxos)
  let
    valHash = marketplaceValidator'.validatorHash

    lookups :: ScriptLookups.ScriptLookups Void
    lookups = txData.lookups <> mconcat
      [ ScriptLookups.typedValidatorLookups $ wrap marketplaceValidator'
      , ScriptLookups.validator marketplaceValidator'.validator
      ]

    constraints :: TxConstraints Void Void
    constraints = txData.constraints
      <> mustSpendScriptOutput txData.inputUtxo unitRedeemer
      <> payBehaviour retBehaviour valHash txData.newAsset

  txDatumsRedeemerTxIns <- liftedE $ mkUnbalancedTx lookups constraints
  metadata <- liftContractE $ getSeabugMetadata
    (modify (_ { nftId = txData.newNft }) nftData)
    (fst txData.newAsset)
  txWithMetadata <- setTxMetadata txDatumsRedeemerTxIns metadata

  signedTx <- liftedE
    ( lmap
        ( \e ->
            name
              <>
                ": Cannot balance, reindex redeemers, attach datums/redeemers\
                \ and sign: "
              <> show e
        )
        <$> balanceAndSignTxE txWithMetadata
    )
  transactionHash <- submit signedTx
  log $ name <> ": Transaction successfully submitted with hash: "
    <> show transactionHash
  log $ name <> ": Buy successful: " <> show txData.newAsset
  pure $ transactionHash /\ txData

-- | Make tx data to change an nft's `NftId` by "reminting"
-- | it. Provide a utxo map for the nft's utxo to be found in; if a
-- | map is not provided, the current user's address will be searched.
mkChangeNftIdTxData
  :: forall (r :: Row Type)
   . String
  -> (NftId -> MintAct) -- To make the minting policy redeemer
  -> (NftId -> NftId) -- To update the nft's `NftId`
  -> NftData
  -> Maybe UtxoM
  -> Contract r SeabugTxData
mkChangeNftIdTxData name act mapNft (NftData nftData) mScriptUtxos = do
  let nftCollection = unwrap nftData.nftCollection
  policy <- liftedE $ mkMintingPolicy $ wrap nftCollection

  curr <- liftedM (name <> ": Cannot get CurrencySymbol")
    $ liftAff
    $ Value.scriptCurrencySymbol policy

  let newNft = mapNft nftData.nftId
  oldName <- liftedM (name <> ": Cannot hash old token")
    $ mkTokenName nftData.nftId
  newName <- liftedM (name <> ": Cannot hash new token")
    $ mkTokenName newNft
  let
    oldNftValue = Value.singleton curr oldName $ negate one
    newNftValue = Value.singleton curr newName one
    mintRedeemer = Redeemer $ toData $ act nftData.nftId

    containsNft :: forall (a :: Type). (a /\ TransactionOutput) -> Boolean
    containsNft (_ /\ TransactionOutput out) =
      Value.valueOf out.amount curr oldName == one

    findNftUtxo = liftContractM (name <> ": NFT not found")
      <<< Array.find containsNft
      <<< Map.toUnfoldable
      <<< unwrap

  utxo /\ utxoIndex <- findNftUtxo =<< case mScriptUtxos of
    Nothing -> do
      userAddr <- liftedM (name <> ": Cannot get user addr") getWalletAddress
      liftedM (name <> ": Cannot get user Utxos") $ utxosAt userAddr
    Just scriptUtxos -> pure scriptUtxos

  let
    lookups = mconcat
      [ ScriptLookups.mintingPolicy policy
      , ScriptLookups.unspentOutputs $ Map.singleton utxo utxoIndex
      ]

    constraints = mustMintValueWithRedeemer mintRedeemer
      (newNftValue <> oldNftValue)

  pure
    { constraints: constraints
    , lookups: lookups
    , oldAsset: curr /\ oldName
    , newAsset: curr /\ newName
    , inputUtxo: utxo
    , newNft: newNft
    }

minUTxOValue :: BigInt
minUTxOValue = BigInt.fromInt 2_000_000

-- | Set metadata on the transaction for the given NFT
getSeabugMetadata
  :: forall (r :: Row Type)
   . NftData
  -> CurrencySymbol -- | The currency symbol of the self-governed nft
  -> Either String SeabugMetadata
getSeabugMetadata (NftData nftData) sgNftCurr = do
  let
    nftCollection = unwrap nftData.nftCollection
    nftId = unwrap nftData.nftId
    natToShare nat = note "Invalid share"
      $ mkShare
      =<< BigInt.toInt (toBigInt nat)
  authorShareValidated <- natToShare nftCollection.authorShare
  marketplaceShareValidated <- natToShare nftCollection.daoShare
  pure $ SeabugMetadata
    { policyId: sgNftCurr
    , mintPolicy: "V1"
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
