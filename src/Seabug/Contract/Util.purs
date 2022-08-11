module Seabug.Contract.Util
  ( SeabugTxData
  , ReturnBehaviour(..)
  , minAdaOnlyUTxOValue
  , mkChangeNftIdTxData
  , modify
  , seabugTxToMarketTx
  , setSeabugMetadata
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
import Contract.ScriptLookups
  ( ScriptLookups
  , UnattachedUnbalancedTx
  , mkUnbalancedTx
  )
import Contract.ScriptLookups
  ( mintingPolicy
  , typedValidatorLookups
  , unspentOutputs
  , validator
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
import Data.Map (insert, toUnfoldable)
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
import Serialization.Types (PlutusData)
import Types.Transaction (TransactionInput)

type SeabugTxData =
  { constraints :: TxConstraints Unit Unit
  , lookups :: ScriptLookups PlutusData
  , oldAsset :: Value.CurrencySymbol /\ Value.TokenName
  , newAsset :: Value.CurrencySymbol /\ Value.TokenName
  , inputUtxo :: TransactionInput
  , newNft :: NftId
  }

-- TODO, remove this, update Data.Newtype
modify :: forall t a. Newtype t a => (a -> a) -> t -> t
modify fn t = wrap (fn (unwrap t))

data ReturnBehaviour = ToMarketPlace | ToCaller

seabugTxToMarketTx
  :: forall (r :: Row Type)
   . String
  -> ReturnBehaviour
  -> (NftData -> Maybe UtxoM -> Contract r SeabugTxData)
  -> NftData
  -> Contract r Unit
seabugTxToMarketTx name retBehaviour mkTxData nftData = do
  marketplaceValidator' <- unwrap <$> liftContractE marketplaceValidator
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
    lookups = txData.lookups <> mconcat
      [ ScriptLookups.typedValidatorLookups $ wrap marketplaceValidator'
      , ScriptLookups.validator marketplaceValidator'.validator
      ]
    newNftValue = Value.singleton (fst txData.newAsset) (snd txData.newAsset)
      one
    constraints = txData.constraints
      <> mustSpendScriptOutput txData.inputUtxo unitRedeemer
      <>
        case retBehaviour of
          ToMarketPlace ->
            mustPayToScript
              valHash
              ( Datum $ toData $
                  MarketplaceDatum { getMarketplaceDatum: txData.newAsset }
              )
              newNftValue
          ToCaller -> mempty -- Balancing will return the token to the caller

  txDatumsRedeemerTxIns <- liftedE $ mkUnbalancedTx lookups constraints
  txWithMetadata <-
    setSeabugMetadata (modify (_ { nftId = txData.newNft }) nftData)
      (fst txData.newAsset)
      txDatumsRedeemerTxIns

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

mkChangeNftIdTxData
  :: forall (r :: Row Type)
   . String
  -> (NftId -> MintAct)
  -> (NftId -> NftId)
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

  userAddr <- liftedM (name <> ": Cannot get user addr") getWalletAddress
  userUtxos <-
    liftedM (name <> ": Cannot get user Utxos") $ utxosAt userAddr
  utxo /\ utxoIndex <-
    liftContractM (name <> ": NFT not found")
      $ Array.find containsNft
      $ toUnfoldable
      $ unwrap
      $ fromMaybe userUtxos mScriptUtxos
  let
    utxosForTx = insert utxo utxoIndex $ unwrap userUtxos
    lookups = mconcat
      [ ScriptLookups.mintingPolicy policy
      , ScriptLookups.unspentOutputs utxosForTx
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

minAdaOnlyUTxOValue :: BigInt
minAdaOnlyUTxOValue = BigInt.fromInt 2_000_000

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
  authorShareValidated <- natToShare nftCollection.authorShare
  marketplaceShareValidated <- natToShare nftCollection.daoShare
  setTxMetadata tx $ SeabugMetadata
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
