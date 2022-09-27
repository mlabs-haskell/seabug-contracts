module Seabug.Contract.MarketPlaceSell
  ( marketPlaceSell
  ) where

import Contract.Prelude

import Contract.Address (getWalletAddress)
import Contract.Monad (Contract, liftedE, liftedM)
import Contract.PlutusData (toData)
import Contract.ScriptLookups as Lookups
import Contract.Transaction (TransactionHash, balanceAndSignTxE, submit)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , Value
  , singleton
  , valueOf
  )
import Data.BigInt (fromInt)
import Effect.Exception (throw)
import Seabug.MarketPlace (marketplaceValidator)
import Seabug.Types (MarketplaceDatum(..))

-- | List the given collection NFT on the marketplace
marketPlaceSell
  :: forall (r :: Row Type)
   . CurrencySymbol /\ TokenName
  -> Contract r TransactionHash
marketPlaceSell (curr /\ tn) = do
  addr <- liftedM "Cannot get address" getWalletAddress
  utxos <- liftedM "Cannot get user utxos" $ utxosAt addr
  marketplaceValidator' <- unwrap <$> marketplaceValidator

  let
    hasToken :: Value -> Boolean
    hasToken v = valueOf v curr tn == fromInt 1

    callerHasToken :: Boolean
    callerHasToken = any (unwrap >>> _.amount >>> hasToken) $ unwrap utxos

    nftValue :: Value
    nftValue = singleton curr tn one

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.unspentOutputs (unwrap utxos)

    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustPayToScript marketplaceValidator'.validatorHash
        ( wrap $ toData $ MarketplaceDatum $
            { getMarketplaceDatum: curr /\ tn }
        )
        nftValue

  unless callerHasToken $ liftEffect $ throw "Missing token"

  unbalancedTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  signedTx <- liftedE $ balanceAndSignTxE unbalancedTx
  transactionHash <- submit signedTx
  log $ "Sell transaction successfully submitted with hash: " <> show
    transactionHash
  pure transactionHash
