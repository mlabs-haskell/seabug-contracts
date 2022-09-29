{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Spec where

import Data.Kind (Type)
import Data.Void (Void)
import Family
import GHC.TypeLits (Symbol)
import Numeric.Natural (Natural)

data TransactionFamily
  = UnderlyingMint Symbol Symbol Symbol
  | MintWrap
  | ChangePrice
  | ChangeOwner
  | UnwrapBurn
  | Listing Symbol Symbol

data SeaBug
  = MainMintingPolicy
  | LockingValidator Symbol Natural Natural
  | MarketplaceValidator
  | NftMint Symbol

type instance DApp (t :: TransactionFamily) = SeaBug

type instance Economy SeaBug = Token

data Token
  = Ada
  | UnderlyingNFT Underlying
  | SGNFT SeabugNFT

data SeabugNFT = SeabugNFT Symbol

data Underlying = Underlying Symbol Symbol

-- * Minting policies

data MainMintRedeemer = MainMintRedeemer Symbol Natural Symbol

instance MintingPolicyScript 'MainMintingPolicy where
  type MintedToken 'MainMintingPolicy = SeabugNFT
  type MintRedeemer 'MainMintingPolicy = MainMintRedeemer

instance MintingPolicyScript ('NftMint txoutRef) where
  type MintedToken ('NftMint txoutRef) = Underlying
  type MintRedeemer ('NftMint txoutRef) = ()

-- * Validators

data LockDatum (s :: Symbol) (m :: Natural) (n :: Natural) = LockDatum

instance ValidatorScript ('LockingValidator s m n) where
  type Datum ('LockingValidator s m n) = LockDatum s m n
  type Redeemer ('LockingValidator s m n) = Void

data MarketplaceDatum = MarketplaceDatum

instance ValidatorScript 'MarketplaceValidator where
  type Datum 'MarketplaceValidator = MarketplaceDatum
  type Redeemer 'MarketplaceValidator = ()

-- * Transactions

type ListingInputs :: Symbol -> Symbol -> InputsFor SeaBug
data ListingInputs user ac script wallet = ListingInputs
  { nft :: wallet user 'Nothing '[ 'Exactly 1 ('SGNFT ('SeabugNFT ac)), 'MinimumRequiredAda ]
  }

type ListingOutputs :: Symbol -> Symbol -> OutputsFor SeaBug
data ListingOutputs user ac script wallet = ListingOutputs
  { marketplace :: script 'MarketplaceValidator 'MarketplaceDatum '[ 'Exactly 1 ('SGNFT ('SeabugNFT ac)), 'MinimumRequiredAda ]
  }

instance Transaction (Listing user ac) where
  type Inputs (Listing user ac) = ListingInputs user ac
  type Outputs (Listing user ac) = ListingOutputs user ac

type UnderlyingInputs :: Symbol -> InputsFor SeaBug
data UnderlyingInputs utxoRef script wallet = UnderlyingInputs
  { utxo :: wallet utxoRef 'Nothing '[ AnythingElse ]
  }

type UnderlyingMints :: Symbol -> Symbol -> MintsFor SeaBug
data UnderlyingMints utxoRef tokenName mp = UnderlyingMints
  { mintedNFT :: mp ('NftMint utxoRef) '() '[ 'Mint 1 ('Underlying utxoRef tokenName) ]
  }
  
type UnderlyingOutputs :: Symbol -> Symbol -> Symbol -> OutputsFor SeaBug
data UnderlyingOutputs user utxoRef tokenName script wallet = UnderlyingOutputs
  { utxo :: wallet utxoRef 'Nothing '[ 'AnythingElse ]
  , nft :: wallet user 'Nothing '[ 'Exactly 1 (UnderlyingNFT ('Underlying utxoRef tokenName)), 'MinimumRequiredAda ]
  }

instance Transaction (UnderlyingMint user utxoRef tokenName) where
  type Inputs (UnderlyingMint user utxoRef tokenName) = UnderlyingInputs utxoRef
  type Mints (UnderlyingMint user utxoRef tokenName) = UnderlyingMints utxoRef tokenName
  type Outputs (UnderlyingMint user utxoRef tokenName) = UnderlyingOutputs user utxoRef tokenName
