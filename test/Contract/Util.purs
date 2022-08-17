module Test.Contract.Util
  ( ContractWrapAssertion
  , assertContract
  , assertLovelaceChangeAtAddr
  , assertLovelaceDecAtAddr
  , assertLovelaceIncAtAddr
  , callMintCnft
  , callMintSgNft
  , checkBalanceChangeAtAddr
  , checkNftAtAddress
  , findUtxoWithNft
  , mintParams1
  , mintParams2
  , mintParams3
  , mintParams4
  , plutipConfig
  , privateStakeKey1
  , privateStakeKey2
  , privateStakeKey3
  , valueAtAddress
  , valueToLovelace
  , withAssertions
  ) where

import Contract.Prelude

import Contract.Address (Address, Slot(..))
import Contract.Config (PrivateStakeKey)
import Contract.Monad (Contract, liftedM)
import Contract.Numeric.Natural as Nat
import Contract.Test.Plutip (PlutipConfig)
import Contract.Transaction (TransactionOutput(..), awaitTxConfirmed)
import Contract.Utxos (utxosAt)
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , Value
  , getLovelace
  , valueOf
  , valueToCoin
  )
import Contract.Wallet (privateKeyFromBytes)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Map as Map
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (ala)
import Data.UInt as UInt
import Effect.Exception (throw)
import Partial.Unsafe (unsafePartial)
import Seabug.Contract.CnftMint (mintCnft)
import Seabug.Contract.Mint (mintWithCollection')
import Seabug.Contract.Util (modify)
import Seabug.Types (MintCnftParams(..), MintParams, NftData)
import Types.BigNum as BigNum
import Types.RawBytes (hexToRawBytes)

mintParams1 :: MintParams
mintParams1 = wrap
  { authorShare: Nat.fromInt' 1000
  , daoShare: Nat.fromInt' 1000
  , price: Nat.fromInt' $ 100 * 1000000
  , lockLockup: BigInt.fromInt 5
  , lockLockupEnd: Slot $ BigNum.fromInt 5
  , feeVaultKeys: []
  }

mintParams2 :: MintParams
mintParams2 = modify (_ { daoShare = Nat.fromInt' 10 }) mintParams1

mintParams3 :: MintParams
mintParams3 = modify (_ { authorShare = Nat.fromInt' 10 }) mintParams1

mintParams4 :: MintParams
mintParams4 = modify
  (_ { daoShare = Nat.fromInt' 10, authorShare = Nat.fromInt' 10 })
  mintParams1

callMintCnft
  ∷ forall (r :: Row Type). Contract r (CurrencySymbol /\ TokenName)
callMintCnft = do
  log "Minting cnft..."
  txHash /\ cnft <- mintCnft $
    MintCnftParams
      { imageUri:
          "ipfs://k2cwuebwvb6kdiwob6sb2yqnz38r0yv72q1xijbts9ep5lq3nm8rw3i4"
      , tokenNameString: "abcdef"
      , name: "Piaggio Ape"
      , description: "Seabug Testing"
      }
  log $ "Waiting for confirmation of cnft transaction: " <> show txHash
  awaitTxConfirmed txHash
  log $ "Cnft transaction confirmed: " <> show txHash
  log $ "Minted cnft: " <> show cnft
  pure cnft

callMintSgNft
  :: forall (r :: Row Type)
   . Tuple CurrencySymbol TokenName
  -> MintParams
  -> Contract r ((CurrencySymbol /\ TokenName) /\ NftData)
callMintSgNft cnft mintParams = do
  log "Minting sgNft..."
  sgNftTxHash /\ sgNft /\ nftData <- mintWithCollection' cnft mintParams
  log $ "Waiting for confirmation of nft transaction: " <> show
    sgNftTxHash
  awaitTxConfirmed sgNftTxHash
  log $ "Nft transaction confirmed: " <> show sgNftTxHash
  pure $ sgNft /\ nftData

plutipConfig :: PlutipConfig
plutipConfig =
  { host: "127.0.0.1"
  , port: UInt.fromInt 8082
  , logLevel: Error
  , ogmiosConfig:
      { port: UInt.fromInt 1338
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , ogmiosDatumCacheConfig:
      { port: UInt.fromInt 10000
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , ctlServerConfig:
      { port: UInt.fromInt 8083
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , postgresConfig:
      { host: "127.0.0.1"
      , port: UInt.fromInt 5433
      , user: "ctxlib"
      , password: "ctxlib"
      , dbname: "ctxlib"
      }
  }

privateStakeKeyFromStr :: String -> PrivateStakeKey
privateStakeKeyFromStr s = wrap $ unsafePartial $ fromJust
  $ privateKeyFromBytes
  =<< hexToRawBytes s

privateStakeKey1 :: PrivateStakeKey
privateStakeKey1 = privateStakeKeyFromStr
  "633b1c4c4a075a538d37e062c1ed0706d3f0a94b013708e8f5ab0a0ca1df163d"

privateStakeKey2 :: PrivateStakeKey
privateStakeKey2 = privateStakeKeyFromStr
  "8ad4245e25152bbd9de44257c7a2a5f625d92f43ae54ae74716e6ad58e32d42e"

privateStakeKey3 :: PrivateStakeKey
privateStakeKey3 = privateStakeKeyFromStr
  "caff25cdb2c64d8edd4405ca62fa1d1641545890d3f1eb52be44317056216126"

assertContract :: forall (r :: Row Type). String -> Boolean -> Contract r Unit
assertContract msg cond = if cond then pure unit else liftEffect $ throw msg

checkNftAtAddress
  :: forall (r :: Row Type)
   . (CurrencySymbol /\ TokenName)
  -> Address
  -> Contract r Boolean
checkNftAtAddress nft addr = isJust <$> findUtxoWithNft nft addr

findUtxoWithNft
  :: forall (r :: Row Type)
   . (CurrencySymbol /\ TokenName)
  -> Address
  -> Contract r (Maybe TransactionOutput)
findUtxoWithNft (nftCs /\ nftTn) addr = do
  utxos <- liftedM "Could not get utxos" $ map unwrap <$> utxosAt addr
  pure $ find
    ( \(TransactionOutput { amount }) ->
        valueOf amount nftCs nftTn == one
    )
    utxos

valueToLovelace :: Value -> BigInt
valueToLovelace = getLovelace <<< valueToCoin

valueAtAddress :: forall (r :: Row Type). Address -> Contract r (Maybe Value)
valueAtAddress address = utxosAt address <#> map
  (fold <<< map _.amount <<< map unwrap <<< Map.values <<< unwrap)

-- | `checkBalanceChangeAtAddr addrName addr check contract` returns
-- | the result of passing to `check` the total value at the address
-- | `addr` (named `addrName`) before and after calling `contract`.
checkBalanceChangeAtAddr
  :: forall (r :: Row Type) (a :: Type) (b :: Type)
   . String
  -> Address
  -> (Value -> Value -> Contract r b)
  -> Contract r a
  -> Contract r b
checkBalanceChangeAtAddr addrName addr check contract = do
  valueBefore <- liftedM ("Could not get " <> addrName <> " value before") $
    valueAtAddress addr
  void $ contract
  valueAfter <- liftedM ("Could not get " <> addrName <> " value after") $
    valueAtAddress addr
  check valueBefore valueAfter

-- | `assertLovelaceChangeAtAddr addrName addr expected comp contract`
-- | requires the predicate `comp actual expected` to succeed, where
-- | `actual` is the lovelace at `addr` after `contract` minus the
-- | lovelace before.
assertLovelaceChangeAtAddr
  :: forall (r :: Row Type) (a :: Type)
   . String
  -> Address
  -> BigInt
  -> (BigInt -> BigInt -> Boolean)
  -> Contract r a
  -> Contract r Unit
assertLovelaceChangeAtAddr addrName addr expected comp contract =
  flip (checkBalanceChangeAtAddr addrName addr) contract \valBefore valAfter ->
    do
      let actual = valueToLovelace valAfter - valueToLovelace valBefore
      assertContract
        ( "Unexpected lovelace change at addr " <> addrName
            <> "\n expected="
            <> show expected
            <> "\n actual="
            <> show actual
        )
        $ comp actual expected

-- | Requires that at least the passed amount of lovelace was gained
-- | at the address by calling the contract.
assertLovelaceIncAtAddr
  :: forall (r :: Row Type) (a :: Type)
   . String
  -> Address
  -> BigInt
  -> Contract r a
  -> Contract r Unit
assertLovelaceIncAtAddr addrName addr minGain contract =
  assertLovelaceChangeAtAddr addrName addr minGain (>=) contract

-- | Requires that at least the passed amount of lovelace was lost at
-- | the address by calling the contract.
assertLovelaceDecAtAddr
  :: forall (r :: Row Type) (a :: Type)
   . String
  -> Address
  -> BigInt
  -> Contract r a
  -> Contract r Unit
assertLovelaceDecAtAddr addrName addr minLoss contract =
  assertLovelaceChangeAtAddr addrName addr (negate minLoss) (<=) contract

type ContractWrapAssertion (r :: Row Type) = Contract r Unit -> Contract r Unit

-- | Composes assertions to be run with a contract.
withAssertions
  :: forall (r :: Row Type) (a :: Type)
   . Array (ContractWrapAssertion r)
  -> Contract r a
  -> Contract r Unit
withAssertions assertions contract = ala Endo foldMap assertions (void contract)
