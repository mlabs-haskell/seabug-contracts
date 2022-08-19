module Test.Contract.Util
  ( BasicAssertion
  , BasicAssertionMaker
  , ContractWrapAssertion
  , assertContract
  , assertLovelaceChangeAtAddr
  , assertLovelaceDecAtAddr
  , assertLovelaceDecAtAddr'
  , assertLovelaceIncAtAddr
  , assertLovelaceIncAtAddr'
  , callMintCnft
  , callMintSgNft
  , checkBalanceChangeAtAddr
  , checkNftAtAddress
  , class WrappingAssertion
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
  , wrapAndAssert
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

-- | Get the total value at an address. Throws an exception if this
-- | fails.
valueAtAddress :: forall (r :: Row Type). String -> Address -> Contract r Value
valueAtAddress name address =
  liftedM ("Could not get " <> name <> " address value") $ utxosAt address <#>
    map (fold <<< map _.amount <<< map unwrap <<< Map.values <<< unwrap)

-- | `checkBalanceChangeAtAddr addrName addr check contract` returns
-- | the result of passing to `check` the total value at the address
-- | `addr` (named `addrName`) before and after calling `contract`.
checkBalanceChangeAtAddr
  :: forall (r :: Row Type) (a :: Type) (b :: Type)
   . String
  -> Address
  -> (a -> Value -> Value -> Contract r b)
  -> Contract r a
  -> Contract r b
checkBalanceChangeAtAddr addrName addr check contract = do
  valueBefore <- valueAtAddress addrName addr
  res <- contract
  valueAfter <- valueAtAddress addrName addr
  check res valueBefore valueAfter

-- | `assertLovelaceChangeAtAddr addrName addr expected comp contract`
-- | requires the predicate `comp actual expected` to succeed, where
-- | `actual` is the lovelace at `addr` after `contract` minus the
-- | lovelace before.
assertLovelaceChangeAtAddr
  :: forall (r :: Row Type) (a :: Type)
   . String
  -> Address
  -> (a -> Contract r BigInt)
  -> (BigInt -> BigInt -> Boolean)
  -> Contract r a
  -> Contract r a
assertLovelaceChangeAtAddr addrName addr getExpected comp contract =
  flip (checkBalanceChangeAtAddr addrName addr) contract
    \res valBefore valAfter ->
      do
        let actual = valueToLovelace valAfter - valueToLovelace valBefore
        expected <- getExpected res
        assertContract
          ( "Unexpected lovelace change at addr " <> addrName
              <> "\n expected=\t"
              <> show expected
              <> "\n actual=\t"
              <> show actual
          )
          $ comp actual expected
        pure res

-- | Requires that at least the computed amount of lovelace was gained
-- | at the address by calling the contract.
assertLovelaceIncAtAddr
  :: forall (r :: Row Type) (a :: Type)
   . String
  -> Address
  -> (a -> Contract r BigInt)
  -> Contract r a
  -> Contract r a
assertLovelaceIncAtAddr addrName addr getMinGain contract =
  assertLovelaceChangeAtAddr addrName addr getMinGain (>=) contract

-- | Requires that at least the passed amount of lovelace was gained
-- | at the address by calling the contract.
assertLovelaceIncAtAddr'
  :: forall (r :: Row Type) (a :: Type)
   . String
  -> Address
  -> BigInt
  -> Contract r a
  -> Contract r a
assertLovelaceIncAtAddr' addrName addr minGain contract =
  assertLovelaceIncAtAddr addrName addr (const $ pure minGain) contract

-- | Requires that at least the computed amount of lovelace was lost at
-- | the address by calling the contract.
assertLovelaceDecAtAddr
  :: forall (r :: Row Type) (a :: Type)
   . String
  -> Address
  -> (a -> Contract r BigInt)
  -> Contract r a
  -> Contract r a
assertLovelaceDecAtAddr addrName addr getMinLoss contract =
  assertLovelaceChangeAtAddr addrName addr (map negate <<< getMinLoss) (<=)
    contract

-- | Requires that at least the passed amount of lovelace was lost at
-- | the address by calling the contract.
assertLovelaceDecAtAddr'
  :: forall (r :: Row Type) (a :: Type)
   . String
  -> Address
  -> BigInt
  -> Contract r a
  -> Contract r a
assertLovelaceDecAtAddr' addrName addr minLoss contract =
  assertLovelaceDecAtAddr addrName addr (const (pure minLoss)) contract

-- | An assertion that can control when the contract is run. The
-- | assertion inhabiting this type should not call the contract more
-- | than once, as other assertions need to be able to make this
-- | assumption to succesfully compose.
type ContractWrapAssertion r a = Contract r a -> Contract r a

-- | An assertion that only needs the result of the contract.
type BasicAssertion r a b = a -> Contract r b

type BasicAssertionMaker r a b = a -> Array (Contract r b)

-- | Class to unify different methods of making assertions about a
-- | contract under a single interface. Note that the typechecker may
-- | need some help when using this class; try providing type
-- | annotations for your assertions using the type aliases for the
-- | instances of this class.
class WrappingAssertion f r a where
  -- | Wrap a contract in an assertion. The wrapped contract itself
  -- | becomes a contract which can be wrapped, allowing for
  -- | composition of assertions.
  -- |
  -- | No guarantees are made about the order in which assertions are
  -- | made. Assertions with side effects should not be used.
  wrapAndAssert :: Contract r a -> f -> Contract r a

instance WrappingAssertion (ContractWrapAssertion r a) r a where
  wrapAndAssert contract assertion = assertion contract
else instance WrappingAssertion (BasicAssertionMaker r a b) r a where
  wrapAndAssert contract assertionMaker = contract >>= \r ->
    sequence_ (assertionMaker r) *> pure r
else instance WrappingAssertion (BasicAssertion r a b) r a where
  wrapAndAssert contract assertion = contract >>= \r -> assertion r *> pure r

instance WrappingAssertion (Array (ContractWrapAssertion r a)) r a where
  wrapAndAssert contract assertions = ala Endo foldMap assertions contract
else instance WrappingAssertion (Array (BasicAssertion r a b)) r a where
  wrapAndAssert contract assertions = contract >>= \r ->
    traverse_ (_ $ r) assertions *> pure r

instance
  ( WrappingAssertion f r a
  , WrappingAssertion g r a
  ) =>
  WrappingAssertion (f /\ g) r a where
  wrapAndAssert contract (assert1 /\ assert2) =
    wrapAndAssert (wrapAndAssert contract assert1) assert2

-- | `wrapAndAssert` flipped
withAssertions
  :: forall (r :: Row Type) (a :: Type) (assertions :: Type)
   . WrappingAssertion assertions r a
  => assertions
  -> Contract r a
  -> Contract r a
withAssertions = flip wrapAndAssert
