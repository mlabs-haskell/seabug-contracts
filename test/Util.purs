module Test.Util
  ( assertContract
  , checkNftAtAddress
  , findUtxoWithNft
  , plutipConfig
  , privateStakeKey
  ) where

import Contract.Prelude

import Contract.Address (Address)
import Contract.Config (PrivateStakeKey)
import Contract.Monad (Contract, liftedM)
import Contract.Test.Plutip (PlutipConfig)
import Contract.Transaction (TransactionOutput(..))
import Contract.Utxos (utxosAt)
import Contract.Value (CurrencySymbol, TokenName, valueOf)
import Contract.Wallet (privateKeyFromBytes)
import Data.UInt as UInt
import Effect.Exception (throw)
import Partial.Unsafe (unsafePartial)
import Types.RawBytes (hexToRawBytes)

plutipConfig :: PlutipConfig
plutipConfig =
  { host: "127.0.0.1"
  , port: UInt.fromInt 8082
  , logLevel: Trace
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

privateStakeKey :: PrivateStakeKey
privateStakeKey = wrap $ unsafePartial $ fromJust
  $ privateKeyFromBytes
  =<< hexToRawBytes
    "633b1c4c4a075a538d37e062c1ed0706d3f0a94b013708e8f5ab0a0ca1df163d"

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
