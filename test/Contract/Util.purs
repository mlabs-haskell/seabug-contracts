module Test.Contract.Util
  ( assertContract
  , callMintCnft
  , callMintSgNft
  , checkNftAtAddress
  , findUtxoWithNft
  , mintParams1
  , plutipConfig
  , privateStakeKey
  ) where

import Contract.Prelude

import Contract.Address (Address, Slot(..))
import Contract.Config (PrivateStakeKey)
import Contract.Monad (Contract, liftedM)
import Contract.Numeric.Natural as Nat
import Contract.Test.Plutip (PlutipConfig)
import Contract.Transaction (TransactionOutput(..), awaitTxConfirmed)
import Contract.Utxos (utxosAt)
import Contract.Value (CurrencySymbol, TokenName, valueOf)
import Contract.Wallet (privateKeyFromBytes)
import Data.BigInt as BigInt
import Data.UInt as UInt
import Effect.Exception (throw)
import Partial.Unsafe (unsafePartial)
import Seabug.Contract.CnftMint (mintCnft)
import Seabug.Contract.Mint (mintWithCollection')
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
