module Main (main) where

import Contract.Prelude

import Contract.Address (NetworkId(..), ownPaymentPubKeyHash)
import Contract.Test.Plutip (PlutipConfig, runPlutipContract, withKeyWallet)
import Contract.Transaction (awaitTxConfirmed)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.UInt as UInt
import Effect.Aff (launchAff_)
import Seabug.Contract.CnftMint (mintCnft)
import Seabug.Contract.MarketPlaceListNft (marketPlaceListNft)
import Seabug.Types (MintCnftParams(..))
import Serialization.Address (addressBech32)

main :: Effect Unit
main = launchAff_ $ do
  let
    distribution :: Array BigInt /\ Array BigInt
    distribution =
      [ BigInt.fromInt 1_000_000_000
      , BigInt.fromInt 2_000_000_000
      ] /\
        [ BigInt.fromInt 2_000_000_000 ]
  runPlutipContract config distribution \(alice /\ bob) -> do
    w <- liftAff $ unwrap alice # _.address $ MainnetId
    log $ show $ addressBech32 w
    withKeyWallet alice do
      txHash /\ cnft <- mintCnft $
        MintCnftParams
          { imageUri:
              "ipfs://k2cwuebwvb6kdiwob6sb2yqnz38r0yv72q1xijbts9ep5lq3nm8rw3i4"
          , tokenNameString: "abcdef"
          , name: "Piaggio Ape"
          , description: "Seabug Testing"
          }
      awaitTxConfirmed txHash
      log <<< show =<< marketPlaceListNft
        "testnetu7qDM8q2XT1S6gEBSicUIqXB6QN60l7B"
      pure unit -- sign, balance, submit, etc.
    withKeyWallet bob do
      log <<< show =<< ownPaymentPubKeyHash
      pure unit -- sign, balance, submit, etc.

config :: PlutipConfig
config =
  { host: "127.0.0.1"
  , port: UInt.fromInt 8082
  , logLevel: Trace
  -- Server configs are used to deploy the corresponding services.
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
