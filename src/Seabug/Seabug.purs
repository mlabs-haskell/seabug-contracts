module Seabug
  ( module Seabug.CallContract
  , mint
  ) where

import Contract.Prelude

import Control.Promise (Promise)
import Data.BigInt as BigInt
import Seabug.CallContract
  ( callConnectWallet
  , callMarketPlaceBuy
  , callMarketPlaceListNft
  , callMarketPlaceFetchNft
  , callMint
  , callGetWalletBalance
  )

mint :: Effect (Promise Unit)
mint = callMint
  { serverHost: "ctl.localho.st"
  , serverPort: 8080
  , serverSecureConn: false
  , ogmiosHost: "localho.st"
  , ogmiosPort: 1337
  , ogmiosSecureConn: false
  , datumCacheHost: "localho.st"
  , datumCachePort: 9999
  , datumCacheSecureConn: false
  , networkId: 0
  , projectId: "testnetu7qDM8q2XT1S6gEBSicUIqXB6QN60l7B"
  , logLevel: "Trace"
  }
  { -- base36 ipfs cid obtained from mint-nft.sh
    imageUri: "ipfs://k2cwuebwvb6kdiwob6sb2yqnz38r0yv72q1xijbts9ep5lq3nm8rw3i4"
  , tokenNameString: "abcdef"
  , name: "Piaggio Ape"
  , description: "Seabug Testing"
  , price: BigInt.fromInt (12 * 1000000)
  }
