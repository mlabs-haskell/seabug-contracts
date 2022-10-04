module Seabug
  ( mint
  , module Seabug.CallContract
  , sell
  ) where

import Contract.Prelude

import Control.Promise (Promise)
import Data.BigInt as BigInt
import Seabug.CallContract
  ( ContractConfiguration
  , callConnectWallet
  , callGetWalletBalance
  , callMarketPlaceBuy
  , callMarketPlaceFetchNft
  , callMarketPlaceListNft
  , callMarketPlaceSell
  , callMint
  )

mint :: Effect (Promise Unit)
mint = callMint config
  { -- base36 ipfs cid obtained from upload-image.sh
    imageUri: "ipfs://k2cwueahicvck9req1x9ej93oq6y71wtez2tnr10g4a9wl0mwy7wz9r5"
  , tokenNameString: "abcdef"
  , name: "Bee"
  , description: "From pixabay"
  , price: BigInt.fromInt (120 * 1000000)
  }

sell :: Effect (Promise Unit)
sell = callMarketPlaceSell config
  { tokenCS: "a1ab229c7e657eb0f258bb78fe01efbb4c06d5490540734a0400b844"
  , tokenName:
      "e3b8d0618070674104fa8dcd4ed57c7595e21b648aa83b93e22af5590f6fe8a8"
  }

config :: ContractConfiguration
config =
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
  , projectId: "previewoXa2yw1U0z39X4VmTs6hstw4c6cPx1LN"
  , logLevel: "Trace"
  }
