module Seabug
  ( module Seabug.CallContract
  , module QueryM.Utxos
  , main
  ) where

import Contract.Prelude

import Control.Promise (Promise)
import Data.BigInt as BigInt
import QueryM.Utxos (getWalletBalance)
import Seabug.CallContract
  ( callMarketPlaceBuy
  , callMarketPlaceBuyTest
  , callMarketPlaceListNft
  , callMint
  )

main :: Effect (Promise Unit)
main = callMint
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
    imageUri: "ipfs://k2cwuee3arxg398hwxx6c0iferxitu126xntuzg8t765oo020h5y6npn"
  , tokenNameString: "abcdef"
  , name: "Allium"
  , description: "Seabug Testing"
  , price: BigInt.fromInt 10
  }
