module Seabug
  ( module Seabug.CallContract
  , getWalletBalance
  ) where

import Prelude (bind, ($))

import Control.Promise (Promise, fromAff)
import Data.Maybe (Maybe)
import Effect (Effect)
import QueryM (callNami)
import Seabug.CallContract
  ( callMarketPlaceBuy
  , callMarketPlaceBuyTest
  , callMarketPlaceListNft
  )
import Serialization.Types (Value)
import Wallet (Wallet(..), mkNamiWalletAff)

getWalletBalance :: Effect (Promise (Maybe Value))
getWalletBalance = fromAff $ do
  (Nami nami) <- mkNamiWalletAff
  callNami nami _.getBalance
