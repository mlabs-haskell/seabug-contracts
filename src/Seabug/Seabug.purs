module Seabug
  ( module Seabug.CallContract
  , getWalletBalance
  )
  where

-- import Prelude

-- import Control.Promise (Promise, fromAff)
-- import Data.Maybe (Maybe)
-- import Effect (Effect)
-- import QueryM (callNami)
import Seabug.CallContract
-- import Serialization.Types (Value)
-- import Wallet (Wallet(..), mkNamiWalletAff)

-- connectWallet :: Effect (Promise Wallet)
-- connectWallet = fromAff mkNamiWalletAff

getWalletBalance :: Effect (Promise (Maybe Value))
getWalletBalance = fromAff $ do
  (Nami nami) <- mkNamiWalletAff
  callNami nami _.getBalance

