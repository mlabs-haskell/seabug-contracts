module Test.Plutip (main) where

import Contract.Prelude

import Contract.Monad (launchAff_)
import Test.Contract.Buy as Buy
import Test.Contract.Minting as Minting
import Test.Spec.Runner (defaultConfig)
import Test.Util (interpretWithConfig)
import TestM (TestPlanM)

-- Run with `spago test --main Test.Plutip`
main :: Effect Unit
main = launchAff_ $ interpretWithConfig
  -- we don't want to exit because we need to clean up after failure by
  -- timeout (something likely to happen with plutip tests)
  defaultConfig { timeout = Just $ wrap 30_000.0, exit = false }
  plutipTestPlan

plutipTestPlan :: TestPlanM Unit
plutipTestPlan = do
  Minting.suite
  Buy.suite
