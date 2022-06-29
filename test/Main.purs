module Test.Main (main) where

import Contract.Prelude

import Contract.Monad (launchAff_)
import Test.Metadata as Metadata
import Test.Util (interpret)
import TestM (TestPlanM)

main :: Effect Unit
main = launchAff_ do
  interpret do
    unitTestPlan

unitTestPlan :: TestPlanM Unit
unitTestPlan = do
  Metadata.suite
