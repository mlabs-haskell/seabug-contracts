module Test.Main (main) where

import Contract.Prelude

import Contract.Monad (launchAff_)
import Test.Metadata as Metadata
import Test.Util (interpret)
import TestM (TestPlanM)

main :: Effect Unit
main = launchAff_ $ interpret unitTestPlan

unitTestPlan :: TestPlanM Unit
unitTestPlan = Metadata.suite
