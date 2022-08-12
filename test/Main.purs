module Test.Main (main) where

import Contract.Prelude

import Contract.Monad (launchAff_)
import Data.Const (Const)
import Mote (Plan, foldPlan, planT)
import Test.Metadata as Metadata
import Test.Minting as Minting
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpec')
import TestM (TestPlanM)

main :: Effect Unit
main = launchAff_ $ interpret do
  integrationTestPlan
  unitTestPlan

unitTestPlan :: TestPlanM Unit
unitTestPlan = Metadata.suite

integrationTestPlan :: TestPlanM Unit
integrationTestPlan = Minting.suite

interpret :: TestPlanM Unit -> Aff Unit
interpret spif = do
  plan <- planT spif
  runSpec' defaultConfig { timeout = Just (wrap 30000.0) } [ consoleReporter ] $
    go plan
  where
  go :: Plan (Const Void) (Aff Unit) -> Spec Unit
  go =
    foldPlan
      (\x -> it x.label $ liftAff x.value)
      pending
      (\x -> describe x.label $ go x.value)
      sequence_
