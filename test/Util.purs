module Test.Util
  ( interpret
  , interpretWithConfig
  ) where

import Contract.Prelude

import Data.Const (Const)
import Mote (Plan, foldPlan, planT)
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpec')
import Test.Spec.Runner as SpecRunner
import TestM (TestPlanM)

-- | We use `mote` here so that we can use effects to build up a test tree, which
-- | is then interpreted here in a pure context, mainly due to some painful types
-- | in Test.Spec which prohibit effects.
interpret :: TestPlanM Unit -> Aff Unit
interpret = interpretWithConfig defaultConfig { timeout = Just (wrap 10000.0) }

interpretWithConfig :: SpecRunner.Config -> TestPlanM Unit -> Aff Unit
interpretWithConfig config spif = do
  plan <- planT spif
  runSpec' config [ consoleReporter ] $ planToSpec plan

planToSpec :: Plan (Const Void) (Aff Unit) -> Spec Unit
planToSpec =
  foldPlan
    (\x -> it x.label $ liftAff x.value)
    pending
    (\x -> describe x.label $ planToSpec x.value)
    sequence_
