module Test.Util
  ( interpret
  ) where

import Prelude

import Data.Const (Const)
import Data.Foldable (sequence_)
import Data.Maybe (Maybe(Just))
import Data.Newtype (wrap)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Mote (Plan, foldPlan, planT)
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpec')
import TestM (TestPlanM)

interpret :: TestPlanM Unit -> Aff Unit
interpret spif = do
  plan <- planT spif
  runSpec' defaultConfig { timeout = Just (wrap 10000.0) } [ consoleReporter ] $
    go plan
  where
  go :: Plan (Const Void) (Aff Unit) -> Spec Unit
  go =
    foldPlan
      (\x -> it x.label $ liftAff x.value)
      pending
      (\x -> describe x.label $ go x.value)
      sequence_
