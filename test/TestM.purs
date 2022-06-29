module TestM
  ( TestPlanM
  ) where

import Prelude
import Data.Const (Const)
import Effect.Aff (Aff)
import Mote (MoteT)

type TestPlanM a = MoteT (Const Void) (Aff Unit) Aff a
