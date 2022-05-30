module Seabug.Helpers
  ( jsonReader
  ) where

import Aeson
  ( class DecodeAeson
  , caseAesonObject
  , getField
  , jsonToAeson
  )
import Contract.Prelude
import Data.Argonaut
  ( Json
  , JsonDecodeError(TypeMismatch)
  )

-- | Helper to decode the local inputs such as unapplied minting policy and
-- | typed validator
jsonReader
  :: forall (a :: Type)
   . DecodeAeson a
  => String
  -> Json
  -> Either JsonDecodeError a
jsonReader field = jsonToAeson >>> caseAesonObject (Left $ TypeMismatch "Expected Object")
  (flip getField field)
