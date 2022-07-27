module Seabug.Contract.Common
  ( NftResult
  ) where

import Contract.Transaction (TransactionInput, TransactionOutput)
import Seabug.Metadata (FullSeabugMetadata)

type NftResult =
  { input :: TransactionInput
  , output :: TransactionOutput
  , metadata :: FullSeabugMetadata
  }
