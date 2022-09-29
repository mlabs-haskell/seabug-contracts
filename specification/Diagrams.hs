{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Concurrent (forkIO)
import Control.Monad (forM_)
import Data.GraphViz
  ( GraphvizCanvas (Xlib),
    GraphvizOutput (Canon, DotOutput, Svg),
    runGraphviz,
    runGraphvizCanvas',
  )
import Family
import Family.Diagram
  ( OverlayMode (Distinct, Parallel, Serial),
    combineTransactionGraphs,
    transactionGraphToDot,
    transactionTypeGraph,
  )
import Family.Diagram.TH (diagramForTransactionType, untypedDiagramForTransactionType)
import qualified Language.Haskell.TH as TH
-- import the transaction instances
import qualified Spec

-- transaction diagrams
listing = $([t|'Spec.Listing "Alice" "BTC"|] >>= untypedDiagramForTransactionType)

underlying = $([t|'Spec.UnderlyingMint "Alice" "UTxORef" "token_name" |] >>= untypedDiagramForTransactionType)

-- rendering
main = do
  let listingGraph = transactionTypeGraph listing
      underlyingGraph = transactionTypeGraph underlying
      dots = transactionGraphToDot "diagram" <$> [listingGraph, underlyingGraph]
      distinct = transactionGraphToDot "distinct" $
        combineTransactionGraphs Distinct [listingGraph, underlyingGraph]
      parallel = transactionGraphToDot "parallel" $
        combineTransactionGraphs Parallel [listingGraph, underlyingGraph]
      serial = transactionGraphToDot "serial" $
        combineTransactionGraphs Serial [listingGraph, underlyingGraph]
  forM_ ([distinct, parallel, serial] <> dots) $ \dot -> do
    forkIO (runGraphvizCanvas' dot Xlib)
    runGraphviz dot Svg "diagram.svg"
