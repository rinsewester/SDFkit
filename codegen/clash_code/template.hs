{-# LANGUAGE RecordWildCards, DataKinds #-}

module Template where

-- imports
import CLaSH.Prelude
import Debug.Trace
import qualified Data.List as L

import HSDFTypes
import HSDFEdge


-- Definitions for the node functions:
<NODE_FUNC_DEFS>

-- Definitions for the nodes:
<NODE_DEFS>

-- Definiton of edges
<EDGE_DEFS>

-- The actual graph 
graph :: <GRAPH_TYPE>
graph inputs = outputs
    where
        -- Instantiations of nodes
        <NODE_INSTANCES>

        ---- Define the edges
        <EDGE_INSTANCES>
        
        ---- Connect the node and edge signals
        <GRAPH_CONNECTIONS>

        -- Output for debugging
        output = bundle <GRAPH_OUTPUTS>


simres = L.take 25 $ simulate graph $ L.repeat True


topEntity = graph
