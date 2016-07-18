{-# LANGUAGE RecordWildCards, DataKinds #-}

module <GRAPH_NAME> where

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
<GRAPH_TYPE>
graph inputs = outputs
    where
        -- Instantiations of nodes
<NODE_INSTANCES>

        -- Define the edges
<EDGE_INSTANCES>
        
        -- Connect the node and edge signals
<GRAPH_CONNECTIONS>

        -- Output for debugging
        outputs = bundle <GRAPH_OUTPUTS>


-- Simulate the circuit for 128 clock-cycles
simres = L.take 128 $ simulate graph $ L.repeat True


topEntity = graph
