{-# LANGUAGE RecordWildCards, DataKinds #-}

module HSDFGraph where

-- imports
import CLaSH.Prelude
import Debug.Trace
import qualified Data.List as L

import HSDFTypes
import HSDFEdge
import HSDFNode


-- Combinational functions used in the nodes
f :: Byte -> Byte
f = id 
g :: Byte -> Byte
g x = x + 1
h :: Byte -> Byte
h = id


node1L = mealy (hsdfnode f) (0, 0)
node2L = mealy (hsdfnode g) (0, 0)
node3L = mealy (hsdfnode h) (0, 0)

edge12L = mealy hsdfedge8 (repeat 0 :: ByteVec, 0 :: Ptr, 0 :: Ptr)
