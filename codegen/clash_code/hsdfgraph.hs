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

-- NodeL: (datain, empty, full) -> (dataout, cons, prod)
node1L = mealy (hsdfnode f) (0, 0)
node2L = mealy (hsdfnode g) (0, 0)
node3L = mealy (hsdfnode h) (0, 0)

-- EdgeL: (datain, prod, cons) -> (dataout, empty, full)
edge12L = mealy hsdfedge8 (repeat 0 :: ByteVec, 0 :: Ptr, 0 :: Ptr)
edge23L = mealy hsdfedge8 (repeat 0 :: ByteVec, 0 :: Ptr, 0 :: Ptr)
edge31L = mealy hsdfedge8 (repeat 0 :: ByteVec, 0 :: Ptr, 1 :: Ptr)


-- An actual HSDF graph  loop ov n1,n2 and n3
graph1 :: Signal Bool -> Signal (Bool, Byte, Bool, Byte, Bool, Byte)
graph1 input = output
    where
        (e12_datain, e31_cons, e12_prod) = unbundle $ node1L $ bundle (e31_dataout, e31_empty, e12_full)
        (e23_datain, e12_cons, e23_prod) = unbundle $ node2L $ bundle (e12_dataout, e12_empty, e23_full)
        (e31_datain, e23_cons, e31_prod) = unbundle $ node3L $ bundle (e23_dataout, e23_empty, e31_full)

        (e12_dataout, e12_empty, e12_full) = unbundle $ edge12L $ bundle (e12_datain, e12_prod, e12_cons)
        (e23_dataout, e23_empty, e23_full) = unbundle $ edge23L $ bundle (e23_datain, e23_prod, e23_cons)
        (e31_dataout, e31_empty, e31_full) = unbundle $ edge31L $ bundle (e31_datain, e31_prod, e31_cons)

        output = bundle (e12_prod, e12_datain, e23_prod, e23_datain, e31_prod, e31_datain)

simres = L.take 25 $ simulate graph1 $ L.repeat True


topEntity = graph1

