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


-- NodeL: (datain, empty, full) -> (dataout, fire)
node1L = mealy (hsdfnode_1_1 f) (0, 0)
node2L = mealy (hsdfnode_1_1 g) (0, 0)

-- EdgeL: (datain, rd, wrt) -> (dataout, empty, full)
edge12L = mealy hsdfedge8 (repeat 0 :: ByteVec, 0 :: RdPtr, 1 :: WrPtr)
edge21L = mealy hsdfedge8 (repeat 0 :: ByteVec, 0 :: RdPtr, 0 :: WrPtr)


-- An actual HSDF graph  loop ov n1,n2 and n3
graph1 :: Signal Bool -> Signal (Bool, Byte)
graph1 input = output
    where
        -- Define the nodes
        (n1_dataout, n1_fire) = unbundle $ node1L $ bundle (n1_datain, n1_empty, n1_full)
        (n2_dataout, n2_fire) = unbundle $ node2L $ bundle (n2_datain, n2_empty, n2_full)

        -- Define the edges
        (e12_dataout, e12_empty, e12_full) = unbundle $ edge12L $ bundle (e12_datain, e12_rd, e12_wrt)
        (e21_dataout, e21_empty, e21_full) = unbundle $ edge21L $ bundle (e21_datain, e21_rd, e21_wrt)

        -- Connect the node and edge signals
        (n1_datain, n1_empty, n1_full) = (e21_dataout, e21_empty, e12_full)
        (n2_datain, n2_empty, n2_full) = (e12_dataout, e12_empty, e21_full)
        (e12_datain, e12_rd, e12_wrt) = (n1_dataout, n2_fire, n1_fire)
        (e21_datain, e21_rd, e21_wrt) = (n2_dataout, n1_fire, n2_fire)

        -- Output for debugging
        output = bundle (n1_fire, n1_dataout)

simres = L.take 25 $ simulate graph1 $ L.repeat True


topEntity = graph1

