{-# LANGUAGE RecordWildCards, DataKinds #-}

module SplittingGraph where

-- imports
import CLaSH.Prelude
import Debug.Trace
import qualified Data.List as L

import HSDFTypes
import HSDFEdge
import HSDFNode




-- Nodes: hsdfnode_0_1L, hsdfnode_1_2L and hsdfnode_2_0L
-- hsdfnode_0_1 f (full) = (dataout, fire)
-- hsdfnode_1_2 f (datain0, empty0, full0, full1) = (dataout0, dataout1, fire)
-- hsdfnode_2_0 f (datain0, datain1, empty0, empty1) = (fire)


-- Edges: (datain, rd, wrt) -> (dataout, empty, full)
edge12L = mealy hsdfedge8 (repeat 0 :: Vec8 Cntr, 0 :: RdPtr, 0 :: WrPtr)
edge23tL = mealy hsdfedge8 (repeat 0 :: Vec8 Cntr, 0 :: RdPtr, 0 :: WrPtr)
edge23bL = mealy hsdfedge8 (repeat 0 :: Vec8 Cntr, 0 :: RdPtr, 0 :: WrPtr)



-- The actual graph 
graph :: Signal Bool -> Signal (Bool, Cntr, Bool, Cntr, Bool, Cntr)
graph input = output
    where
        -- Define the nodes
        (n0_dataout, n0_fire)               = unbundle $ hsdfnode_0_1L n0_full 
        (n1_dataout0, n1_dataout1, n1_fire) = unbundle $ hsdfnode_1_2L $ bundle (n1_datain0, n1_empty0, n1_full0, n1_full1)
        n2_fire                             = hsdfnode_2_0L $ bundle (n2_datain0, n2_datain1, n2_empty0, n2_empty1)

        ---- Define the edges
        (e12_dataout, e12_empty, e12_full)      = unbundle $ edge12L $ bundle (e12_datain, e12_rd, e12_wrt)
        (e23t_dataout, e23t_empty, e23t_full)   = unbundle $ edge23tL $ bundle (e23t_datain, e23t_rd, e23t_wrt)
        (e23b_dataout, e23b_empty, e23b_full)   = unbundle $ edge23bL $ bundle (e23b_datain, e23b_rd, e23b_wrt)
        

        ---- Connect the node and edge signals
        n0_full = e12_full
        (n1_datain0, n1_empty0, n1_full0, n1_full1)     = (e12_dataout, e12_empty, e23t_full, e23b_full)
        (n2_datain0, n2_datain1, n2_empty0, n2_empty1)  = (e23t_dataout, e23b_dataout, e23t_empty, e23b_empty)
        (e12_datain, e12_rd, e12_wrt)       = (n0_dataout, n1_fire, n0_fire)
        (e23t_datain, e23t_rd, e23t_wrt)    = (n1_dataout0, n2_fire, n1_fire)
        (e23b_datain, e23b_rd, e23b_wrt)    = (n1_dataout1, n2_fire, n1_fire)

        -- Output for debugging
        output = bundle (n0_fire, n0_dataout, n1_fire, n1_dataout0, n1_fire, n1_dataout1)


simres = L.take 25 $ simulate graph $ L.repeat True


topEntity = graph

