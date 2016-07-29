{-# LANGUAGE RecordWildCards, DataKinds #-}

module ProducerConsumer where

-- imports
import CLaSH.Prelude
import Debug.Trace
import qualified Data.List as L

import HSDFTypes
import HSDFEdge
import CSDFEdge


-- Definitions for the node functions:
f_Pr :: Cntr -> Cntr -> Vec 3 Byte
f_Pr firecounter phase = res
  where
    res = repeat (resize firecounter)

f_Cr :: Vec 3 Byte -> Cntr -> Cntr -> ()
f_Cr x firecounter phase = res
  where
    res = ()


-- Definitions for the nodes:
n_Pr :: (Cntr -> Cntr -> (a)) -> (Cntr, Cntr) -> (Bool) -> ((Cntr, Cntr), (a, Bool))
n_Pr f (firecounter, phase) (full0) = ((firecounter', phase'), (dataout0, fire))
    where
        fire = not full0
        firecounter' = if fire then firecounter + 1 else firecounter
        phase_max = 0
        phase' = if fire then (if phase < phase_max then phase + 1 else 0) else phase_max
        (dataout0) = f firecounter phase

n_PrL = mealy (n_Pr f_Pr) (0, 0)

n_Cr :: (a -> Cntr -> Cntr -> ()) -> (Cntr, Cntr) -> (a, Bool) -> ((Cntr, Cntr), (Bool))
n_Cr f (firecounter, phase) (datain0, empty0) = ((firecounter', phase'), (fire))
    where
        fire = not empty0
        firecounter' = if fire then firecounter + 1 else firecounter
        phase_max = 0
        phase' = if fire then (if phase < phase_max then phase + 1 else 0) else phase_max
        _ = f datain0 firecounter phase

n_CrL = mealy (n_Cr f_Cr) (0, 0)



-- The actual graph
graph :: Signal Bool -> Signal (Bool, Cntr, Bool, Cntr, Bool, Cntr)
graph inputs = outputs
    where
        -- Instantiations of nodes
        (n_Pr_dataout0, n_Pr_fire) = unbundle $ n_PrL n_Pr_full0
        n_Cr_fire = n_CrL $ bundle (n_Cr_datain0, n_Cr_empty0)
        
        -- Define the edges
        (e_Pr_Int_dataout, e_Pr_Int_empty, e_Pr_Int_full) = unbundle $ e_Pr_IntL $ bundle (e_Pr_Int_datain, e_Pr_Int_rd, e_Pr_Int_wrt)

        
        -- Connect the node and edge signals
        (nodeinps) = ........
        (edgeinps) = ......

        -- Output for debugging
        outputs = bundle ()


-- Simulate the circuit for 128 clock-cycles
simres = L.take 128 $ simulate graph $ L.repeat True


topEntity = graph
