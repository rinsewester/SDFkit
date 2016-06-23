{-# LANGUAGE RecordWildCards, DataKinds #-}

module HSDFNode where

-- imports
import CLaSH.Prelude
import Debug.Trace
import qualified Data.List as L

import HSDFTypes


hsdfnode_1_1 :: (a -> b) -> (Cntr, Cntr) -> (a, Bool, Bool) -> ((Cntr, Cntr), (b, Bool))
hsdfnode_1_1 f (firecounter, phase) (datain, empty, full) = ((firecounter', phase'), (dataout, fire))
    where
        fire = not empty && not full
        firecounter' = if fire then firecounter + 1 else firecounter
        phase_max = 0
        phase' = if fire then (if phase<phase_max then phase + 1 else 0) else phase_max
        dataout = f datain

func = id :: Byte -> Byte

hsdfnodeL = mealy (hsdfnode_1_1 func) (0, 0)





hsdfnode_0_1 :: (Cntr -> Cntr -> b) -> (Cntr, Cntr) -> (Bool) -> ((Cntr, Cntr), (b, Bool))
hsdfnode_0_1 f (firecounter, phase) (full) = ((firecounter', phase'), (dataout, fire))
    where
        fire = not full
        firecounter' = if fire then firecounter + 1 else firecounter
        phase_max = 0
        phase' = if fire then (if phase<phase_max then phase + 1 else 0) else phase_max
        dataout = f firecounter phase

func2 :: Cntr -> Cntr -> Cntr
func2 firecounter phase = firecounter

hsdfnode_0_1L = mealy (hsdfnode_0_1 func2) (0, 0)




