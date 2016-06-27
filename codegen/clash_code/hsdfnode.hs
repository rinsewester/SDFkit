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
        phase' = if fire then (if phase < phase_max then phase + 1 else 0) else phase_max
        dataout = f datain

func = id :: Byte -> Byte

hsdfnodeL = mealy (hsdfnode_1_1 func) (0, 0)





hsdfnode_0_1 :: (Cntr -> Cntr -> b) -> (Cntr, Cntr) -> (Bool) -> ((Cntr, Cntr), (b, Bool))
hsdfnode_0_1 f (firecounter, phase) (full) = ((firecounter', phase'), (dataout, fire))
    where
        fire = not full
        firecounter' = if fire then firecounter + 1 else firecounter
        phase_max = 0
        phase' = if fire then (if phase < phase_max then phase + 1 else 0) else phase_max
        dataout = f firecounter phase

func2 :: Cntr -> Cntr -> Cntr
func2 firecounter phase = res
    where
        res = firecounter

hsdfnode_0_1L = mealy (hsdfnode_0_1 func2) (0, 0)





hsdfnode_1_2 :: (a -> Cntr -> Cntr ->(b, c)) -> (Cntr, Cntr) -> (a, Bool, Bool, Bool) -> ((Cntr, Cntr), (b, c, Bool))
hsdfnode_1_2 f (firecounter, phase) (datain0, empty0, full0, full1) = ((firecounter', phase'), (dataout0, dataout1, fire))
    where
        fire = not empty0 && not full0 && not full1
        firecounter' = if fire then firecounter + 1 else firecounter
        phase_max = 0
        phase' = if fire then (if phase < phase_max then phase + 1 else 0) else phase_max
        (dataout0, dataout1) = f datain0 firecounter phase

func3 :: Cntr -> Cntr -> Cntr -> (Cntr, Cntr)
func3 datain0 firecounter phase = (datain0 * 2, datain0 * 2 + 1)

hsdfnode_1_2L = mealy (hsdfnode_1_2 func3) (0, 0)




hsdfnode_2_0 :: (a -> b -> Cntr -> Cntr -> ()) -> (Cntr, Cntr) -> (a, b, Bool, Bool) -> ((Cntr, Cntr), (Bool))
hsdfnode_2_0 f (firecounter, phase) (datain0, datain1, empty0, empty1) = ((firecounter', phase'), (fire))
    where
        fire = not empty0 && not empty1
        firecounter' = if fire then firecounter + 1 else firecounter
        phase_max = 0
        phase' = if fire then (if phase < phase_max then phase + 1 else 0) else phase_max
        _ = f datain0 datain1 firecounter phase

func4 :: Cntr -> Cntr -> Cntr -> Cntr -> ()
func4 datain0 datain1 firecounter phase = ()

hsdfnode_2_0L = mealy (hsdfnode_2_0 func4) (0, 0)




