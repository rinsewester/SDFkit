{-# LANGUAGE RecordWildCards, DataKinds #-}

module HSDFNode where

-- imports
import CLaSH.Prelude
import Debug.Trace
import qualified Data.List as L

import HSDFTypes


hsdfnode :: (a -> b) -> (Cntr, Cntr) -> (a, Bool, Bool) -> ((Cntr, Cntr), (b, Bool))
hsdfnode f (firecounter, phase) (datain, empty, full) = ((firecounter', phase'), (dataout, fire))
    where
        fire = not empty && not full
        firecounter' = if fire then firecounter + 1 else firecounter
        phase' = phase
        dataout = f datain

func = id :: Byte -> Byte

hsdfnodeL = mealy (hsdfnode func) (0, 0)


