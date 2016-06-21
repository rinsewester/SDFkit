{-# LANGUAGE RecordWildCards, DataKinds #-}

module HSDFNode where

-- imports
import CLaSH.Prelude
import Debug.Trace
import qualified Data.List as L

--type definitions
type Cntr = Unsigned 16
type Byte = Unsigned 8



hsdfnode :: (a -> b) -> (Cntr, Cntr) -> (a, Bool, Bool) -> ((Cntr, Cntr), (b, Bool, Bool))
hsdfnode f (firecounter, phase) (datain, empty, full) = ((firecounter', phase'), (dataout, cons, prod))
    where
        fire = not empty && not full
        firecounter' = if fire then firecounter + 1 else firecounter
        phase' = phase
        dataout = f datain
        (cons, prod) = (fire, fire)

func = id :: Byte -> Byte

hsdfnodeL = mealy (hsdfnode func) (0, 0)




--topEntity = 

