{-# LANGUAGE RecordWildCards, DataKinds #-}

module CSDFEdge where

-- imports
import CLaSH.Prelude
import Debug.Trace
import qualified Data.List as L

import HSDFTypes

type WrtCnt = Unsigned 2 -- Write count: number of tokens to write in parallel
type RdCnt = Unsigned 2  -- Read count: number of tokens to read in parallel 

-- ignore the most significant bit by shifting up and down
ptr2ind ptr = shiftR (shiftL ptr 1) 1


csdfedge8 :: (Vec8 a, RdPtr, WrPtr) -> (Vec3 a, RdCnt, WrtCnt) -> ((Vec8 a, RdPtr, WrPtr), (Vec3 a, Bool, Bool))
csdfedge8 (elms, rptr, wptr) (datain, rdn, wrtn) = ((elms', rptr', wptr'), (dataout, empty, full))
    where
        rptr' = rptr + resize rdn
        wptr' = wptr + resize wrtn
        dataout = take d3 $ rotateLeft elms (ptr2ind rptr)
        empty = rptr == wptr
        full = (msb rptr /= msb wptr) && (ptr2ind rptr == ptr2ind wptr)
         
        indcs = 0 :> 1 :> 2 :> Nil :: Vec3 WrPtr
        updates = map (<(resize wrtn)) indcs
        updinds = zip updates indcs -- list wether to update and which index 
        elms' = foldl condupd elms updinds
        condupd ems (upd, ind) = if upd then replace (ptr2ind (wptr + ind)) (datain !! ind) ems else ems



csdfedge8byteL = mealy csdfedge8 (repeat 0 :: Vec 8 Byte, 0 :: WrPtr, 0 :: RdPtr)

inpdatevec1 = 11 :> 22 :> 33 :> Nil :: Vec3 Byte
inpdatevec2 = 44 :> 55 :> 66 :> Nil :: Vec3 Byte
inpdatevec3 = 77 :> 88 :> 99 :> Nil :: Vec3 Byte

-- Dont write at all: should leave the output onchanged
simres1 = simulate csdfedge8byteL [(inpdatevec1, 0, 0), (inpdatevec1, 0, 0)]
expres1 = [(0 :> 0 :> 0 :> Nil, True, False), (0 :> 0 :> 0 :> Nil, True, False)]
simvalid1 = L.take 2 simres1 == expres1

-- write single value into edge
simres2 = simulate csdfedge8byteL [(inpdatevec1, 0, 1), (inpdatevec1, 0, 0)]
expres2 = [(0 :> 00 :> 00 :> Nil, True, False), (11 :> 0 :> 0 :> Nil, False, False)]
simvalid2 = L.take 2 simres2 == expres2

-- write three values into edge
simres3 = simulate csdfedge8byteL [(inpdatevec1, 0, 3), (inpdatevec1, 0, 0)]
expres3 = [(0 :> 00 :> 00 :> Nil, True, False), (11 :> 22 :> 33 :> Nil, False, False)]
simvalid3 = L.take 2 simres3 == expres3

-- write first one value then two more into the edge
simres4 = simulate csdfedge8byteL [(inpdatevec1, 0, 1), (inpdatevec2, 0, 2), (inpdatevec1, 0, 0)]
simvalid4 = (L.take 3 simres4) L.!! 2 == (11 :> 44 :> 55 :> Nil, False, False)



allSimsCorrect = simvalid1 && simvalid2 && simvalid3 && simvalid4

main = do
    putStrLn $ "All CSDF Edge tests correct: " L.++ (show allSimsCorrect)

topEntity = csdfedge8byteL

