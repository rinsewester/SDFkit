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


csdfedge8 :: (Vec8 a, RdPtr, WrPtr) -> (Vec 3 a, RdCnt, WrtCnt) -> ((Vec8 a, RdPtr, WrPtr), (Vec 3 a, Bool, Bool))
csdfedge8 (elms, rptr, wptr) (datain, rdn, wrtn) = ((elms', rptr', wptr'), (dataout, empty, full))
    where
        rptr' = rptr + resize rdn
        wptr' = wptr + resize wrtn
        dataout = take d3 $ rotateLeft elms (ptr2ind rptr)
        empty = rptr == wptr
        full = (msb rptr /= msb wptr) && (ptr2ind rptr == ptr2ind wptr)
         
        indcs = 0 :> 1 :> 2 :> Nil :: Vec 3 WrPtr
        updates = map (<(resize wrtn)) indcs
        updinds = zip updates indcs -- list wether to update and which index 
        elms' = foldl condupd elms updinds
        condupd ems (upd, ind) = if upd then replace (ptr2ind (wptr + ind)) (datain !! ind) ems else ems

csdfedge8byteL = mealy csdfedge8 (repeat 0 :: Vec 8 Byte, 0 :: WrPtr, 0 :: RdPtr)

inpdatevec1 = 11 :> 22 :> 33 :> Nil :: Vec 3 Byte
inpdatevec2 = 44 :> 55 :> 66 :> Nil :: Vec 3 Byte
inpdatevec3 = 77 :> 88 :> 99 :> Nil :: Vec 3 Byte

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

--write three values at once, then read one and two values leaving the edge empty again
simres5 = simulate csdfedge8byteL [(inpdatevec1, 0, 3), (inpdatevec2, 1, 0), (inpdatevec1, 2, 0), (inpdatevec1, 0, 0)]
--simvalid5 = (L.take 3 simres5) L.!! 3 == (11 :> 44 :> 55 :> Nil, False, False)

allSimsCorrect = simvalid1 && simvalid2 && simvalid3 && simvalid4

main1 = do
    putStrLn $ "All CSDF Edge tests correct: " L.++ (show allSimsCorrect)


type RdPtr16 = Unsigned 4
type WrtPtr16 = Unsigned 4
type TknCntr16 = Unsigned 5
type PRate = Unsigned 4
type CRate = Unsigned 4
type RatesPtr = Unsigned 4



csdfedge :: (Vec16 a, RdPtr16, WrtPtr16, TknCntr16, RatesPtr, RatesPtr) ->
    (Vec 3 a, Bool, Bool) ->
    ((Vec16 a, RdPtr16, WrtPtr16, TknCntr16, RatesPtr, RatesPtr), (Vec 2 a, Bool, Bool))
csdfedge (elms, rptr, wptr, tkncntr, prateptr, crateptr)
    (datain, rd, wrt) = 
    ((elms', rptr', wptr', tkncntr', prateptr', crateptr'), (dataout, canread, canwrite))
    where
        -- Vector for production and consumption rates
        prates = 3 :> 1 :> Nil :: Vec 2 PRate
        crates = 2 :> 0 :> 2 :> Nil :: Vec 3 CRate
        -- Max value of pointer before wrap to 0
        prateptr_max = 1
        crateptr_max = 2
        -- Current prduction/consumption rate
        cur_prate = prates !! prateptr
        cur_crate = crates !! crateptr
        -- Update the production/consumption rate pointers
        prateptr' = if wrt
                    then if prateptr < prateptr_max then prateptr + 1 else 0
                    else prateptr
        crateptr' = if rd
                    then if crateptr < crateptr_max then crateptr + 1 else 0
                    else crateptr
        -- Update the read/write pointer and tokencounter
        rd_diff = if rd then cur_crate else 0
        wrt_diff = if wrt then cur_prate else 0
        rptr' = rptr + resize rd_diff
        wptr' = wptr + resize wrt_diff
        tkncntr' = tkncntr + resize rd_diff - resize wrt_diff
        -- The output data: select the element pointed to by the readpointer and the ones after        
        dataout = take d2 $ rotateLeft elms (ptr2ind rptr)
        -- TODO: implement signals for firing rules
        canread = True
        canwrite = True
        -- Update the tokens based on current production rate
        indcs = 0 :> 1 :> 2 :> Nil :: Vec 3 WrtPtr16
        updates = map (<(resize wrt_diff)) indcs
        updinds = zip updates indcs -- list wether to update and which index 
        elms' = foldl condupd elms updinds
        condupd ems (upd, ind) = if upd then replace (ptr2ind (wptr + ind)) (datain !! ind) ems else ems

csdfedgeL = mealy csdfedge (repeat 0 :: Vec16 Byte, 0, 0, 0, 0, 0)

simres = simulate csdfedgeL [(1 :> 2 :> 3 :> Nil, False, True), (4 :> 0 :> 0 :> Nil, True, True), (4 :> 0 :> 0 :> Nil, True, False), (4 :> 0 :> 0 :> Nil, False, False), (0 :> 0 :> 0 :> Nil, False, False)]








topEntity = csdfedge8byteL

