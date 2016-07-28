{-# LANGUAGE RecordWildCards, DataKinds #-}

module CSDFEdge where

-- imports
import CLaSH.Prelude
import Debug.Trace
import qualified Data.List as L

import HSDFTypes

type RdPtr16 = Unsigned 4
type WrtPtr16 = Unsigned 4
type TknCntr16 = Unsigned 5
type PRate = Unsigned 4
type CRate = Unsigned 4
type RatesPtr = Unsigned 4


csdfedge :: (Vec16 a, RdPtr16, WrtPtr16, TknCntr16, RatesPtr, RatesPtr) ->
    (Vec 3 a, Bool, Bool) ->
    ((Vec16 a, RdPtr16, WrtPtr16, TknCntr16, RatesPtr, RatesPtr), (Vec 2 a, Bool, Bool, Bool))
csdfedge (elms, rptr, wptr, tkncntr, prateptr, crateptr)
    (datain, rd, wrt) = 
    ((elms', rptr', wptr', tkncntr', prateptr', crateptr'), (dataout, canread, canwrite, err))
    where
        -- Vector for production and consumption rates
        prates = 3 :> 1 :> Nil :: Vec 2 PRate
        crates = 2 :> 0 :> 2 :> Nil :: Vec 3 CRate
        -- Max value of pointer before wrap to 0
        prateptr_max = 1 :: RatesPtr
        crateptr_max = 2 :: RatesPtr
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
        dataout = take d2 $ rotateLeft elms rptr
        -- Check to prevent getting 'too empty' or 'to full':
        canread = resize cur_crate <= tkncntr
        canwrite = (tkncntr + resize cur_prate) <= 16
        err = (not canread && rd) || (not canwrite && wrt)
        -- Update the tokens based on current production rate
        indcs = 0 :> 1 :> 2 :> Nil :: Vec 3 WrtPtr16
        updates = map (<(resize wrt_diff)) indcs
        updinds = zip updates indcs -- list wether to update and which index 
        elms' = foldl condupd elms updinds
        condupd ems (upd, ind) = if upd then replace (wptr + ind) (datain !! ind) ems else ems

csdfedgeL = mealy csdfedge (repeat 0 :: Vec16 Byte, 0, 0, 0, 0, 0)

simres_a = simulate csdfedgeL [(1 :> 2 :> 3 :> Nil, False, True), (4 :> 0 :> 0 :> Nil, True, False), (0 :> 0 :> 0 :> Nil, True, False), (0 :> 0 :> 0 :> Nil, False, False), (0 :> 0 :> 0 :> Nil, False, False)]


topEntity = csdfedgeL

