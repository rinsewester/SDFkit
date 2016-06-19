{-# LANGUAGE RecordWildCards, DataKinds #-}

module HSDFEdge where

-- imports
import CLaSH.Prelude
import Debug.Trace

--type definitions
type Byte = Unsigned 8
type Elm = Byte
type ElmVec = Vec 8 Elm
type Ptr = Unsigned 4

-- ignore the most significant bit by shifting up and down
ptr2ind ptr = shiftR (shiftL ptr 1) 1


hsdfedge :: (ElmVec, Ptr, Ptr) -> (Elm, Bool, Bool) -> ((ElmVec, Ptr, Ptr), (Elm, Bool, Bool, ElmVec))
hsdfedge (elms, rptr, wptr) (datain, prod, cons) = ((elms', rptr', wptr'), (dataout, empty, full, elms'))
    where
        elms' = if cons then replace (ptr2ind wptr) datain elms else elms
        rptr' = if cons then rptr + 1 else rptr
        wptr' = if prod then wptr + 1 else wptr
        dataout = elms !! (ptr2ind rptr)
        empty = rptr == wptr
        full = (msb rptr /= msb wptr) && (ptr2ind rptr == ptr2ind wptr)


hsdfedgeL = mealy hsdfedge (repeat 0, 0, 0)

write_sim_res = simulate hsdfedgeL [(99, False, True), (99, False, False), (99, False, False)]