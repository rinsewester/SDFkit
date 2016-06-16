{-# LANGUAGE RecordWildCards, DataKinds #-}

module HSDFEdge where

-- imports
import CLaSH.Prelude

--type definitions
type Byte = Unsigned 8


hsdfedge :: Byte -> Byte -> (Byte, Byte)
hsdfedge s i = (s', o)
    where
        o = s
        s' = i

hsdfedgeL = mealy hsdfedge 0



