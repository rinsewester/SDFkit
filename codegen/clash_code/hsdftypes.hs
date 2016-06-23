{-# LANGUAGE RecordWildCards, DataKinds #-}

module HSDFTypes where

-- imports
import CLaSH.Prelude
import qualified Data.List as L


-- Scalar types
type Byte = Unsigned 8
type Cntr = Unsigned 16
type Nibble = Unsigned 4
type RdPtr = Unsigned 4
type WrPtr = Unsigned 4


-- Vector types
type ByteVec = Vec 8 Byte
type Vec8 a = Vec 8 a
