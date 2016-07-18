{-# LANGUAGE RecordWildCards, DataKinds #-}

module HSDFTypes where

-- imports
import CLaSH.Prelude
import qualified Data.List as L


-- Scalar types
type Cntr = Unsigned 16

type Nibble = Unsigned 4
type Byte = Unsigned 8

type UInt8 = Unsigned 8
type UInt16 = Unsigned 16
type UInt32 = Unsigned 32
type UInt64 = Unsigned 64

type Int8 = Signed 8
type Int16 = Signed 16
type Int32 = Signed 32
type Int64 = Signed 64


-- Pointer types for the FIFOs
type RdPtr = Unsigned 4
type WrPtr = Unsigned 4


-- Vector types
type ByteVec = Vec 8 Byte
type Vec8 a = Vec 8 a
