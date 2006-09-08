{-# OPTIONS_GHC -fglasgow-exts -fallow-overlapping-instances #-}

module Pugs.Val where

import Pugs.Internals
import qualified Data.ByteString as Buf

data Val
data ValNative
    = NBit      !NativeBit
    | NInt      !NativeInt
    | NUint     !NativeUint
    | NBuf      !NativeBuf
    | NNum      !NativeNum
    | NComplex  !NativeComplex

type NativeBit      = Bool
type NativeInt      = Int
type NativeUint     = Word
type NativeBuf      = ByteString
type NativeNum      = Float
newtype NativeComplex = MkNComplex { unComplex :: Complex NativeNum }

type P = Identity
type Table = Map ID Val
newtype Pad = MkPad { padEntries :: Map Var PadEntry }
data PadEntry
data Var -- XXX: should be moved to from Val to AST definiton
--data Stmt
