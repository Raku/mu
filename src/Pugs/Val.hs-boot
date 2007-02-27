{-# OPTIONS_GHC -fglasgow-exts -fallow-overlapping-instances #-}

module Pugs.Val where
import Pugs.Class
import Pugs.AST.Eval

type Val = Invocant Eval

{-
newtype Pad = MkPad { padEntries :: Map Var PadEntry }
type Table = Map ID Val
data PadEntry

instance Show Pad
instance Show Val

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
instance Typeable1 P
--data Stmt
-}
