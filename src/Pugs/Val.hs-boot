{-# OPTIONS_GHC -fglasgow-exts #-}

module Pugs.Val (
    Val, ValNative(..), PureBit, PureInt, PureNum, PureList, P
) where
import Pugs.Internals

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
type NativeComplex  = () -- Complex NativeNum 

type PureBit = Bool
data PureInt
data PureNum
data PureList

type P = Identity
