{-# OPTIONS_GHC -cpp -fglasgow-exts #-}

module Compile.Parrot where

data ParrotContext
    = CtxUnknown | CtxBool | CtxVoid
    | CtxInt | CtxNum | CtxStr
    | CtxPMC | CtxKey

data ParrotNode = NodeConst
    { nodeContext   :: ParrotContext
    , noteUpContext :: ParrotContext
    }
