{-# OPTIONS_GHC -fglasgow-exts #-}

module Types.Code where

import {-# SOURCE #-} AST
import Internals
import Types

class (Typeable a) => Class a where
    iType :: a -> VStr
    iType _ = "Code"
    fetch    :: a -> Eval VCode
    fetch a = assuming a [] []
    store    :: a -> VCode -> Eval ()
    assuming :: a -> [Exp] -> [Exp] -> Eval VCode
    apply    :: a -> Eval Val
    assoc    :: a -> VStr
    params   :: a -> Params
