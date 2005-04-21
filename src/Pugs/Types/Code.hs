{-# OPTIONS_GHC -fglasgow-exts #-}

module Pugs.Types.Code where

import {-# SOURCE #-} Pugs.AST
import Pugs.Internals
import Pugs.Types

class (Typeable a) => Class a where
    iType :: a -> Type
    iType _ = mkType "Code"
    fetch    :: a -> Eval VCode
    fetch a = assuming a [] []
    store    :: a -> VCode -> Eval ()
    assuming :: a -> [Exp] -> [Exp] -> Eval VCode
    apply    :: a -> Eval Val
    assoc    :: a -> VStr
    params   :: a -> Params
