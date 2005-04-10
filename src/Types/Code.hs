module Types.Code where

import {-# SOURCE #-} AST
import Internals

class Class a where
    fetch    :: a -> Eval VCode
    fetch a = assuming a [] []
    store    :: a -> VCode -> Eval ()
    assuming :: a -> [Exp] -> [Exp] -> Eval VCode
    apply    :: a -> Eval Val
    assoc    :: a -> VStr
    params   :: a -> Params
