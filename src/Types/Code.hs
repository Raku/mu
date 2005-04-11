{-# OPTIONS_GHC -fglasgow-exts #-}

module Types.Code where

import {-# SOURCE #-} AST

class Class a where
    iType :: a -> String
    iType _ = "Code"
    fetch    :: a -> Eval VCode
    fetch a = assuming a [] []
    store    :: a -> VCode -> Eval ()
    assuming :: a -> [Exp] -> [Exp] -> Eval VCode
    apply    :: a -> Eval Val
    assoc    :: a -> VStr
    params   :: a -> Params
