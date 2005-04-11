{-# OPTIONS_GHC -fglasgow-exts #-}

module Types.Scalar where

import {-# SOURCE #-} AST

class Class a where
    iType :: a -> String
    iType _ = "Scalar"
    fetch :: a -> Eval VScalar
    store :: a -> VScalar -> Eval ()
