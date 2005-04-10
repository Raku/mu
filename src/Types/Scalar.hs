{-# OPTIONS_GHC -fglasgow-exts #-}

module Types.Scalar where

import {-# SOURCE #-} AST

class Class a where
    fetch :: a -> Eval VScalar
    store :: a -> VScalar -> Eval ()
