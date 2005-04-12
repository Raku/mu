{-# OPTIONS_GHC -fglasgow-exts #-}

module Types.Scalar where

import {-# SOURCE #-} AST
import Internals
import Types

class (Typeable a) => Class a where
    iType :: a -> VStr
    iType _ = "Scalar"
    fetch :: a -> Eval VScalar
    store :: a -> VScalar -> Eval ()
