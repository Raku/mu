{-# OPTIONS_GHC -fglasgow-exts #-}

module Pugs.Types.Scalar where

import {-# SOURCE #-} Pugs.AST
import Pugs.Internals
import Pugs.Types

class (Typeable a) => Class a where
    iType :: a -> Type
    iType _ = mkType "Scalar"
    fetch :: a -> Eval VScalar
    store :: a -> VScalar -> Eval ()
