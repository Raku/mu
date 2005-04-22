{-# OPTIONS_GHC -fglasgow-exts #-}

module Pugs.Types.Thunk where

import {-# SOURCE #-} Pugs.AST
import Pugs.Internals
import Pugs.Types

class (Typeable a) => Class a where
    iType :: a -> Type
    iType = const $ mkType "Thunk"
    force :: a -> Eval Val
