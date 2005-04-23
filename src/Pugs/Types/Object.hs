{-# OPTIONS_GHC -fglasgow-exts #-}

module Pugs.Types.Object where

import Pugs.Internals
import Pugs.Types

class (Typeable a) => Class a where
    iType :: a -> Type
