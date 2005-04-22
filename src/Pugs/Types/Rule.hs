{-# OPTIONS_GHC -fglasgow-exts #-}

module Pugs.Types.Rule where

import {-# SOURCE #-} Pugs.AST
import Pugs.Internals
import Pugs.Types

class (Typeable a) => Class a where
    iType :: a -> Type
    iType = const $ mkType "Rule"
    fetch :: a -> Eval VRule
    store :: a -> VRule -> Eval ()
    match :: a -> VStr -> Eval (MatchResult Val)
