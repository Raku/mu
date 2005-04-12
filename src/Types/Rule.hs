{-# OPTIONS_GHC -fglasgow-exts #-}

module Types.Rule where

import {-# SOURCE #-} AST
import Internals
import Types

class (Typeable a) => Class a where
    iType :: a -> VStr
    iType _ = "Rule"
    fetch :: a -> Eval VRule
    store :: a -> VRule -> Eval ()
    match :: a -> VStr -> Eval (MatchResult Val)
