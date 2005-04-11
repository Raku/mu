{-# OPTIONS_GHC -fglasgow-exts #-}

module Types.Rule where

import {-# SOURCE #-} AST
import Internals

class Class a where
    iType :: a -> String
    iType _ = "Rule"
    fetch :: a -> Eval VRule
    store :: a -> VRule -> Eval ()
    match :: a -> VStr -> Eval (MatchResult Val)
