module Types.Rule where

import {-# SOURCE #-} AST
import Internals

class RuleClass a where
    match :: a -> VStr -> Eval (MatchResult Val)
