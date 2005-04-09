module Types.Rule where

import AST
import Internals

class (Show a) => RuleClass a where
    match :: a -> VStr -> Eval (MatchResult Val)
