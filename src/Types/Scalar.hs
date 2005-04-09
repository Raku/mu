module Types.Scalar where

import AST
import Internals

class (Show a) => ScalarClass a where
    fetch :: a -> Eval Val
    store :: a -> Val -> Eval ()
