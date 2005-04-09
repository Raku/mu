module Types.Scalar where

import {-# SOURCE #-} AST
import Internals

class ScalarClass a where
    fetch :: a -> Eval Val
    store :: a -> Val -> Eval ()
