module Types.Code where

import {-# SOURCE #-} AST
import Internals

class CodeClass a where
    assuming :: a -> [Exp] -> [Exp] -> Eval VSub
    apply    :: a -> Eval Val
    assoc    :: a -> VStr
    params   :: a -> Params
