module Types.Code where

import AST
import Internals

class (Show a) => CodeClass a where
    assuming :: CodeClass b => a -> [Exp] -> [Exp] -> Eval b
    apply    :: a -> Eval Val
