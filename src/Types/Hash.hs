module Types.Hash where

import AST
import Internals

type Index = String

class HashClass a where
    fetch       :: a -> Index -> Eval Val
    store       :: a -> Index -> Val -> Eval ()
    delete      :: a -> Index -> Eval ()
    clear       :: a -> Eval ()
    firstKey    :: a -> Eval Index
    nextKey     :: a -> Index -> Eval Index
    isEmpty     :: a -> Eval VBool

