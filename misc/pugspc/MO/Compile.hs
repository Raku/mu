{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances -funbox-strict-fields #-}

module MO.Compile where

import MO.Base
import MO.Util

-- A Method must have name and an implementation encapsulated in a
-- MethodCompile type. This abstraction allows having different
-- types of methods, but for now we have only SimpleMethod, which is
-- the minimal sane instance of Method.

type MethodName = ID

class Monad m => Method m a | a -> m where
    methodName      :: a -> MethodName
    methodCompile   :: a -> MethodCompiled m

instance Monad m => Method m (AnyMethod m) where
    methodName (MkMethod x)    = methodName x
    methodCompile (MkMethod x) = methodCompile x

data SimpleMethod m
    = MkSimpleMethod
        { sm_name        :: MethodName
        , sm_definition  :: MethodCompiled m
        }

instance Monad m => Method m (SimpleMethod m) where
    methodName = sm_name
    methodCompile = sm_definition


-- AnyMethod may contain any type of the Method class. This makes
-- other functions life easier. And is a common pattern in MO code.

data AnyMethod m = forall a. Method m a => MkMethod !a

-- FIXME: Its not ok to use this since we can define method with
-- same name which are different. 
instance Eq (AnyMethod m) where
    MkMethod a == MkMethod b = methodName a == methodName b

instance Ord (AnyMethod m) where
    MkMethod a `compare` MkMethod b = methodName a `compare` (methodName b)

instance Show (AnyMethod m) where
    show (MkMethod m) = show (methodName m)


-- MethodCompiled represent a method that maybe called (via runMC) with
-- an Arguments and return an Invocant. It may in the future use the
-- Codeable abstraction (see Base.hs) but for now is "just" a Haskell method.

newtype MethodCompiled m = MkMethodCompiled { runMC :: Arguments m -> m (Invocant m) }

