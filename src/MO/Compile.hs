{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances -funbox-strict-fields #-}

module MO.Compile where

import MO.Base
import MO.Util

type MethodName = ID

data AnyMethod m = forall a. Method m a => MkMethod !a

-- FIXME: Its not ok to use this since we can define method with
-- same name which are different. 
instance Eq (AnyMethod m) where
    MkMethod a == MkMethod b = methodName a == methodName b

instance Ord (AnyMethod m) where
    MkMethod a `compare` MkMethod b = methodName a `compare` (methodName b)

instance Show (AnyMethod m) where
    show (MkMethod m) = show (methodName m)

-- Method and friends

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


data MethodCompiled m
    = forall c. Codeable m c => MkMethodCompiled c

-- NOTE: Maybe I should instantiate MethodCompiled for Codeable? :P
runMC :: MethodCompiled m -> Arguments m -> m (Invocant m)
runMC (MkMethodCompiled c) = run c
