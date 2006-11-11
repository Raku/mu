{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}

module MO.Compile where

import MO.Base
import MO.Util

data AnyMethod m
    = forall a. Method m a => AnyMethod a

-- FIXME: Its not ok to use this since we can define method with
-- same name which are different. 
instance Eq (AnyMethod m) where
    AnyMethod a == AnyMethod b = (methodName a) == (methodName b)

instance Ord (AnyMethod m) where
    AnyMethod a `compare` AnyMethod b = (methodName a) `compare` (methodName b)

instance Show (AnyMethod m) where
    show (AnyMethod m) = show (methodName m)

-- Method and friends

class Monad m => Method m a | a -> m where
    methodName      :: a -> String
    methodCompile   :: a -> MethodCompiled m

instance Monad m => Method m (AnyMethod m) where
    methodName (AnyMethod x)    = methodName x
    methodCompile (AnyMethod x) = methodCompile x

data SimpleMethod m
    = MkSimpleMethod
        { smName        :: String
        , smDefinition  :: MethodCompiled m
        }

instance Monad m => Method m (SimpleMethod m) where
    methodName = smName
    methodCompile = smDefinition


data MethodCompiled m
    = forall c. Code m c => MkMethodCompiled c

-- NOTE: Maybe I should instantiate MethodCompiled for Code? :P
runMC :: MethodCompiled m -> Arguments m -> m (Invocant m)
runMC (MkMethodCompiled c) = run c
