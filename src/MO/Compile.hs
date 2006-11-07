{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}

module MO.Compile where

import MO.Base
import MO.Util

data AnyMethod m = forall a. Method m a => AnyMethod { anyMethod :: a }

-- FIXME: Its not ok to use this since we can define method with
-- same name which are different. 
instance Eq (AnyMethod m) where
    AnyMethod a == AnyMethod b = (name a) == (name b)

instance Ord (AnyMethod m) where
    AnyMethod a `compare` AnyMethod b = (name a) `compare` (name b)

instance Show (AnyMethod m) where
    show (AnyMethod m) = show (name m)

-- Method and friends

class Monad m => Method m a | a -> m where
    name    :: a -> String
    compile :: a -> MethodCompiled m

instance Monad m => Method m (AnyMethod m) where
    name (AnyMethod x)    = name x
    compile (AnyMethod x) = compile x

data SimpleMethod m
    = MkSimpleMethod
        { smName        :: String
        , smDefinition  :: MethodCompiled m
        }

instance Monad m => Method m (SimpleMethod m) where
    name = smName
    compile = smDefinition


data MethodCompiled m
    = forall c. Code m c => MkMethodCompiled { mcBody :: c }

-- NOTE: Maybe I should instantiate MethodCompiled for Code? :P
runMC :: Arguments m a => MethodCompiled m -> a -> m (Invocant m)
runMC MkMethodCompiled { mcBody = c } = run c


-- OLD STUFF DOWN HERE

-- -- TODO: How to do the coercing stuff? :P
-- -- just instance MethodDefinition Code(Ref) ?
-- class MethodDefinition a
--     
-- data AnyMethodDefinition = forall a. MethodDefinition a => AnyMethodDefinition a



-- instance MethodDefinition MethodCompiled




