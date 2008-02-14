{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances -fparr #-}

module MO.Base (module MO.Base, Invocant, stubInvocant) where
import {-# SOURCE #-} MO.Run
import Data.Maybe
import Data.Typeable
import Pugs.Internals.ID
import Pugs.Val.Capture
import GHC.PArr
import qualified Data.Map as Map

-- Codeable is an abstraction of possible different pieces of code that
-- a method may use as implementation. It's supposed to be used as member
-- of the MethodCompiled structure. A Codeable type need to have a function
-- "run" that accepts Arguments and returns some Invocant.

-- | open type to represent Code
class Monad m => Codeable m c where
    run :: c -> Arguments m -> m (Invocant m)

-- | stub code which always return the same
newtype NoCode m = NoCode (Invocant m)

instance (Typeable (NoCode m), Monad m) => Codeable m (NoCode m) where
    run (NoCode obj) _ = return obj
instance Show (NoCode m) where
    show _ = "<NoCode>"

-- | Pure code that works with any monad.
newtype PureCode = PureCode (forall m. (Typeable1 m, Monad m) => Arguments m -> Invocant m)

instance (Typeable1 m, Monad m) => Codeable m PureCode where
    run (PureCode f) a = return (f a)
instance Show PureCode where
    show _ = "<PureCode>"

-- | Real monadic primitive code.
newtype Monad m => HsCode m = HsCode (Arguments m -> m (Invocant m))

instance (Typeable1 m, Monad m) => Codeable m (HsCode m) where
    run (HsCode f) a = f a
instance Show (HsCode m) where
    show _ = "<HsCode>"


-- Arguments represents (surprise) arguments that are passed to methods,
-- right now is just a Pugs' Capture type, but could be generalized to a
-- class, in case of separating MO "generic" code from Pugs specifics.

type Arguments m = Capt (Invocant m)


-- This Invocant refers to the same concept as in Perl-esque syntax:
-- "foo $moose: $a, $b" which means "$moose.foo($a, $b)".

withInvocant :: (Typeable1 m, Monad m) => Arguments m -> Invocant m -> Arguments m
withInvocant args x = CaptMeth{ c_invocant = x, c_feeds = c_feeds args }

getInvocant :: (Typeable1 m, Monad m) => Arguments m -> Maybe (Invocant m)
getInvocant CaptMeth{ c_invocant = x }  = Just x
getInvocant _                           = Nothing

namedArg :: (Typeable1 m, Monad m) => Arguments m -> ID -> Maybe (Invocant m)
namedArg args key = foldlP findArg Nothing (c_feeds args)
    where
    -- Notice that each feed has a Map with the named arguments (given by f_nameds)
    -- and the values are of type '[:a:]' and not 'a', because of this we get only
    -- the first one. "(!: 0)" means "(!! 0)" in parallel arrays notation.
    -- (is getting only the first one right??)
    findArg Nothing MkFeed{ f_nameds = ns } = fmap (!: 0) (Map.lookup key ns)
    findArg x       _                       = x

