{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}

module MO.Base (module MO.Base, Invocant, stubInvocant) where
import {-# SOURCE #-} MO.Run
import Data.Maybe
import Data.Typeable
import MO.Util


-- | open type to represent Code
class Monad m => Code m c where
    run :: Arguments m a => c -> a -> m (Invocant m)

-- | stub code which always return the same
newtype NoCode m = NoCode (Invocant m)

instance (Typeable (NoCode m), Monad m) => Code m (NoCode m) where
    run (NoCode obj) _ = return obj
instance Show (NoCode m) where
    show _ = "<NoCode>"

-- | Pure code that works with any monad.
newtype PureCode = PureCode (forall m a. Arguments m a => a -> Invocant m)

instance Monad m => Code m PureCode where
    run (PureCode f) a = return (f a)
instance Show PureCode where
    show _ = "<PureCode>"

-- | Real monadic primitive code.
newtype Monad m => HsCode m = HsCode (forall a. Arguments m a => a -> m (Invocant m))

instance (Typeable1 m, Monad m) => Code m (HsCode m) where
    run (HsCode f) a = f a
instance Show (HsCode m) where
    show _ = "<HsCode>"


-- | don't know how arguments interface will be :P
class (Typeable1 m, Monad m, Show a) => Arguments m a | a -> m where
    withInvocant    :: a -> Invocant m -> a
    getInvocant     :: a -> Maybe (Invocant m)
    toList          :: a -> [Invocant m]
    namedArg        :: a -> String -> m (Maybe (Invocant m))

newtype ArgList m = ArgList [Invocant m]

instance (Typeable1 m, Monad m) => Arguments m (ArgList m) where 
    withInvocant (ArgList bs) b   = ArgList (b:bs)
    getInvocant (ArgList bs)      = listToMaybe bs
    toList (ArgList bs) = bs
    namedArg _ _ = return Nothing

instance Show (ArgList m) where
    show (ArgList bs) = "<arglist>"

