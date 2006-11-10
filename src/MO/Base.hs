{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}

module MO.Base (module MO.Base, Invocant, stubInvocant) where
import {-# SOURCE #-} MO.Run
import Data.Maybe
import Data.Typeable
import MO.Util


-- | open type to represent Code
class Monad m => Code m c where
    run :: c -> Arguments m -> m (Invocant m)

-- | stub code which always return the same
newtype NoCode m = NoCode (Invocant m)

instance (Typeable (NoCode m), Monad m) => Code m (NoCode m) where
    run (NoCode obj) _ = return obj
instance Show (NoCode m) where
    show _ = "<NoCode>"

-- | Pure code that works with any monad.
newtype PureCode = PureCode (forall m. (Typeable1 m, Monad m) => Arguments m -> Invocant m)

instance (Typeable1 m, Monad m) => Code m PureCode where
    run (PureCode f) a = return (f a)
instance Show PureCode where
    show _ = "<PureCode>"

-- | Real monadic primitive code.
newtype Monad m => HsCode m = HsCode (Arguments m -> m (Invocant m))

instance (Typeable1 m, Monad m) => Code m (HsCode m) where
    run (HsCode f) a = f a
instance Show (HsCode m) where
    show _ = "<HsCode>"

withInvocant :: (Typeable1 m, Monad m) => Arguments m -> Invocant m -> Arguments m
withInvocant (MkArguments xs) x = MkArguments (x:xs)

getInvocant :: (Typeable1 m, Monad m) => Arguments m -> Maybe (Invocant m)
getInvocant (MkArguments xs) = listToMaybe xs

toList :: (Typeable1 m, Monad m) => Arguments m -> [Invocant m]
toList (MkArguments xs) = xs

namedArg :: (Typeable1 m, Monad m) => Arguments m -> String -> Maybe (Invocant m)
namedArg _ _ = Nothing

newtype Arguments m = MkArguments [Invocant m] deriving (Show)
