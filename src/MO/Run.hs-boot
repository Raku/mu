{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances -fno-warn-orphans #-}
module MO.Run where
import Data.Typeable
data (Typeable1 m, Monad m) => Invocant m
stubInvocant :: (Typeable1 m, Monad m) => Invocant m
instance (Typeable1 m, Monad m) => Show (Invocant m)
