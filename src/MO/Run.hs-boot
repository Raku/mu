{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}
module MO.Run where
import Data.Typeable
data (Typeable1 m, Monad m) => Invocant m
stubInvocant :: (Typeable1 m, Monad m) => Invocant m
