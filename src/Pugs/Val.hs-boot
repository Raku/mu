{-# OPTIONS_GHC -fglasgow-exts #-}

module Pugs.Val (Val) where
import Data.Generics (Data, Typeable)

data Val

instance Show       Val
instance Eq         Val
instance Ord        Val
instance Data       Val
instance Typeable   Val

{-
instance Show       ValId
instance Eq         ValId
instance Ord        ValId
instance Data       ValId
instance Typeable   ValId
-}
