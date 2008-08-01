{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans -funbox-strict-fields -fallow-overlapping-instances #-}
module Pugs.AST where
import Pugs.Internals
import Pugs.Types
import Pugs.Class hiding (Val)
import {-# SOURCE #-} Pugs.AST.Internals

(./) :: ((:>:) Call a) => Val -> a -> Eval Val
instance ((:>:) Call) Cxt
