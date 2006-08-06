{-# OPTIONS_GHC -fglasgow-exts #-}

module Pugs.Val (
    Val(..), ValUndef, ValNative, ValPure, ValMut, ValIO, Id
) where
import Pugs.Internals

data Val
    = VUndef  !ValUndef   -- ^ Values that defeat type constraints (ValId = 0)
    | VNative !ValNative  -- ^ Values that can fit into an UArray  (ValId = boxed value)
    | VPure   !ValPure    -- ^ Values that are immutable           (ValId = itself)
    | VMut    !ValMut     -- ^ In-memory mutable structures        (ValId = memory addr)
    | VIO     !ValIO      -- ^ Input/Ouput handles                 (ValId = impl. dep.)

data ValUndef
data ValNative
data ValPure
data ValMut
data ValIO

type Id = ValPure
