{-# OPTIONS_GHC -fglasgow-exts #-}

module Pugs.Val (Val(..), ValId) where
import Data.Generics (Data, Typeable)
import Pugs.AST.CapInternals hiding (Val)

{-|
    Perl 6 Values.

>   There beryl, pearl, and opal pale,
>   And metal wrought like fishes' mail,
>   Buckler and corslet, axe and sword,
>   And shining spears were laid in hoard...
-}

-- | 'Val' represents what an unconstrained scalar container can hold.
data Val
    = VUndef  !ValUndef   -- ^ Values that defeat type constraints (ValId = 0)
    | VNative !ValNative  -- ^ Values that can fit into an UArray  (ValId = boxed value)
    | VPure   !ValPure    -- ^ Values that are immutable           (ValId = itself)
    | VMut    !ValMut     -- ^ In-memory mutable structures        (ValId = memory addr)
    | VIO     !ValIO      -- ^ Input/Ouput handles                 (ValId = impl. dep.)
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

-- | 'ValId' is an unique ID that distinguishes two @Val@s of the same type from each other.
type ValId = ValPure

