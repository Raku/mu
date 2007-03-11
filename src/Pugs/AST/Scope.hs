{-# OPTIONS_GHC -cpp -fglasgow-exts -fno-warn-orphans -funbox-strict-fields #-}

module Pugs.AST.Scope (
    Scope(..),
) where
import Data.Typeable

-- | The scope of a variable declaration.
data Scope = SMy        -- ^ Ordinary lexically scoped variable
           | SConstant  -- ^ Lexically scoped alias to package variable
           | SHas       -- ^ Object attribute
           | SState     -- ^ Persistent lexical (cloned with closures)
           | SOur       -- ^ Lexically scoped compile-time constant
    deriving (Show, Eq, Ord, Enum, Typeable, Bounded)
