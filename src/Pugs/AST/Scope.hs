{-# OPTIONS_GHC -cpp -fglasgow-exts -fno-warn-orphans -funbox-strict-fields #-}

module Pugs.AST.Scope (
    Scope(..),
) where

-- | The scope of a variable declaration.
data Scope = SGlobal -- ^ Global
           | SMy     -- ^ Local
           | SOur    -- ^ Package
           | SLet    -- ^ Hypotheticalised (reverted upon failure)
           | STemp   -- ^ Temporary (reverted at scope exit)
           | SState  -- ^ Persistent across calls
    deriving (Show, Eq, Ord, Read, Enum)
