{-# OPTIONS_GHC -cpp -fglasgow-exts -fno-warn-orphans -funbox-strict-fields #-}

module Pugs.AST.Scope (
    Scope(..),
) where

-- | The scope of a variable declaration.
data Scope = SState  -- ^ Persistent across calls
           | SLet    -- ^ Hypotheticalised (reverted upon failure)
           | STemp   -- ^ Temporary (reverted at scope exit)
           | SMy     -- ^ Lexical
           | SOur    -- ^ Package
           | SGlobal -- ^ Global
    deriving (Show, Eq, Ord, Enum)
