{-# OPTIONS_GHC -fglasgow-exts #-}

module PIL.Syn where

{-

To represent "print 1" as a syntax tree, we need:

- function application
- variable lookup
- literals

-}

data Syn = MkSyn deriving Show
