{-# OPTIONS -cpp -fglasgow-exts -fth #-}

module Internals.TH (
    module TH,
    showTH,
) where

import Language.Haskell.TH as TH
showTH :: (Ppr a) => a -> String
showTH = show . ppr
