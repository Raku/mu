{-# OPTIONS -cpp -fglasgow-exts -fth #-}

module Internals.TH (
    module TH,
    showTH,
) where

#if __GLASGOW_HASKELL__ < 604
import Language.Haskell.THSyntax as TH
showTH :: [Dec] -> String
showTH decl = "$(returnQ " ++ show decl ++ ")"
#else
import Language.Haskell.TH as TH
showTH :: (Ppr a) => a -> String
showTH = show . ppr
#endif

