{-# OPTIONS -cpp -fglasgow-exts -fth #-}

module Compile.Parrot where

#if __GLASGOW_HASKELL__ < 604
import Language.Haskell.THSyntax
#else
import Language.Haskell.Syntax
#endif

genPIR _ = error "..."
