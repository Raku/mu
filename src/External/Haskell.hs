{-# OPTIONS -fglasgow-exts -cpp -O #-}
{-# INCLUDE "Linker.h" #-}

module External.Haskell where
import AST
import Internals
import Internals.RuntimeLoader

loadHaskell :: FilePath -> IO [(String, [Val] -> Eval Val)]
loadHaskell file = do
    initializeRuntimeLoader
    mod <- loadObject file
    resolveFunctions
    syms <- loadFunction mod "extern"
    return $ map (\(name, fun) -> (('&':name), fun)) syms
