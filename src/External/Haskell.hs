{-# OPTIONS -fglasgow-exts -cpp -O #-}

module External.Haskell where
import AST
import Internals
import External.Haskell.NameLoader

loadHaskell :: FilePath -> IO [(String, [Val] -> Eval Val)]
loadHaskell file = undefined

{- do
    initializeRuntimeLoader
    mod <- loadObject file
    resolveFunctions
    syms <- loadFunction mod "extern"
    return $ map (\(name, fun) -> (('&':name), fun)) syms
-}
