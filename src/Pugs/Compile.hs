{-# OPTIONS_GHC -fglasgow-exts #-}

{-
    Compiler interface.

    And words unheard were spoken then
    Of folk and Men and Elven-kin,
    Beyond the world were visions showed
    Forbid to those that dwell therein...
-}

module Pugs.Compile where
import Pugs.AST
import Pugs.Compile.Pugs (genPugs)
import Pugs.Compile.Parrot (genPIR)
import Pugs.Compile.Haskell (genGHC)

compile :: String -> Env -> IO String
compile "Haskell" = genGHC
compile "Pugs" = genPugs
compile "Parrot" = genPIR
compile s = const (error $ "Cannot compile to " ++ s)

