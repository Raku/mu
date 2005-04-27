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
import Pugs.Internals
import Pugs.Compile.Pugs (genPugs)
import Pugs.Compile.Parrot (genIMC)
import Pugs.Compile.Haskell (genGHC)

compile :: String -> Env -> IO String
compile "Haskell" env = fmap vCast $ runEval env genGHC
compile "Pugs"    env = fmap vCast $ runEval env genPugs
compile "Parrot"  env = fmap vCast $ runEval env genIMC
compile s _ = fail $ "Cannot compile to " ++ s

