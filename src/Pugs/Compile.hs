{-# OPTIONS_GHC -fglasgow-exts #-}

{-|
    Compiler interface.

>   And words unheard were spoken then
>   Of folk and Men and Elven-kin,
>   Beyond the world were visions showed
>   Forbid to those that dwell therein...
-}

module Pugs.Compile where
import Pugs.AST
import Pugs.Internals
import Pugs.Compile.PIR (genPIR)
import Pugs.Compile.Pugs (genPugs)
import Pugs.Compile.Haskell (genGHC)

compile :: String -> Env -> IO String
compile "GHC"     env = fmap vCast $ runEvalIO env genGHC
compile "Ghc"     env = fmap vCast $ runEvalIO env genGHC
compile "Haskell" env = fmap vCast $ runEvalIO env genGHC
compile "Parrot"  env = fmap vCast $ runEvalIO env genPIR
compile "Pir"     env = fmap vCast $ runEvalIO env genPIR
compile "PIR"     env = fmap vCast $ runEvalIO env genPIR
compile "Pugs"    env = fmap vCast $ runEvalIO env genPugs
compile s _ = fail $ "Cannot compile to " ++ s

