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
import Pugs.Compile.Pugs (genPugs)
import Pugs.Compile.Pugs2 (genPugs2)
import Pugs.Compile.Parrot (genPIR)
import Pugs.Compile.PIR (genPIR')
import Pugs.Compile.Haskell (genGHC)

compile :: String -> Env -> IO String
compile "Haskell" env = fmap vCast $ runEvalIO env genGHC
compile "Pugs"    env = fmap vCast $ runEvalIO env genPugs
compile "Pugs2"   env = fmap vCast $ runEvalIO env genPugs2
compile "Parrot"  env = fmap vCast $ runEvalIO env genPIR
compile "Pir"     env = fmap vCast $ runEvalIO env genPIR'
compile "PIR"     env = fmap vCast $ runEvalIO env genPIR'
compile s _ = fail $ "Cannot compile to " ++ s

