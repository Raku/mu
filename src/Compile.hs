{-# OPTIONS_GHC -fglasgow-exts #-}

{-
    Compiler interface.

    And words unheard were spoken then
    Of folk and Men and Elven-kin,
    Beyond the world were visions showed
    Forbid to those that dwell therein...
-}

module Compile where
import Compile.Pugs (genPugs)
import Compile.Parrot (genPIR)
import Compile.Haskell (genGHC)

compile "Haskell" = genGHC
compile "Pugs" = genPugs
compile "Parrot" = genPIR
compile s = const (error $ "Cannot compile to " ++ s)

