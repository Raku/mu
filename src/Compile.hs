{-# OPTIONS -fglasgow-exts #-}

{-
    Compiler interface.

    And words unheard were spoken then
    of folk and Men and Elven-kin,
    beyond the world were visions showed
    forbid to those that dwell therein...
-}

module Compile where
import Compile.Parrot (genPIR)
import Compile.Haskell (genGHC)

compile "Parrot" = genPIR
compile "Haskell" = genGHC
compile s = \_ -> error $ "Cannot compile to " ++ s
