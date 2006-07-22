module Main where

-- ghc -package-conf third-party/installed/packages.conf -isrc -O0 --make misc/pX/gaal/CITest.hs

import DrIFT.Perl6Class

import Pugs.AST.CapInternals
import Pugs.AST.CapInternals.Instances

main :: IO ()
main = do
    putStrLn $ showPerl6TypeDef (undefined :: Code)

