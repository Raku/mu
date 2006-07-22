module Main where

-- ghc -package-conf third-party/installed/packages.conf -isrc -O0 --make misc/pX/gaal/SimpleTest.hs

import DrIFT.Perl6Class

import P
import P.Instances

main :: IO ()
main = do
    putStrLn $ showPerl6TypeDef (undefined :: Simple)
    putStrLn $ showPerl6TypeDef (undefined :: PosAttr)
    putStrLn $ showPerl6TypeDef (undefined :: RecAttr)

