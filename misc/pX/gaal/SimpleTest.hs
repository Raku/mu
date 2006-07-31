module Main where

-- ghc -package-conf third-party/installed/packages.conf -isrc -O0 --make misc/pX/gaal/SimpleTest.hs

import DrIFT.Perl6Class

import SimpleMod
import SimpleMod.Instances

main :: IO ()
main = do
    objDemo
    gop (undefined :: Simple)
    gop (undefined :: PosAttr)
    gop (undefined :: RecAttr)

    gom (undefined :: Simple)
    gom (undefined :: PosAttr)
    gom (undefined :: RecAttr)

gop :: Perl6Class a => a -> IO ()
gop = putStrLn . showPerl6TypeDef ("v6::AST::" ++)

gom :: MooseClass a => a -> IO ()
gom = putStrLn . showMooseTypeDef ("v6::AST::" ++)

objDemo :: IO ()
objDemo = do
    putStrLn $ asPerl6Object Variant2
    putStrLn $ asPerl6Object $ PosClass 17 [42, 54] "Moose"
    putStrLn $ asPerl6Object $ MkAltRec [[1], [2]] 3.1427 "Elk"
    putStrLn $ asPerl6Object $ PosClass 17 [42] "Moose"
