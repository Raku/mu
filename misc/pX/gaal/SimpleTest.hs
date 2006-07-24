module Main where

-- ghc -package-conf third-party/installed/packages.conf -isrc -O0 --make misc/pX/gaal/SimpleTest.hs

import DrIFT.Perl6Class

import SimpleMod
import SimpleMod.Instances

main :: IO ()
main = do
    gop (undefined :: Simple)
    gop (undefined :: PosAttr)
    gop (undefined :: RecAttr)

    gom (undefined :: Simple)
    gom (undefined :: PosAttr)
    gom (undefined :: RecAttr)

gop :: Perl6Class a => a -> IO ()
gop = putStrLn . showPerl6TypeDef

gom :: MooseClass a => a -> IO ()
gom = putStrLn . showMooseTypeDef


