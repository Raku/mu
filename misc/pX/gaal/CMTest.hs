module Main where

{- build with:
 - util/drift.pl src/Pugs/AST/CapInternals.hs
 - util/drift.pl src/Pugs/MOP.hs
 - ghc -package-conf third-party/installed/packages.conf -isrc -O0 --make misc/pX/gaal/CMTest.hs src/pcre/pcre.o
 -}
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as Str
import DrIFT.Perl6Class

import Pugs.AST.CapInternals
import Pugs.AST.CapInternals.Instances
import Pugs.MOP
import Pugs.MOP.Instances


type Str = Str.ByteString


main :: IO ()
main = do
    putStrLn $ asPerl6Object $ classMeta

