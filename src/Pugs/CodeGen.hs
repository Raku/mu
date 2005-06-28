{-# OPTIONS_GHC -fglasgow-exts #-}

{-|
    Code generation interface.

    I sit beside the fire and think
    of all that I have seen,
    of meadow-flowers and butterflies
    in summers that have been...
-}

module Pugs.CodeGen (translate, backends) where
import Pugs.AST
import Pugs.Internals
import Pugs.CodeGen.PIL (genPIL)
import Pugs.CodeGen.PIR (genPIR)
import Pugs.Compile.Pugs (genPugs)
import Pugs.Compile.Haskell (genGHC)
import qualified Data.Map as Map

type Generator = Eval Val

generators :: Map String Generator
generators = Map.fromList $
    [ ("Ghc",         genGHC)
    , ("Parrot",      genPIR)
    , ("Pir",         genPIR)
    , ("Pil",         genPIL)
    , ("Pugs",        genPugs)
    ]

backends :: [String]
backends = Map.keys generators

norm :: String -> String
norm s = ucfirst $ map toLower s
    where ucfirst (x:xs) = toUpper x : xs
          ucfirst [] = []

doLookup :: String -> IO Generator
doLookup s = Map.lookup (norm s) generators

translate :: String -> Env -> IO String
translate s env = do
    gen <- catch (doLookup s) $ \_ -> do
        fail $ "Cannot compile to " ++ s
    VStr str <- runEvalIO env gen
    return str
