{-# OPTIONS_GHC -fglasgow-exts #-}

{-|
    Code generation interface.

>   I sit beside the fire and think
>   of all that I have seen,
>   of meadow-flowers and butterflies
>   in summers that have been...
-}

module Pugs.CodeGen (codeGen, backends) where
import Pugs.AST
import Pugs.Internals
import Pugs.CodeGen.PIL (genPIL)
-- import Pugs.CodeGen.PIL2 (genPIL2)
import Pugs.CodeGen.PIR (genPIR)
import Pugs.CodeGen.Perl5 (genPerl5)
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
--  , ("Pil2",        genPIL2)
    , ("Perl5",       genPerl5)
    , ("Pugs",        genPugs)
    ]

backends :: [String]
backends = Map.keys generators

norm :: String -> String
norm "" = ""
norm (x:xs) = toUpper x : map toLower xs

doLookup :: String -> IO Generator
doLookup s = Map.lookup (norm s) generators

codeGen :: String -> Env -> IO String
codeGen s env = do
    gen <- catch (doLookup s) $ \_ -> do
        fail $ "Cannot generate code for " ++ s
    rv <- runEvalIO env gen
    case rv of
        VStr str    -> return str
        _           -> fail (show rv)
