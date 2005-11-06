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
import Pugs.CodeGen.PIL1 (genPIL1)
import Pugs.CodeGen.PIL2 (genPIL2, genPIL2Perl5, genPIL2Binary, genPIL2JSON)
import Pugs.CodeGen.PIR (genPIR)
import Pugs.CodeGen.Perl5 (genPerl5)
import Pugs.CodeGen.JSON (genJSON)
import Pugs.CodeGen.Binary (genBinary)
import Pugs.Compile.Pugs (genPugs)
import Pugs.Compile.Haskell (genGHC)
-- import Pugs.CodeGen.XML (genXML)
import qualified Data.Map as Map

type Generator = Eval Val

generators :: Map String Generator
generators = Map.fromList $
    [ ("GHC",         genGHC)
    , ("Parrot",      genPIR)
    , ("PIR",         genPIR)
    , ("PIL1",        genPIL1)
    , ("PIL2",        genPIL2)
    , ("PIL2-Perl5",  genPIL2Perl5)
    , ("PIL2-JSON",   genPIL2JSON)
    , ("PIL2-Binary", genPIL2Binary)
    , ("Perl5",       genPerl5)
    , ("Pugs",        genPugs)
    , ("Binary",      genBinary)
    , ("JSON",        genJSON)
--  , ("XML",         genXML)
    ]

backends :: [String]
backends = Map.keys generators

norm :: String -> String
norm = norm' . map toLower . filter isAlphaNum
    where
    norm' "ghc"    = "GHC"
    norm' "parrot" = "Parrot"
    norm' "pir"    = "PIR"
    norm' "pil"    = "PIL1" -- XXX - this will change
    norm' "pil1"   = "PIL1"
    norm' "pil2"   = "PIL2"
    norm' "perl5"  = "Perl5"
    norm' "pil2perl5"  = "PIL2-Perl5"
    norm' "pil2json"   = "PIL2-JSON"
    norm' "pil2binary" = "PIL2-Binary"
    norm' "pugs"   = "Pugs"
    norm' "binary" = "Binary"
    norm' "json"   = "JSON"
    -- norm' "xml"    = "XML"
    norm' x        = x

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
