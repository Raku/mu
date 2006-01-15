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
import Pugs.CodeGen.PIL2 (genPIL2, genPIL2Perl5, genPIL2JSON, genPIL2YAML)
import Pugs.CodeGen.PIR (genPIR, genPIR_YAML)
import Pugs.CodeGen.Perl5 (genPerl5)
import Pugs.CodeGen.YAML (genYAML)
import Pugs.CodeGen.JSON (genJSON)
import Pugs.Compile.Pugs (genPugs)
import Pugs.Compile.Haskell (genGHC)
-- import Pugs.CodeGen.XML (genXML)
import qualified Data.Map as Map

type Generator = Eval Val

generators :: Map String Generator
generators = Map.fromList $
    [ ("GHC",         genGHC)
    , ("PIR",         genPIR)
    , ("PIR-YAML",    genPIR_YAML)
    , ("PIL1",        genPIL1)
    , ("PIL1-Perl5",  genPerl5)
    , ("PIL1-JSON",   genJSON)
    , ("PIL1-YAML",   genYAML)
    , ("PIL2",        genPIL2)
    , ("PIL2-Perl5",  genPIL2Perl5)
    , ("PIL2-JSON",   genPIL2JSON)
    , ("PIL2-YAML",   genPIL2YAML)
    , ("Pugs",        genPugs)
    , ("Parse-YAML",  genParseYAML)
--  , ("XML",         genXML)
    ]

backends :: [String]
backends = Map.keys generators

norm :: String -> String
norm = norm' . map toLower . filter isAlphaNum
    where
    norm' "ghc"    = "GHC"
    norm' "parrot" = "!PIR"
    norm' "pir"    = "PIR"
    norm' "piryaml"= "PIR-YAML"
    norm' "pil"    = "!PIL1"
    norm' "pil1"   = "PIL1"
    norm' "pil2"   = "PIL2"
    norm' "perl5"  = "!PIL1-Perl5"
    norm' "json"   = "!PIL1-JSON"
    norm' "yaml"   = "!PIL1-YAML"
    norm' "pil1perl5"  = "PIL1-Perl5"
    norm' "pil1json"   = "PIL1-JSON"
    norm' "pil1yaml"   = "PIL1-YAML"
    norm' "pil2perl5"  = "PIL2-Perl5"
    norm' "pil2json"   = "PIL2-JSON"
    norm' "pil2yaml"   = "PIL2-YAML"
    norm' "pugs"   = "Pugs"
    -- norm' "xml"    = "XML"
    norm' x        = x

doLookup :: String -> IO Generator
doLookup s = do
    case norm s of
        ('!':key) -> do
            hPutStrLn stderr $ "*** The backend '" ++ s ++ "' is deprecated."
            hPutStrLn stderr $ "    Please use '" ++ key ++ "' instead."
            Map.lookup key generators
        key -> Map.lookup key generators

codeGen :: String -> Env -> IO String
codeGen s env = do
    gen <- catch (doLookup s) $ \_ -> do
        fail $ "Cannot generate code for " ++ s
    rv <- runEvalIO env gen
    case rv of
        VStr str    -> return str
        _           -> fail (show rv)
