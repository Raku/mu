{-# OPTIONS_GHC -fglasgow-exts #-}

{-|
    CodeGen interface.

    TODO: add Tolkien
-}

module Pugs.CodeGen (translate, backends) where
import Pugs.AST
import Pugs.Internals
import Pugs.CodeGen.PIR (genPIR)
import Pugs.Compile.Pugs (genPugs)
import Pugs.Compile.Haskell (genGHC)
import qualified Data.Map as Map

type Translator = Eval Val

translators :: Map String Translator
translators = Map.fromList $
    [ ("Ghc",         genGHC)
    , ("Parrot",      genPIR)
    , ("Pir",         genPIR)
    , ("Pugs",        genPugs)
    ]

backends :: [String]
backends = Map.keys translators

norm :: String -> String
norm s = ucfirst $ map toLower s
    where ucfirst (x:xs) = toUpper x : xs
          ucfirst [] = []

doLookup :: String -> IO Translator
doLookup s = Map.lookup (norm s) translators

translate :: String -> Env -> IO String
translate s env = do
    gen <- catch (doLookup s) $ \_ -> do
        fail $ "Cannot compile to " ++ s
    VStr str <- runEvalIO env gen
    return str
