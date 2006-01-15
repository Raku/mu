{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances -fno-warn-orphans -funbox-strict-fields -cpp #-}
{-# OPTIONS_GHC -#include "../../UnicodeC.h" #-}

module Pugs.CodeGen.PIL2 (
    genPIL2,
    genPIL2Perl5, genPIL2JSON, genPIL2YAML
) where
import Pugs.Internals
import Pugs.AST
import Pugs.PIL2
import Pugs.Compile.PIL2
import System.IO
import System.Directory
import DrIFT.Perl5
import DrIFT.JSON
import DrIFT.YAML

genPIL2 :: Eval Val
genPIL2 = do
    penv <- compile ()
    return . VStr . unlines $
        [ "PIL_Environment"
        , "    { pilMain = (" ++ show (pilMain penv) ++ ")"
        , "    , pilGlob = (" ++ show (pilGlob penv) ++ ")"
        , "    }"
        ]

genPIL2Perl5 :: Eval Val
genPIL2Perl5 = do
    penv <- compile () :: Eval PIL_Environment
    return . VStr . unlines $ [showPerl5 penv]

genPIL2JSON :: Eval Val
genPIL2JSON = do
    penv <- compile () :: Eval PIL_Environment
    return . VStr . unlines $ [showJSON penv]

genPIL2YAML :: Eval Val
genPIL2YAML = do
    penv <- compile () :: Eval PIL_Environment
    yaml <- liftIO (showYaml penv)
    return . VStr . unlines $ [yaml]
