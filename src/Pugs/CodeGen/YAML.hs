{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances -fno-warn-orphans -funbox-strict-fields -cpp #-}
{-# OPTIONS_GHC -#include "../../UnicodeC.h" #-}

module Pugs.CodeGen.YAML (genYAML) where
import Pugs.Internals
import Pugs.AST
import Pugs.Compile
import Pugs.PIL1
import DrIFT.YAML

genYAML :: Eval Val
genYAML = do
    penv <- compile () :: Eval PIL_Environment
    yaml <- liftIO (showYaml penv)
    return . VStr . unlines $ [yaml]
