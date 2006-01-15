{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances -fno-warn-orphans -funbox-strict-fields -cpp #-}
{-# OPTIONS_GHC -#include "../../UnicodeC.h" #-}

module Pugs.CodeGen.YAML (genYAML, genParseYAML) where
import Pugs.Internals
import Pugs.AST
import Pugs.Compile
import Pugs.PIL1
import DrIFT.YAML

genParseYAML :: Eval Val
genParseYAML = do
    main    <- asks envBody
    yaml    <- liftIO (showYaml main)
    return (VStr yaml)

genYAML :: Eval Val
genYAML = do
    penv <- compile () :: Eval PIL_Environment
    yaml <- liftIO (showYaml penv)
    return (VStr yaml)
