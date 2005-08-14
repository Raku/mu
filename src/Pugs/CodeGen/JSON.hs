{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances -fno-warn-orphans -funbox-strict-fields -cpp #-}
{-# OPTIONS_GHC -#include "UnicodeC.h" #-}

module Pugs.CodeGen.JSON (genJSON) where
import Pugs.Internals
import Pugs.AST
import Pugs.Compile
import Pugs.PIL1
import DrIFT.JSON

genJSON :: Eval Val
genJSON = do
    penv <- compile () :: Eval PIL_Environment
    return . VStr . unlines $ [showJSON penv]
