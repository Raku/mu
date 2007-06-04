{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances -fno-warn-orphans -funbox-strict-fields -cpp #-}

module Pugs.CodeGen.JSON (genJSON) where
import Pugs.AST
import Pugs.Compile
import Pugs.PIL1.Instances ()
import Pugs.PIL1
import DrIFT.JSON

-- XXX: do something useful with the filename arg
genJSON :: FilePath -> Eval Val
genJSON _ = do
    penv <- compile () :: Eval PIL_Environment
    return . VStr . unlines $ [showJSON penv]
