{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances -fno-warn-orphans -funbox-strict-fields -cpp #-}

module Pugs.CodeGen.Perl5 (genPerl5) where
import Pugs.AST
import Pugs.Compile
import Pugs.PIL1.Instances ()
import Pugs.PIL1
import DrIFT.Perl5

-- XXX: do something useful with the filename arg
genPerl5 :: FilePath -> Eval Val
genPerl5 _ = do
    penv <- compile () :: Eval PIL_Environment
    return . VStr . unlines $ [showPerl5 penv]
