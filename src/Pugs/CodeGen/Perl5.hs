{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances -fno-warn-orphans -funbox-strict-fields -cpp #-}
{-# OPTIONS_GHC -#include "../../UnicodeC.h" #-}

module Pugs.CodeGen.Perl5 (genPerl5) where
import Pugs.Internals
import Pugs.AST
import Pugs.Compile
import Pugs.PIL1.Instances ()
import Pugs.PIL1
import DrIFT.Perl5

genPerl5 :: Eval Val
genPerl5 = do
    penv <- compile () :: Eval PIL_Environment
    return . VStr . unlines $ [showPerl5 penv]
