{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances -fno-warn-orphans -funbox-strict-fields -cpp #-}
{-# OPTIONS_GHC -#include "UnicodeC.h" #-}

module Pugs.CodeGen.Perl5 (genPerl5) where
import Pugs.Internals
import Pugs.AST
import Pugs.Compile
import DrIFT.Perl5

genPerl5 :: Eval Val
genPerl5 = do
    glob        <- askGlobal
    main        <- asks envBody
    globPIL     <- compile glob :: Eval [PIL_Decl]
    mainPIL     <- compile main :: Eval PIL_Stmts
    return . VStr . unlines $
        [ "bless({"
        , "    pilMain => " ++ showPerl5 mainPIL ++ ","
        , "    pilGlob => " ++ showPerl5 globPIL
        , "} => 'PIL::Environment')"
        ]
