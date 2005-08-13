{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances -fno-warn-orphans -funbox-strict-fields -cpp #-}
{-# OPTIONS_GHC -#include "UnicodeC.h" #-}

module Pugs.CodeGen.PIL (genPIL) where
import Pugs.Internals
import Pugs.AST
import Pugs.Compile

genPIL :: Eval Val
genPIL = do
    glob        <- askGlobal
    main        <- asks envBody
    globPIL     <- compile glob :: Eval [PIL_Decl]
    mainPIL     <- compile main :: Eval PIL_Stmts
    return . VStr . unlines $
        [ "PIL_Environment"
        , "    { pilMain = (" ++ show mainPIL ++ ")"
        , "    , pilGlob = (" ++ show globPIL ++ ")"
        , "    }"
        ]
