{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances -fno-warn-orphans -funbox-strict-fields -cpp #-}
{-# OPTIONS_GHC -#include "UnicodeC.h" #-}

module Pugs.CodeGen.PIL (genPIL) where
import Pugs.Internals
import Pugs.AST.Internals
import Emit.PIR
import Pugs.Compile

genPIL :: Eval Val
genPIL = do
    glob        <- askGlobal
    main        <- asks envBody
    globPIL     <- compile glob :: Eval [PIL Decl]
    mainPIL     <- compile main :: Eval (PIL [Stmt])
    return . VStr . unlines $
        [ "PIL_Environment"
        , "    { pilMain = (" ++ show mainPIL ++ ")"
        , "    , pilGlob = (" ++ show globPIL ++ ")"
        , "    }"
        ]
