{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances -fno-warn-orphans -funbox-strict-fields -cpp #-}
{-# OPTIONS_GHC -#include "UnicodeC.h" #-}

module Pugs.CodeGen.PIL (genPIL) where
import Pugs.Internals
import Pugs.AST.Internals
import Emit.PIR
import Pugs.Compile

{-| Compiles the current environment to PIR code. -}
genPIL :: Eval Val
genPIL = do
    main        <- asks envBody
    mainPIL     <- compile main :: Eval (PIL [Stmt])
    return . VStr $ show mainPIL
