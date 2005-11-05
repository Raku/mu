{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances -fno-warn-orphans -funbox-strict-fields -cpp #-}
{-# OPTIONS_GHC -#include "../../UnicodeC.h" #-}

module Pugs.CodeGen.PIL2 (genPIL2) where
import Pugs.Internals
import Pugs.AST
import Pugs.PIL1
import Pugs.Compile

genPIL2 :: Eval Val
genPIL2 = do
    penv <- compile ()
    return . VStr . unlines $
        [ "PIL_Environment"
        , "    { pilMain = (" ++ show (pilMain penv) ++ ")"
        , "    , pilGlob = (" ++ show (pilGlob penv) ++ ")"
        , "    }"
        ]
