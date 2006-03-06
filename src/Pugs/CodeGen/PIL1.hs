{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances -fno-warn-orphans -funbox-strict-fields -cpp #-}
{-# OPTIONS_GHC -#include "../../UnicodeC.h" #-}

module Pugs.CodeGen.PIL1 (genPIL1) where
import Pugs.Internals
import Pugs.AST
import Pugs.PIL1.Instances ()
import Pugs.PIL1
import Pugs.Compile

genPIL1 :: Eval Val
genPIL1 = do
    penv <- compile ()
    return . VStr . unlines $
        [ "PIL_Environment"
        , "    { pilMain = (" ++ show (pilMain penv) ++ ")"
        , "    , pilGlob = (" ++ show (pilGlob penv) ++ ")"
        , "    }"
        ]
