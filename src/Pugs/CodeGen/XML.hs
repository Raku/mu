{-# OPTIONS_GHC -fglasgow-exts #-}
{-# OPTIONS_GHC -#include "UnicodeC.h" #-}

module Pugs.CodeGen.XML (genXML) where
import Pugs.Internals
import Pugs.AST
import Pugs.Compile
import Pugs.PIL1
import Text.XML.HaXml.Haskell2Xml

genXML :: Eval Val
genXML = do
    glob        <- askGlobal
    main        <- asks envBody
    globPIL     <- compile glob :: Eval [PIL_Decl]
    mainPIL     <- compile main :: Eval PIL_Stmts
    return . VStr . showXml $ PIL_Environment globPIL mainPIL
