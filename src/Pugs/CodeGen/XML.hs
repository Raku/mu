{-# OPTIONS_GHC -fglasgow-exts #-}

module Pugs.CodeGen.XML (genXML) where
import Pugs.Internals
import Pugs.AST
import Pugs.Compile
import Pugs.PIL1
import Text.XML.HaXml.Haskell2Xml

genXML :: Eval Val
genXML = do
    penv <- compile () :: Eval PIL_Environment
    return $ VStr (showXml penv)
