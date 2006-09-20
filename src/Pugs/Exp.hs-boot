{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances -cpp #-}

module Pugs.Exp where
import Text.PrettyPrint

data Exp
data Stmt

prettyExp :: Exp -> Doc
