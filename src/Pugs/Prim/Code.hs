{-# OPTIONS_GHC -fglasgow-exts #-}
module Pugs.Prim.Code where
import Pugs.AST
import Pugs.Internals

op1CodeAssoc :: Val -> Eval Val
op1CodeAssoc v = do
    code <- fromVal v
    return . castV $ subAssoc code

op1CodeName :: Val -> Eval Val
op1CodeName v = do
    code <- fromVal v
    return . castV $ subName code

op1CodeArity :: Val -> Eval Val
op1CodeArity v = do
    code <- fromVal v
    return . castV . length $ subParams code

op1CodeBody :: Val -> Eval Val
op1CodeBody v = do
    code <- fromVal v
    expToEvalVal $ subBody code