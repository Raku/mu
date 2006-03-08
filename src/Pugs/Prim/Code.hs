{-# OPTIONS_GHC -fglasgow-exts #-}
module Pugs.Prim.Code (
    op1CodeAssoc, op1CodeName, op1CodeArity, op1CodeBody, op1CodePos,
) where
import Pugs.AST
import Pugs.Internals
import Pugs.Pretty

{- On Code -}

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
    (code :: VCode) <- fromVal v
    --expToEvalVal $ subBody code
    return $ castV code

op1CodePos :: Val -> Eval Val
op1CodePos v = do
    code <- fromVal v
    let env = subEnv code
    case env of
        Nothing  -> return VUndef
        Just env -> return $ castV $ pretty $ envPos env

{- On Code::Exp -}

