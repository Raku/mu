{-# OPTIONS_GHC -fglasgow-exts -fallow-overlapping-instances #-}
module Pugs.Prim.Code (
    op1CodeAssoc, op1CodeName, op1CodeArity, op1CodeBody, op1CodePos, op1CodeSignature
) where
import Pugs.AST
import Pugs.Internals
-- import Pugs.Pretty

{- On Code -}

op1CodeAssoc :: Val -> Eval Val
op1CodeAssoc v = do
    code <- fromVal v
    return $ case subAssoc code of
        ANil                    -> undef
        AIrrelevantToParsing    -> undef
        A_left                  -> castV "left"
        A_right                 -> castV "right"
        A_non                   -> castV "non"
        A_chain                 -> castV "chain"
        A_list                  -> castV "list"

op1CodeName :: Val -> Eval Val
op1CodeName v = do
    code <- fromVal v
    return . VStr $ case cast $ subName code of
        "<anon>"  -> cast $ subName code
        _         -> "&" ++ cast (subPackage code) ++
                     "::" ++ tail (cast (subName code))

op1CodeArity :: Val -> Eval Val
op1CodeArity v = do
    code <- fromVal v
    return . castV . length $ subParams code

op1CodeBody :: Val -> Eval Val
op1CodeBody v = do
    (code :: VCode) <- fromVal v
    expToEvalVal $ subBody code

op1CodePos :: Val -> Eval Val
op1CodePos v = do -- die "XXX - code.pos not implemented" v
    return $ castV (show v)
{-
do
    code <- fromVal v
    let env = subEnv code
    case env of
        Nothing  -> return VUndef
        Just env -> return $ castV $ pretty $ envPos env
-}

op1CodeSignature :: Val -> Eval Val
op1CodeSignature v = do
    code <- fromVal v
    return . VV . mkVal . paramsToSig . subParams $ code

{- On Code::Exp -}

