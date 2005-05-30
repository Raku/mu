{-# OPTIONS_GHC -fglasgow-exts #-}
module Pugs.Prim.Code (
    -- for Pugs.Prim
    op1Assoc
) where
import Pugs.AST
import Pugs.Internals
import Pugs.Types

op1Assoc :: Val -> Eval Val
op1Assoc codeval = do
    code <- fromVal codeval :: Eval VCode
    return $ VStr $ code_assoc code
