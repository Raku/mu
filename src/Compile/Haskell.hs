{-# OPTIONS -fglasgow-exts -fth #-}

module Compile.Haskell where
import Internals
import AST
import Prim
import Language.Haskell.TH

genGHC x = do
    str <- runQ $ compile x
    return $ pprint str

compile (App ('&':op) [] [arg]) = [| do
        val <- $(argC)
        op1 op val
    |] where
    argC = compile arg
compile (Val (VStr s)) = [| return (VStr s) |]
compile (Statements [(st, pos)]) = [| do $(compile st) |]
compile x = error (show x)
