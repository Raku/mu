{-# OPTIONS -cpp -fglasgow-exts -fth #-}

module Compile.Haskell where
import Internals
import AST
import Prim

#if __GLASGOW_HASKELL__ < 604
import Language.Haskell.THSyntax
ppr = show
#else
import Language.Haskell.Syntax
#endif

genGHC x = do
    str <- runQ $ compile x
    return $ ppr str

compile (App ('&':op) [] [arg]) = [| do
        val <- $(argC)
        op1 op val
    |] where
    argC = compile arg
compile (Val (VStr s)) = [| return (VStr s) |]
compile (Statements [(st, pos)]) = [| do $(compile st) |]
compile x = error (show x)
