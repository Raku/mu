{-# OPTIONS -cpp -fglasgow-exts -fth #-}

module Compile.Haskell where
import Internals
import AST
import Prim

#if __GLASGOW_HASKELL__ < 604
import Language.Haskell.THSyntax
display = show
#else
import Language.Haskell.TH
display = show . ppr
#endif

genGHC x = do
    str <- runQ $ compile x
    return $ display str

compile (App ('&':op) [] [arg]) = [| do
        val <- $(argC)
        op1 op val
    |] where
    argC = compile arg
compile (Val (VStr s)) = [| return (VStr s) |]
compile (Statements [(st, _)]) = [| do $(compile st) |]
compile x = error (show x)
