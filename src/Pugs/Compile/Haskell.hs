{-# OPTIONS_GHC -fglasgow-exts -fth -cpp #-}

module Pugs.Compile.Haskell where

#undef PUGS_HAVE_TH
#include "../pugs_config.h"
#ifndef PUGS_HAVE_TH
genGHC = error "Template Haskell support not compiled in"
#else

import Pugs.Internals
import qualified Language.Haskell.TH as TH
import Pugs.AST
import Pugs.Run
import Pugs.Prim

genGHC :: Env -> IO String
genGHC Env{ envBody = exp } =
    TH.runQ [d| mainCC = runComp $(compile exp) |] >>= \str -> return . unlines $
        [ "{-# OPTIONS_GHC -fglasgow-exts -fth -O #-}"
        , "module MainCC where"
        , "import GHC.Base"
        , "import Pugs.Run"
        , "import Pugs.AST"
        , "import Pugs.Types"
        , "import Pugs.Prim"
        , "import Pugs.Internals"
        , "import Language.Haskell.TH as TH"
        , ""
        , TH.pprint str
        ]

compile (Stmts []) = [| return undef |]
compile (Stmts [(stmt, _)]) = compile stmt
compile (Stmts ((stmt, _):stmts)) = [| do
        $(argC)
        $(argRest)
    |] where
    argC = compile stmt
    argRest = compile (Stmts stmts)
compile (App op [] []) = [| op0 op [] |]
compile (App op [] args) = compile (App op args [])
compile (App ('&':op) [arg] []) = [| do
        val <- $(argC)
        op1 op val
    |] where
    argC = compile arg
compile (App ('&':op) [arg1, arg2] []) = [| do
        val1 <- $(argC1)
        val2 <- $(argC2)
        op2 op val1 val2
    |] where
    argC1 = compile arg1
    argC2 = compile arg2
compile (Cxt _ arg) = compile arg
compile (Val (VInt i)) = [| return (VInt i) |]
compile (Val (VStr s)) = [| return (VStr s) |]
compile (Val (VBool b)) = [| return (VBool b) |]
compile Noop = [| return () |]
compile exp = internalError ("Unrecognized construct: " ++ show exp)

#endif

