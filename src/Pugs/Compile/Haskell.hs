{-# OPTIONS_GHC -fglasgow-exts -fth -cpp #-}

module Pugs.Compile.Haskell where

#undef PUGS_HAVE_TH
#include "../pugs_config.h"
#ifndef PUGS_HAVE_TH
genGHC = error "Template Haskell support not compiled in"
#else

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Lib
import Pugs.Internals
import Pugs.AST
import Pugs.Run
import Pugs.Prim

genGHC :: Eval Val
-- Haddock doesn't like Template Haskell.
#ifndef HADDOCK
genGHC = do
    exp <- asks envBody
    liftIO (TH.runQ [d|
        mainCC :: IO Val
        mainCC = runComp $(compile exp) |]) >>= \str -> return . VStr . unlines $
            [ "{-# OPTIONS_GHC -fglasgow-exts -fth -O #-}"
            , "module MainCC where"
            , "import qualified GHC.Base"
            , "import qualified GHC.IOBase"
            , "import qualified Pugs.Run"
            , "import qualified Pugs.AST"
            , "import qualified Pugs.AST.Internals"
            , "import qualified Pugs.Types"
            , "import qualified Pugs.Prim"
            , "import qualified Pugs.Internals"
            , "import Language.Haskell.TH as TH"
            , ""
            , TH.pprint str
            ]
#endif

-- Haddock doesn't like Template Haskell.
compile :: Exp -> Language.Haskell.TH.Lib.ExpQ
#ifndef HADDOCK
compile (Stmts stmt rest) = [| do
        $(argC)
        $(argRest)
    |] where
    argC = compile stmt
    argRest = compile rest
compile (App (Var op) [] []) = [| op0 op [] |]
compile (App (Var op) [] args) = compile (App (Var op) args [])
compile (App (Var ('&':op)) [arg] []) = [| do
        val <- $(argC)
        op1 op val
    |] where
    argC = compile arg
compile (App (Var ('&':op)) [arg1, arg2] []) = [| do
        val1 <- $(argC1)
        val2 <- $(argC2)
        op2 op val1 val2
    |] where
    argC1 = compile arg1
    argC2 = compile arg2
compile (Cxt _ arg) = compile arg
compile (Pos _ arg) = compile arg
compile (Val (VInt i)) = [| return (VInt i) |]
compile (Val (VStr s)) = [| return (VStr s) |]
compile (Val (VBool b)) = [| return (VBool b) |]
compile Noop = [| return undef |]
compile exp = internalError ("Unrecognized construct: " ++ show exp)
#endif

#endif

