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
        , "import Run"
        , "import AST"
        , "import Pugs.Types"
        , "import Prim"
        , "import Internals"
        , "import Language.Haskell.TH as TH"
        , ""
        , TH.pprint str
        ]

compile (App op [inv] []) = compile (App op [] [inv])
compile (App ('&':op) [] [arg]) = [| do
        val <- $(argC)
        op1 op val
    |] where
    argC = compile arg
compile (Val (VStr s)) = [| return (VStr s) |]
compile (Stmts [(st, _)]) = [| do $(compile st) |]
compile Noop = [| return () |]
compile exp = internalError ("Unrecognized construct: " ++ show exp)

#endif

