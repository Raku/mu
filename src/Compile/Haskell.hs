{-# OPTIONS_GHC -fglasgow-exts -fth -cpp #-}

module Compile.Haskell where

#undef PUGS_HAVE_TH
#include "pugs_config.h"
#ifndef PUGS_HAVE_TH
genGHC = error "Template Haskell support not compiled in"
#else

import Internals
import Language.Haskell.TH as TH
import AST
import Run
import Prim

genGHC exp = runQ [d| mainCC = runComp $(compile exp) |] >>= \str -> return . unlines $
    [ "{-# OPTIONS_GHC -fglasgow-exts -fth -O #-}"
    , "module MainCC where"
    , "import GHC.Base"
    , "import Run"
    , "import AST"
    , "import Prim"
    , "import Internals"
    , "import Language.Haskell.TH as TH"
    , ""
    , pprint str
    ]

compile (App op [inv] []) = compile (App op [] [inv])
compile (App ('&':op) [] [arg]) = [| do
        val <- $(argC)
        op1 op val
    |] where
    argC = compile arg
compile (Val (VStr s)) = [| return (VStr s) |]
compile (Statements [(st, _)]) = [| do $(compile st) |]
compile exp = internalError ("Unrecognized construct: " ++ show exp)

#endif
