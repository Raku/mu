{-# OPTIONS -cpp -fglasgow-exts -fth #-}

module Compile.Haskell where
import Internals
import Internals.TH
import AST
import Run
import Prim

genGHC exp = runQ [d| mainCC = runComp $(compile exp) |] >>= \str -> return . unlines $
    [ "{-# OPTIONS -fglasgow-exts -O #-}"
    , "module MainCC where"
    , "import GHC.Base"
    , "import Run"
    , "import AST"
    , "import Prim"
    , "import Internals"
    , ""
    , showTH str
    ]

compile (App ('&':op) [] [arg]) = [| do
        val <- $(argC)
        op1 op val
    |] where
    argC = compile arg
compile (Val (VStr s)) = [| return (VStr s) |]
compile (Statements [(st, _)]) = [| do $(compile st) |]
compile exp = internalError ("Unrecognized construct: " ++ show exp)
