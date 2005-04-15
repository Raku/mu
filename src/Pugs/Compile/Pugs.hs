{-# OPTIONS_GHC -fglasgow-exts #-}

module Pugs.Compile.Pugs where
import Pugs.Internals

genPugs :: (Show a, Monad m) => a -> m String
genPugs exp = return . unlines $
    [ "{-# OPTIONS_GHC -fglasgow-exts -fno-warn-unused-imports -fno-warn-unused-binds -O #-}"
    , "module MainCC where"
    , "import Pugs.Run"
    , "import Pugs.AST"
    , "import Pugs.Internals"
    , ""
    , "mainCC = runAST $ " ++ show exp
    ]

