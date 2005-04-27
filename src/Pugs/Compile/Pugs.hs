{-# OPTIONS_GHC -fglasgow-exts #-}

module Pugs.Compile.Pugs where
import Pugs.AST
import Pugs.Internals

genPugs :: Env -> IO String
genPugs Env{ envBody = exp, envGlobal = globRef } = do
    glob <- readIORef globRef
    return . unlines $
        [ "{-# OPTIONS_GHC -fglasgow-exts -fno-warn-unused-imports -fno-warn-unused-binds -O #-}"
        , "module MainCC where"
        , "import Pugs.Run"
        , "import Pugs.AST"
        , "import Pugs.Types"
        , "import Pugs.Internals"
        , ""
        , "mainCC = runAST glob ast"
        , "    where"
        , "    glob = " ++ show glob
        , "    ast  = " ++ show exp
        , ""
        ]

