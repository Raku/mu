{-# OPTIONS -cpp -fglasgow-exts -fth #-}

module Compile.Parrot where
import Internals
import Pretty
import AST
import Text.PrettyPrint

genPIR exp = return . unlines $
    [ "#!/usr/bin/env parrot"
    , ".sub main @MAIN"
    , renderStyle (Style LeftMode 0 0) (compile exp)
    , ".end"
    ]

compile :: Exp -> Doc
compile (App "&say" invs args) = 
    compile $ App "&print" invs (args ++ [Val $ VStr "\n"])
compile (App "&print" invs args) = vcat $
    map ((text "print" <+>) . compile) (invs ++ args)
compile (Val (VStr x))  = showText $ encodeUTF8 (concatMap quoted x)
compile (Val (VInt x))  = integer x
compile (Val (VNum x))  = double x
compile (Val (VRat x))  = double $ ratToNum x
compile (Val VUndef)    = text "PerlUndef"
compile (Statements stmts) = vcat $ map (compile . fst) stmts
compile exp = internalError ("Unrecognized construct: " ++ show exp)

showText = text . show
