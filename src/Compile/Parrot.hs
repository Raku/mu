{-# OPTIONS -cpp -fglasgow-exts -fth #-}

module Compile.Parrot where
import Internals
import Pretty
import AST
import Text.PrettyPrint

-- XXX This compiler needs a totaly rewrite using Parrot AST,
-- XXX and maybe TH-based AST combinators

genPIR exp = return . unlines $
    [ "#!/usr/bin/env parrot"
    , ".sub main @MAIN"
    , ""
    , renderStyle (Style LeftMode 0 0) (compile exp)
    , ".end"
    ]

class (Show x) => Compile x where
    compile :: x -> Doc
    compile x = internalError ("Unrecognized construct: " ++ show x)

varText ('$':name)  = text $ "s__" ++ name
varText ('@':name)  = text $ "a__" ++ name
varText ('%':name)  = text $ "h__" ++ name
varText x           = error $ "invalid name: " ++ x

varInit ('$':_) = text $ "PerlUndef"
varInit ('@':_) = text $ "PerlArray"
varInit ('%':_) = text $ "PerlHash"
varInit x       = error $ "invalid name: " ++ x

instance Compile Symbol where
    compile (SymExp _ name exp) = vcat $
        [ text ".local" <+> text "pmc" <+> varText name
        , varText name <+> text "=" <+> text "new" <+> varInit name
        , mval exp
        ]
        where
        mval (App "&not" [] []) = text ""
        mval _ | ('$':_) <- name  = varText name <+> text "=" <+> compile exp
        mval (Syn "mval" [Syn "," [x, exp]]) | ('@':_) <- name =
            mval (Syn "mval" [x]) $+$
            text "push" <+> varText name <+> text "," <+> compile exp
        mval (Syn "mval" [exp]) | ('@':_) <- name =
            text "push" <+> varText name <+> text "," <+> compile exp
        mval _ = error $ show (exp, name)
    compile (SymVal scope name val) =
        compile (SymExp scope name $ Val val)

instance Compile SourcePos where
    compile pos = text "#" <+> format pos

instance Compile Exp where
    compile (Var name) = varText name
    compile (App "&say" invs args) = 
        compile $ App "&print" invs (args ++ [Val $ VStr "\n"])
    compile (App "&print" invs args) = vcat $
        map ((text "print" <+>) . compile) (invs ++ args)
    compile (Val (VStr x))  = showText $ encodeUTF8 (concatMap quoted x)
    compile (Val (VInt x))  = integer x
    compile (Val (VNum x))  = double x
    compile (Val (VRat x))  = double $ ratToNum x
    compile (Val VUndef)    = text "PerlUndef"
    compile (Statements stmts) = vcat $
        [ compile pos $+$ compile stmt $+$ text "" | (stmt, pos) <- stmts ]
    compile (Sym syms) = vcat $
        map compile syms
    compile (Syn "mval" [exp]) = compile exp
    compile (App "&not" [] []) =
        text "new" <+> compile (Val VUndef)
    compile x = error $ "XXX" ++ (show x)

showText = text . show
