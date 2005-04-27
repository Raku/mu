{-# OPTIONS_GHC -fglasgow-exts #-}

module Pugs.Compile.Parrot where
import Pugs.Internals
import Pugs.Pretty
import Pugs.AST
import Pugs.Types
import Data.HashTable
import Text.PrettyPrint
import qualified Pugs.Types.Scalar as Scalar
import qualified Pugs.Types.Code   as Code

-- XXX This compiler needs a totaly rewrite using Parrot AST,
-- XXX and maybe TH-based AST combinators

genPIR :: Eval Val
genPIR = do
    Env{ envBody = exp, envGlobal = globRef } <- ask

    glob <- liftIO $ readIORef globRef

    -- get a list of functions
    init <- compileEval glob

    return . VStr . unlines $
        [ "#!/usr/bin/env parrot"
        , renderStyle (Style PageMode 0 0) init
        , renderStyle (Style PageMode 0 0) $ vcat
            [ text ".sub main @MAIN"
            , nest 4 (compile exp)
            , text ".end"
            ]
        ]

instance Compile Doc where
    compile = id

instance Compile Pad where
    compileEval pad = fmap vcat $ mapM compileEval (padToList pad)

instance Compile (String, [IORef VRef]) where
    compileEval (('&':name), [sym]) = do
        imc <- compileEval sym
        return $ vcat
            [ text (".sub \"" ++ name ++ "\"")
            , nest 4 imc
            , text ".end"
            ]
    compileEval x = internalError ("Unrecognized construct: " ++ show x)

instance Compile (IORef VRef) where
    compileEval x = do
        ref <- liftIO $ readIORef x
        compileEval ref

instance Compile VRef where
    compileEval (MkRef (ICode cv)) = do
        vsub <- Code.fetch cv
        compileEval vsub
    compileEval (MkRef (IScalar sv))
        | Scalar.iType sv == mkType "Scalar::Const" = do
            sv  <- Scalar.fetch sv
            ref <- fromVal sv
            compileEval (ref :: VCode)
    compileEval x = internalError ("Unrecognized construct: " ++ show x)

instance Compile VCode where
    compileEval sub = do
        prms <- mapM compileEval (subParams sub)
        body <- compileEval (subFun sub)
        return . vcat $ prms ++ [ text "", body ]

instance Compile Param where
    compile prm = text ".param pmc" <+> varText (paramName prm)

class (Show x) => Compile x where
    compileEval :: x -> Eval Doc
    compileEval x = return (compile x)
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

instance Compile SourcePos where
    compile SourcePos{ sourceName = file, sourceLine = line } = hsep $
        [ text "#line"
        , doubleQuotes $ text file
        , showText line
        ]

declareLabel :: (Show a) => a -> String -> Doc
declareLabel exp str = text $
    "LABEL_" ++ show (hashString (show exp)) ++ "_" ++ str

label doc = doc <> text ":"

compileCond neg exps@[cond, bodyIf, bodyElse] =
    let [alt, end] = map (declareLabel exps) ["else", "endif"] in vcat $
        [ text neg <+> compile cond <+> text "goto" <+> alt
        , compile bodyIf
        , text "goto" <+> end
        , label alt
        , compile bodyElse
        , label end
        ]
compileCond x y = error $ show (x,y)

instance Compile Exp where
    compile (Var name) = varText name
    compile (Syn ";" stmts) = vcat $ map compile stmts
    compile (Syn "block" blocks) = vcat $ map compile blocks
    compile (Syn "=" [lhs, rhs]) = compileAssign lhs rhs
    compile (Syn "if" exps) = compileCond "unless" exps
    compile (Syn "unless" exps) = compileCond "if" exps
    compile exp@(Syn "loop" [pre, cond, post, body]) = 
        let [start, end, last] = map (declareLabel exp) ["start", "end", "last"] in vcat $
            [ compile pre
            , text "goto" <+> end
            , label start
            , text ".local pmc last"
            , text "last = new Continuation"
            , text "set_addr last," <+> last
            , compile body
            , compile post
            , label end
            -- , text "if" <+> compile cond <+> text "goto" <+> start
            , compile cond
            , text "goto" <+> start
            , label last
            ]
    compile (App "&return" [] [val]) = text ".return" <+> parens (compile val)
    compile (App "&last" _ _) = text "invoke last"
    compile (App "&substr" [] [str, start, Val (VInt 1)]) = hcat $
        [ compile str
        , text "["
        , compile start
        , text "]"
        ]
    compile (App "&postfix:++" [inv] []) = text "inc" <+> compile inv
    compile (App "&postfix:--" [inv] []) = text "dec" <+> compile inv
    compile (App "&infix:~" [exp, Val (VStr "")] []) = compile exp
    compile (App ('&':'i':'n':'f':'i':'x':':':op) [lhs, rhs] []) =
        compile lhs <+> text op <+> compile rhs
    compile (App "&say" invs args) = 
        compile $ App "&print" invs (args ++ [Val $ VStr "\n"])
    compile (App "&print" invs args) = vcat $
        map (\x -> vcat
                [ text ".local pmc tmp"
                , text "tmp = new PerlUndef"
                , compileAssign (text "tmp") x
                , text "print tmp"
                ])
            (invs ++ args)
    compile (App "&not" [] []) =
        text "new" <+> compile (Val VUndef)
    compile (Val (VStr x))  = showText $ encodeUTF8 (concatMap quoted x)
    compile (Val (VInt x))  = integer x
    compile (Val (VNum x))  = showText x
    compile (Val (VRat x))  = showText $ ratToNum x
    compile (Val VUndef)    = text "PerlUndef"
    compile Noop            = empty
    compile (Stmts stmts) = vcat $
        [ compile pos $+$ compile stmt $+$ text ""
        | (stmt, pos) <- stmts
        ]
    compile (Pad _ pad) = vcat $ concat
        [ [ text ".local" <+> text "pmc" <+> varText name
          , varText name <+> text "=" <+> text "new" <+> varInit name
          ]
          | (name, _) <- padToList pad
        ]
    compile (Syn "mval" [exp]) = compile exp
    compile (Syn "," things) = vcat $ map compile things
    compile (Syn syn [lhs, exp]) | last syn == '=' =
        compile $ Syn "=" [lhs, App ("&infix:" ++ init syn) [lhs, exp] []]
    compile (Cxt _ exp) = compile exp
    compile x = error $ "Cannot compile: " ++ (show x)

showText :: (Show a) => a -> Doc
showText = text . show

compileAssign :: (Compile a) => a -> Exp -> Doc
compileAssign lhs rhs@(Var _) = hsep [ compile lhs, text "=", text "assign", compile rhs ]
compileAssign lhs (App ('&':name) _ [arg]) = vcat $
        [ text ".local pmc tmp"
        , text "tmp = new PerlUndef"
        , compileAssign (text "tmp") arg
        , hsep [compile lhs, text "=", text name <> parens (text "tmp")]
        ]
compileAssign lhs (Syn "[]" [arr, idx]) = vcat $
        [ compile lhs <+> text "=" <+> compile arr <> text "[" <> compile idx <> text"]"
        ]
compileAssign lhs rhs = hsep [ compile lhs, text "=", compile rhs ]
