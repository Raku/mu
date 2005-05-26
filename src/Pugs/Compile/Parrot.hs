{-# OPTIONS_GHC -fglasgow-exts #-}

module Pugs.Compile.Parrot (genPIR) where
import Pugs.Internals
import Pugs.Pretty
import Pugs.AST
import Pugs.Types
import Pugs.Eval
import Text.PrettyPrint
import qualified Data.Map       as Map

-- XXX This compiler needs a totaly rewrite using Parrot AST,
-- XXX and maybe TH-based AST combinators

class (Show x) => Compile x where
    compile :: x -> Eval Doc
    compile x = fail ("Unrecognized construct: " ++ show x)

genPIR :: Eval Val
genPIR = do
    exp  <- asks envBody
    glob <- askGlobal
    ref  <- liftSTM $ newTVar $ Map.fromList [("tempPMC", "9")]

    -- get a list of functions
    local (\e -> e{ envDebug = Just ref }) $ do

    pmc  <- askPMC
    init <- local (\e -> e{ envStash = pmc }) $ compile glob

    pmc  <- askPMC
    main <- local (\e -> e{ envStash = pmc }) $ compile exp

    return . VStr . unlines $
        [ "#!/usr/bin/env parrot"
        , renderStyle (Style PageMode 0 0) init
        , renderStyle (Style PageMode 0 0) $ vcat
            [ text ".sub main @MAIN"
            , nest 4 main
            , text ".end"
            ]
        ]

instance Compile Doc where
    compile = return

padSort :: (Var, [(TVar Bool, TVar VRef)]) -> (String, [(a, b)]) -> Ordering
padSort ((a::[Char]), [(_, _)]) ((b::[Char]), [(_, _)])
    | (head a == ':' && head b == '&') = LT
    | (head b == ':' && head a == '&') = GT
    | otherwise = GT
padSort _ _ = EQ

instance Compile Pad where
    {- XXX The padSort will misplace multiple namespaces in the same pad.
           We *should* compile the namespaces & subs in order of declaration.
    -}   
    compile pad = do
        fmap vcat $ mapM compile (sortBy padSort $ padToList pad)

instance Compile (Var, [(TVar Bool, TVar VRef)]) where
    compile ((_:'?':_), _) = return empty -- XXX - @?INIT etc; punt for now
    compile ((_:'=':_), _) = return empty -- XXX - @=POS etc; punt for now
    compile (('&':name), [(_, sym)]) = do
        ret <- askPMC
        imc <- compile sym
        return $ vcat
            [ text (".sub \"" ++ name ++ "\"")
            , nest 4 imc
            , text $ ".return (" ++ ret ++ ")"
            , text ".end"
            ]
    compile ((':':'*':name), [(_, _)]) =
        return $ text ".namespace" <+> text "['" <> text name <> text "']"
    -- compile v = error $ show v
    compile _ = return $ empty

instance Compile (TVar VRef) where
    compile x = do
        ref <- liftSTM $ readTVar x
        compile ref

instance Compile VRef where
    compile (MkRef (ICode cv)) = do
        vsub <- code_fetch cv
        compile vsub
    compile (MkRef (IScalar sv))
        | scalar_iType sv == mkType "Scalar::Const" = do
            sv  <- scalar_fetch sv
            ref <- fromVal sv
            compile (ref :: VCode)
    compile x = internalError ("Unrecognized construct: " ++ show x)

instance Compile VCode where
    compile sub = do
        prms <- mapM compile (subParams sub)
        body <- compile (subBody sub)
        return . vcat $ prms ++ [ text "", body ]

instance Compile Param where
    compile prm = return $ text ".param pmc" <+> varText (paramName prm)

varText :: String -> Doc
varText ('$':name)  = text $ "s__" ++ name
varText ('@':name)  = text $ "a__" ++ name
varText ('%':name)  = text $ "h__" ++ name
varText x           = error $ "invalid name: " ++ x

varInit :: String -> Doc
varInit ('$':_) = text $ "PerlUndef"
varInit ('@':_) = text $ "PerlArray"
varInit ('%':_) = text $ "PerlHash"
varInit x       = error $ "invalid name: " ++ x

askPMC :: Eval String
askPMC = do
    Just ioRef <- asks envDebug
    fm <- liftSTM $ readTVar ioRef
    let cnt = Map.findWithDefault "0" "tempPMC" fm
    return $ "$P" ++ cnt

tempPMC :: Eval Doc
tempPMC = incCounter "tempPMC" ("$P" ++)

tempLabels :: [String] -> Eval [Doc]
tempLabels strs = do
    tmp <- incCounter "label" ("LABEL_" ++)
    return $ map ((tmp <> text "_" <>) . text) strs

incCounter :: String -> (String -> String) -> Eval Doc
incCounter key f = do
    Just ioRef <- asks envDebug
    liftSTM $ do
        fm <- readTVar ioRef
        let cnt = Map.findWithDefault "0" key fm
            cnt' = show (read cnt + (1 :: Int))
        writeTVar ioRef (Map.insert key cnt' fm)
        return $ text (f cnt')

instance Compile Pos where
    compile MkPos{ posName = file, posBeginLine = line } = return $ hsep $
        [ text "#line"
        , doubleQuotes $ text file
        , showText line
        ]


label :: Doc -> Doc
label doc = doc <> text ":"

compileCond :: Compile a => String -> [a] -> Eval Doc
compileCond neg [cond, bodyIf, bodyElse] = do
    [alt, end]  <- tempLabels ["else", "endif"]
    (condC, p)  <- compileArg cond
    (ifC, _)    <- compileArg bodyIf
    (elseC, _)  <- compileArg bodyElse
    return $ vcat $
        [ condC
        , text neg <+> p <+> text "goto" <+> alt
        , ifC
        , text "goto" <+> end
        , label alt
        , elseC
        , label end
        ]
compileCond x y = error $ show (x,y)

instance Compile Exp where
    compile (Var name) = do
        lv <- asks envLValue
        let p = varText name
        constPMC (if lv then p else text "assign" <+> p)
    compile (Syn ";" stmts) = fmap vcat $ mapM compile stmts
    compile (Syn "block" blocks) = fmap vcat $ mapM compile blocks
    compile (Syn "=" [lhs, rhs]) = do
        (lhsC, p1) <- enterLValue $ compileArg lhs
        (rhsC, p2) <- enterRValue $ compileArg rhs
        p <- constPMC p1
        return $ vcat [ lhsC, rhsC, p1 <+> text "= assign" <+> p2, p ]
    compile (Syn "if" exps) = compileCond "unless" exps
    compile (Syn "unless" exps) = compileCond "if" exps
    compile (Syn "loop" [pre, cond, post, body]) = do
        [start, end, last] <- tempLabels ["start", "end", "last"]
        preC  <- compile pre
        bodyC <- compile body
        postC <- compile post
        condC <- compile cond
        return $ vcat $
            [ preC
            , text "goto" <+> end
            , label start
            , text ".local pmc last"
            , text "last = new Continuation"
            , text "set_addr last," <+> last
            , bodyC
            , postC
            , label end
            , condC
            , text "goto" <+> start
            , label last
            ]
    -- XXX "module" is handled in glob, need stub here to avoid compile error
    compile (Syn "module" [Val (VStr _)]) = return empty
    compile (App (Var "&return") Nothing [val]) = do
        (valC, p) <- compileArg val
        return $ valC $+$ text ".return" <+> parens p
    compile (App (Var "&last") _ _) = return $ text "invoke last"
    compile (App (Var "&substr") Nothing [str, idx, len])
        | Val v <- unwrap len, vCast v == (1 :: VNum) = do
        (strC, p1) <- enterLValue $ compileArg str
        (idxC, p2) <- enterLValue $ compileArg idx
        rv         <- constPMC $ hcat [ p1, text "[" , p2, text "]"]
        return $ vcat [strC, idxC, rv]
    compile (App (Var "&postfix:++") Nothing [inv]) = do
        (invC, p) <- enterLValue $ compileArg inv
        return $ invC $+$ text "inc" <+> p
    compile (App (Var "&postfix:--") Nothing [inv]) = do
        (invC, p) <- enterLValue $ compileArg inv
        return $ invC $+$ text "dec" <+> p
    -- compile (App "&infix:~" [exp, Val (VStr "")] []) = compile exp
    compile (App (Var "&infix:~") Nothing [exp1, exp2]) = do
        tmp <- currentStash
        (arg1, p1) <- compileArg exp1
        (arg2, p2) <- compileArg exp2
        return $ vcat $
            [ arg1
            , arg2
            , tmp <+> text "= new PerlUndef"
            , text "concat" <+> tmp <> comma <+> p1 <> comma <+> p2
            ]
    compile (App (Var ('&':'i':'n':'f':'i':'x':':':op)) Nothing [lhs, rhs]) = do
        (lhsC, p1) <- compileArg lhs
        (rhsC, p2) <- compileArg rhs
        rv  <- case op of
            --- XXX look at signature
            "<" -> do
                i <- constPMC (text "$I9")
                return $ text "$I9 =" <+> text "islt" <+> p1 <> comma <+> p2 $+$ i
            ">" -> do
                i <- constPMC (text "$I9")
                return $ text "$I9 =" <+> text "isgt" <+> p1 <> comma <+> p2 $+$ i
            _ -> do
                constPMC $ p1 <+> text op <+> p2
        return $ vcat [ lhsC, rhsC, rv ]
    -- XXX store return code in $@, whereever that may be in Parrotland
    compile (App (Var "&system") Nothing [cmd]) = do
        (arg, p) <- compileArg cmd
        rc <- constPMC (text "$I10")
        return $ vcat $ 
            [ arg
            , text "$S9" <+> text "=" <+> p
            , text "$I9" <+> text "=" <+> text "spawnw" <+> text "$S9"
            , text "$I10 = 0"
            , text "$I10 = iseq $I9, 0"
            , rc
            ]
    compile (App (Var "&require_parrot") Nothing [arg]) = do
        (path, p) <- compileArg arg
        return $ vcat $
            [ path
            , text "$S9" <+> text "=" <+> p
            , text "load_bytecode" <+> text "$S9"
            ]
    compile (App (Var "&say") invs args) = 
        compile $ App (Var "&print") invs (args ++ [Val $ VStr "\n"])
    compile (App (Var "&print") invs args) = do
        actions <- fmap vcat $ mapM (compileWith (text "print" <+>)) (maybeToList invs ++ args)
        rv      <- compile (Val (VBool True))
        return $ actions $+$ rv
    compile (App (Var ('&':method)) (Just (Var ('$':obj))) [arg]) = do
        lhsC <- askPMC
        compileWith (\tmp -> text lhsC <+> text "=" <+> varText ("$" ++ obj) <> text "." <> text ("'" ++ method ++ "'") <> parens tmp) arg
    compile (App (Var ('&':name)) Nothing [arg]) = do
        lhsC <- askPMC
        compileWith (\tmp -> text lhsC <+> text "=" <+> text name <> parens tmp) arg
    compile (App (Var "&not") Nothing []) = return $ text "new PerlUndef"
    compile (App (Var ('&':name)) Nothing []) = do
        lhsC <- askPMC
        return $ text lhsC <+> text "=" <+> text name <> text "()"
    compile (Val (VStr x))  = constPMC $ showText $ encodeUTF8 (concatMap quoted x)
    compile (Val (VInt x))  = constPMC $ integer x
    compile (Val (VNum x))  = constPMC $ showText x
    compile (Val (VRat x))  = constPMC $ showText $ ratToNum x
    compile (Val VUndef)    = constPMC $ text "PerlUndef"
    compile (Val (VBool True)) = constPMC $ text "1"
    compile (Val (VBool False)) = constPMC $ text "0"
    compile Noop            = return empty
    compile (Stmts this rest) = do
        thisC <- compile this
        restC <- compile rest
        return $ thisC $+$ restC
    {-fmap vcat $ sequence
        [ do
            posC  <- compile pos
            stmtC <- compile stmt
            return $ posC $+$ stmtC $+$ text ""
        | (stmt, pos) <- stmts
        ]
    -}
    compile (Sym _ name rest) = do
        restC <- compile rest
        return . ($+$ restC) $ vcat $
            [ text ".local" <+> text "pmc" <+> varText name
            , varText name <+> text "=" <+> text "new" <+> varInit name
            ]
    compile (Pad _ pad rest) = do
        restC <- compile rest
        return . ($+$ restC) $ vcat $ concat
            [ [ text ".local" <+> text "pmc" <+> varText name
              , varText name <+> text "=" <+> text "new" <+> varInit name
              ]
              | (name, _) <- padToList pad
            ]
    compile (Syn "mval" [exp]) = compile exp
    compile (Syn "," things) = fmap vcat $ mapM compile things
    compile (Syn syn [lhs, exp]) | last syn == '=' =
        compile $ Syn "=" [lhs, App (Var ("&infix:" ++ init syn)) Nothing [lhs, exp]]
    compile (Cxt _ exp) = compile exp
    compile (Pos pos exp) = do
	  posC <- compile pos
	  expC <- compile exp
	  return $ vcat [posC, expC]
    compile x = error $ "Cannot compile: " ++ (show x)

showText :: (Show a) => a -> Doc
showText = text . show

compileWith :: (Doc -> Doc) -> Exp -> Eval Doc
compileWith f x = do
    tmp  <- tempPMC
    pmc  <- askPMC
    argC <- local (\e -> e{ envStash = pmc }) $ compile x
    return $ vcat [ argC, f tmp ]

currentStash :: Eval Doc
currentStash = fmap text $ asks envStash

constPMC :: Doc -> Eval Doc
constPMC doc = do
    tmp  <- currentStash
    return $ vcat
        [ tmp <+> text "= new PerlUndef"
        , tmp <+> text "=" <+> doc
        ]

compileArg :: Compile a => a -> Eval (Doc, Doc)
compileArg exp = do
    tmp  <- tempPMC
    pmc  <- askPMC
    argC <- local (\e -> e{ envStash = pmc }) $ compile exp
    return (argC, tmp)
