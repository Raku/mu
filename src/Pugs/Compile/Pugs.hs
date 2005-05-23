{-# OPTIONS_GHC -fglasgow-exts #-}

module Pugs.Compile.Pugs (genPugs) where
import Pugs.AST
import Pugs.Types
import Pugs.Internals
import Text.PrettyPrint

class (Show x) => Compile x where
    compile :: x -> Eval Doc
    compile x = fail ("Unrecognized construct: " ++ show x)

instance Compile Exp where
    compile (Syn syn exps) = do
        expsC <- mapM compile exps
        return $ parens $
            text "do" <+> (braces $ vcat (punctuate (text ";")
                [ text "exps <- sequence $ " <+> brackets (vcat (punctuate (text ",") expsC))
                , text ("return (Syn " ++ show syn ++ " exps)")
                ]))
    compile (Stmts exp1 exp2) = do
        exp1C <- compile exp1
        exp2C <- compile exp2
        return $ parens $
            text "do" <+> (braces $ vcat (punctuate (text ";")
                [ text "exp1 <-" <+> exp1C
                , text "exp2 <-" <+> exp2C
                , text "return (Stmts exp1 exp2)"
                ]))
    compile (Pad scope pad exp) = do
        padC <- compile pad
        expC <- compile exp
        return $ parens $
            text "do" <+> (braces $ vcat (punctuate (text ";")
                [ text "pad <-" <+> padC
                , text "exp <-" <+> expC
                , text ("return (Pad " ++ show scope ++ " pad exp)")
                ]))
    compile exp = return $ text "return" $+$ parens (text $ show exp)

instance Compile Pad where
    compile pad = do
        symsC <- mapM compile syms
        return $ text "fmap mkPad . sequence $ "
            $+$ nest 4 (brackets $ vcat (punctuate (text ",") $ filter (not . isEmpty) symsC))
        where
        syms = padToList pad

instance Compile (String, [(TVar Bool, TVar VRef)]) where
    compile ((_:'?':_), _) = return empty -- XXX - @?INIT etc; punt for now
    compile ((_:'=':_), _) = return empty -- XXX - @=POS etc; punt for now
    compile (n, tvars) = do
        tvarsC <- mapM compile tvars
        return $ parens $
            text "do" <+> (braces $ vcat (punctuate (text ";")
                [ text "tvars <- sequence" <+> (brackets $ vcat (punctuate (text ",") tvarsC))
                , text ("return (" ++ show n ++ ", tvars)")
                ]))

instance Compile (TVar Bool, TVar VRef) where
    compile (fresh, tvar) = do
        freshC <- compile fresh
        tvarC  <- compile tvar
        return $ parens $
            text "do" <+> (braces $ vcat (punctuate (text ";")
                [ text "fresh <-" <+> freshC
                , text "tvar <-" <+> tvarC
                , text "return (fresh, tvar)"
                ]))

instance Compile (TVar Bool) where
    compile fresh = do
        bool <- liftSTM $ readTVar fresh
        return $ text "liftSTM" <+> parens (text "newTVar" <+> text (show bool))

instance Compile (TVar VRef) where
    compile fresh = do
        vref    <- liftSTM $ readTVar fresh
        vrefC   <- compile vref
        return $ parens $
            text "do" <+> (braces $ vcat (punctuate (text ";")
                [ text "vref <-" <+> vrefC
                , text "liftSTM (newTVar vref)"
                ]))

instance Compile VRef where
    compile (MkRef (ICode cv)) = do
        vsub <- code_fetch cv
        return $ text $ "return (MkRef (ICode $ " ++ show vsub ++ "))"
    compile (MkRef (IScalar sv)) | scalar_iType sv == mkType "Scalar::Const" = do
        sv <- scalar_fetch sv
        return $ text $ "return (MkRef (IScalar $ " ++ show sv ++ "))"
    compile ref = do
        return $ text $ "newObject (mkType \"" ++ showType (refType ref) ++ "\")"

genPugs :: Eval Val
genPugs = do
    exp     <- asks envBody
    glob    <- askGlobal
    globC   <- compile glob
    expC    <- compile exp
    return . VStr . unlines $
        [ "{-# OPTIONS_GHC -fglasgow-exts -fno-warn-unused-imports -fno-warn-unused-binds -O #-}"
        , "module MainCC where"
        , "import Pugs.Run"
        , "import Pugs.AST"
        , "import Pugs.Types"
        , "import Pugs.Internals"
        , ""
        , "mainCC = do"
        , "    glob <- globC"
        , "    exp  <- expC"
        , "    runAST glob exp"
        , ""
        , renderStyle (Style PageMode 0 0) $ text "globC =" <+> globC
        , ""
        , renderStyle (Style PageMode 0 0) $ text "expC =" <+> expC
        , ""
        ]
