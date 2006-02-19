{-# OPTIONS_GHC -fglasgow-exts #-}

module Pugs.Compile.Pugs (genPugs) where
import Pugs.AST
import Pugs.Types
import Pugs.Internals
import Text.PrettyPrint
import qualified Data.FastPackedString as Str
import qualified Data.Map as Map

type Str = Str.FastString
type Comp a = WriterT a Eval a

class (Show x) => Compile x where
    compile :: x -> Comp String
    compile x = fail ("Unrecognized construct: " ++ show x)
    compileList :: [x] -> Comp String
    compileList xs = do
        xsC <- mapM compile xs
        return $ "[" ++ joinMany xsC ++ "]"

joinMany :: [String] -> String
joinMany xs = concat (intersperse ", " (filter (not . null) xs))

instance (Compile x) => Compile [x] where
    compile = compileList


instance Compile (Maybe Exp) where
    compile Nothing = return "Nothing"
    compile (Just exp) = compWith "Just" [compile exp]

compWith :: String -> [Comp String] -> Comp String
compWith con xs = do
    xsC <- sequence xs
    return $ "(" ++ unwords (con:["("++x++")" | x <- xsC]) ++ ")"

instance Compile Exp where
    compile (App exp1 exp2 exps) = do
        compWith "App" [compile exp1, compile exp2, compile exps]
    compile (Syn syn exps) = do
        compWith "Syn" [return (show syn), compile exps, compile exps]
    compile (Ann ann exp) = do
        compWith "Ann" [return (show ann), compile exp]
    compile (Pad scope pad exp) = do
        compWith "Pad" [return (show scope), compile pad, compile exp]
    compile (Stmts exp1 exp2) = do
        compWith "Stmts" [compile exp1, compile exp2]
    compile (Val val) = do
        compWith "Val" [compile val]
    compile exp = return $ "(" ++ show exp ++ ")"

instance Compile Pad where
    compile pad = do
        symsC <- mapM compile syms
        return $ "(mkPad [" ++ joinMany symsC ++ "])"
        where
        syms = padToList pad

instance Compile (String, [(TVar Bool, TVar VRef)]) where
    compile ((':':'*':_), _) = return [] -- XXX - :*Bool etc; punt for now
    compile (n, tvars) = do
        tvarsC <- fmap (filter (not . null)) $ mapM compile tvars
        if null tvarsC then return [] else do
        return $ "(" ++ show n ++ ", [" ++ joinMany tvarsC ++ "])"

instance (Typeable a) => Compile (Maybe (TVar a)) where
    compile = const . return $ "Nothing"

instance Compile (TVar Bool, TVar VRef) where
    compile (fresh, tvar) = do
        tvarC  <- compile tvar
        if null tvarC then return [] else do
        freshC <- compile fresh
        return $ "(" ++ freshC ++ ", " ++ tvarC ++ ")"

instance Compile Bool where
    compile bool = return $ "(" ++ show bool ++ ")"

instance Compile a => Compile (Map VStr a) where
    compile map | Map.null map = return $ "(Map.empty)"
    compile map = error (show map) 

instance Compile (IVar VScalar) where
    compile iv = do
        val     <- lift $ readIVar iv
        valC    <- compile val
        return $ "(newScalar " ++ valC ++ ")"

instance (Typeable a, Compile a) => Compile (TVar a) where
    compile fresh = do
        vref    <- liftIO $ atomically (readTVar fresh)
        vrefC   <- compile vref
        if null vrefC then return [] else do
        tv      <- liftIO $ fmap (('t':) . show . hashUnique) newUnique
        tell $ tv ++ " <- liftSTM (newTVar " ++ vrefC ++ ");\n"
        return tv

instance Compile VRef where
    compile (MkRef (ICode cv)) = do
        vsub    <- lift $ code_fetch cv
        vsubC   <- compile vsub
        if null vsubC then return [] else do
        return $ "(MkRef (ICode " ++ vsubC ++ "))"
    compile (MkRef (IScalar sv)) | scalar_iType sv == mkType "Scalar::Const" = do
        sv  <- lift $ scalar_fetch sv
        svC <- compile sv
        if null svC then return [] else do
        return $ "(MkRef (IScalar " ++ svC ++ "))"
    compile ref = do
        objc   <- liftIO $ fmap (('o':) . show . hashUnique) newUnique
        tell $ objc ++ " <- newObject (mkType \"" ++ showType (refType ref) ++ "\");\n"
        return objc

instance Compile Val where
    compile (VCode code) = do
        compWith "VCode" [compile code]
    compile (VObject obj) = do
        compWith "VObject" [compile obj]
    compile val = return $ "(" ++ show val ++ ")"

instance Compile VObject where
    compile (MkObject typ attrs Nothing _) = do
        attrsC <- compile attrs
        uniq   <- liftIO $ fmap (('u':) . show . hashUnique) newUnique
        tell $ uniq ++ " <- liftIO newUnique;\n"
        return $ "(" ++ unwords ["MkObject", show typ, attrsC, "Nothing", uniq] ++ ")"
    compile obj = fail $ "Cannot compile Object of Dynamic type: " ++ show obj

-- Haddock can't cope with Template Haskell
instance Compile VCode where
    -- compile MkCode{ subBody = Prim _ } = return $ text "return mkPrim"
    compile MkCode{ subBody = Prim _ } = return []
    compile (MkCode v1 v2 v3 _ v4 v5 v6 v7 v8 v9 v10 _) = do 
        compWith "MkCode"
            [ compile v1
            , return (show v2)
            , return (show v3)
            , return (show v4)
            , return "Nothing"
            , return (show v5)
            , return (show v6)
            , return (show v7)
            , return (show v8)
            , compile v9
            , compile v10
            , return "Nothing"
            ]

genPugs :: Eval Val
genPugs = do
    exp             <- asks envBody
    glob            <- askGlobal
    (globC, globT)  <- runWriterT $ compile glob
    (expC, expT)    <- runWriterT $ compile exp
    return . VStr . unlines $
        [ "{-# OPTIONS_GHC -fglasgow-exts -fno-warn-unused-imports -fno-warn-unused-binds #-}"
        , "module Main where"
        , "import Pugs.Run"
        , "import Pugs.AST"
        , "import Pugs.Types"
        , "import Pugs.Internals"
        , "import qualified Data.Map as Map"
        , ""
        , "main = do"
        , "    glob <- globC"
        , "    exp  <- expC"
        , "    runAST glob exp"
        , ""
        , "globC = do {" ++ globT ++ "return " ++ globC ++ "}"
        , ""
        , "expC = do {" ++ expT ++ "return " ++ expC ++ "}"
        , ""
        ]
