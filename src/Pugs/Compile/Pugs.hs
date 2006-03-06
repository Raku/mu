{-# OPTIONS_GHC -fglasgow-exts #-}

module Pugs.Compile.Pugs (genPugs) where
import Pugs.AST
import Pugs.Types
import Pugs.Internals
import qualified Data.FastPackedString as Str
import qualified Data.Map as Map

type Str = Str.FastString
type Comp a = WriterT [a] Eval a

class (Show x) => Compile x where
    compile :: x -> Comp Str
    compile x = fail ("Unrecognized construct: " ++ show x)
    compileList :: [x] -> Comp Str
    compileList xs = do
        xsC <- mapM compile xs
        return $ Str.concat [bl, joinMany xsC, br]

joinMany :: [Str] -> Str
joinMany xs = Str.join cm (filter (not . Str.null) xs)

instance (Compile x) => Compile [x] where
    compile = compileList

instance Compile (Maybe Exp) where
    compile Nothing = return $ Str.pack "Nothing"
    compile (Just exp) = compWith "Just" [compile exp]

pl, pr, bl, br :: Str
pl = Str.pack "("
pr = Str.pack ")"
bl = Str.pack "["
br = Str.pack "]"
cm = Str.pack ", "

ret :: String -> Comp Str
ret = return . Str.pack

compWith :: String -> [Comp Str] -> Comp Str
compWith con xs = do
    xsC <- sequence xs
    return $ Str.concat [pl, Str.unwords (Str.pack con:concatMap (\x -> [pl, x, pr]) xsC), pr]

instance Compile Exp where
    compile (App exp1 exp2 exps) = do
        compWith "App" [compile exp1, compile exp2, compile exps]
    compile (Syn syn exps) = do
        compWith "Syn" [ret (show syn), compile exps]
    compile (Ann ann exp) = do
        compWith "Ann" [ret (show ann), compile exp]
    compile (Pad scope pad exp) = do
        compWith "Pad" [ret (show scope), compile pad, compile exp]
    compile (Stmts exp1 exp2) = do
        compWith "Stmts" [compile exp1, compile exp2]
    compile (Val val) = do
        compWith "Val" [compile val]
    compile exp = ret $ "(" ++ show exp ++ ")"

instance Compile Pad where
    compile pad = do
        symsC <- mapM compile syms
        return $ Str.concat [Str.pack "(mkPad [", joinMany symsC, Str.pack "])"]
        where
        syms = padToList pad

instance Compile (String, [(TVar Bool, TVar VRef)]) where
    compile ((':':'*':_), _) = return Str.empty -- XXX - :*Bool etc; punt for now
    compile (n, tvars) = do
        tvarsC <- fmap (filter (not . Str.null)) $ mapM compile tvars
        if null tvarsC then return Str.empty else do
        return $ Str.concat [pl, Str.pack (show n), Str.pack ", [", joinMany tvarsC, br, pr]

instance (Typeable a) => Compile (Maybe (TVar a)) where
    compile = const . ret $ "Nothing"

instance Compile (TVar Bool, TVar VRef) where
    compile (fresh, tvar) = do
        tvarC  <- compile tvar
        if Str.null tvarC then return Str.empty else do
        freshC <- compile fresh
        return $ Str.concat [pl, freshC, cm, tvarC, pr]

instance Compile Bool where
    compile bool = ret $ "(" ++ show bool ++ ")"

instance Compile a => Compile (Map VStr a) where
    compile map | Map.null map = ret $ "(Map.empty)"
    compile map = error (show map) 

instance Compile (IVar VScalar) where
    compile iv = do
        val     <- lift $ readIVar iv
        valC    <- compile val
        return $ Str.concat [Str.pack "(newScalar ", valC, pr]

instance (Typeable a, Compile a) => Compile (TVar a) where
    compile fresh = do
        vref    <- liftIO $ atomically (readTVar fresh)
        vrefC   <- compile vref
        if Str.null vrefC then return Str.empty else do
        tv      <- liftIO $ fmap (Str.pack . ('t':) . show . hashUnique) newUnique
        tell [Str.concat [tv, Str.pack " <- liftSTM (newTVar ", vrefC, Str.pack ");\n"]]
        return tv

instance Compile VRef where
    compile (MkRef (ICode cv)) = do
        vsub    <- lift $ code_fetch cv
        vsubC   <- compile vsub
        if Str.null vsubC then return Str.empty else do
        return $ Str.concat [Str.pack "(MkRef (ICode ", vsubC, pr, pr]
    compile (MkRef (IScalar sv)) | scalar_iType sv == mkType "Scalar::Const" = do
        sv  <- lift $ scalar_fetch sv
        svC <- compile sv
        if Str.null svC then return Str.empty else do
        return $ Str.concat [Str.pack "(MkRef (IScalar ", svC, pr, pr]
    compile ref = do
        objc   <- liftIO $ fmap (Str.pack . ('o':) . show . hashUnique) newUnique
        tell [Str.append objc (Str.pack (" <- newObject (mkType \"" ++ showType (refType ref) ++ "\");\n"))]
        return objc

instance Compile Val where
    compile (VCode code) = do
        compWith "VCode" [compile code]
    compile (VObject obj) = do
        compWith "VObject" [compile obj]
    compile val = ret $ "(" ++ show val ++ ")"

instance Compile VObject where
    compile (MkObject typ attrs Nothing _) = do
        attrsC <- compile attrs
        uniq   <- liftIO $ fmap (Str.pack . ('u':) . show . hashUnique) newUnique
        tell [Str.append uniq (Str.pack " <- liftIO newUnique;\n")]
        return $ Str.unwords [pl, Str.pack "MkObject", Str.pack (show typ), attrsC, Str.pack "Nothing", uniq, pr]
    compile obj = fail $ "Cannot compile Object of Dynamic type: " ++ show obj

-- Haddock can't cope with Template Haskell
instance Compile VCode where
    -- compile MkCode{ subBody = Prim _ } = return $ text "return mkPrim"
    compile MkCode{ subBody = Prim _ } = return Str.empty
    compile (MkCode v1 v2 v3 _ v4 v5 v6 v7 v8 v9 v10 _) = do 
        compWith "MkCode"
            [ compile v1
            , ret (show v2)
            , ret (show v3)
            , ret "Nothing"
            , ret (show v4)
            , ret (show v5)
            , ret (show v6)
            , ret (show v7)
            , ret (show v8)
            , compile v9
            , compile v10
            , ret "Nothing"
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
        , "globC = do {" ++ Str.unpack (Str.concat globT) ++ "return " ++ Str.unpack globC ++ "}"
        , ""
        , "expC = do {" ++ Str.unpack (Str.concat expT) ++ "return " ++ Str.unpack expC ++ "}"
        , ""
        ]
