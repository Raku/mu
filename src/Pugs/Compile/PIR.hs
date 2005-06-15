{-# OPTIONS_GHC -fglasgow-exts #-}

module Pugs.Compile.PIR (genPIR') where
import Pugs.Internals
import Pugs.AST
import Emit.PIR
import Pugs.Pretty
import Text.PrettyPrint

data PAST a where
    PVal        :: Val -> PAST Literal
    PVar        :: VarName -> PAST LValue
    PExp        :: PAST LValue -> PAST Expression 
    PLit        :: PAST Literal -> PAST Expression
    PStmts      :: PAST Stmt -> PAST [Stmt] -> PAST [Stmt]
    PNil        :: PAST [a]
    PNoop       :: PAST Stmt
    PPos        :: Pos -> PAST Stmt
    PStmt       :: PAST Expression -> PAST Stmt 
    PApp        :: PAST Expression -> [PAST Expression] -> PAST Expression
    PAssign     :: [PAST LValue] -> PAST Expression -> PAST Expression
    PBind       :: [PAST LValue] -> PAST Expression -> PAST Expression
    PPad        :: [(VarName, PAST Expression)] -> PAST [Stmt] -> PAST [Stmt]

instance Show (PAST a) where
    show (PVal x) = "(PVal " ++ show x ++ ")"
    show (PVar x) = "(PVar " ++ show x ++ ")"
    show (PLit x) = "(PLit " ++ show x ++ ")"
    show (PStmts x y) = "(PStmts " ++ show x ++ " " ++ show y ++ ")"
    show PNil = "PNil"
    show PNoop = "PNoop"
    show (PPos x) = "(PPos " ++ show x ++ ")"
    show (PApp x y) = "(PApp " ++ show x ++ " " ++ show y ++ ")"
    show (PExp x) = "(PExp " ++ show x ++ ")"
    show (PStmt x) = "(PStmt " ++ show x ++ ")"
    show (PAssign x y) = "(PAssign " ++ show x ++ " " ++ show y ++ ")"
    show (PBind x y) = "(PBind " ++ show x ++ " " ++ show y ++ ")"
    show (PPad x y) = "(PPad " ++ show x ++ " " ++ show y ++ ")"

data TEnv = MkTEnv
    { tDepth    :: Int
    , tEnv      :: Env
    , tReg      :: TVar Int
    }
    deriving (Show, Eq)

type Comp a = Eval a
type Trans a = WriterT [Stmt] (ReaderT TEnv IO) a

class (Show x) => Compile x y where
    compile :: x -> Comp (PAST y)
    compile x = fail ("Unrecognized construct: " ++ show x)

class Translate x y | x -> y where
    trans :: x -> Trans y
    trans _ = fail "Untranslatable construct!"

instance Compile [(TVar Bool, TVar VRef)] Expression where
    compile _ = return (PLit $ PVal undef)

instance Compile Exp [Stmt] where
    compile (Stmts (Pad SMy pad exp) rest) = do
        expC    <- compile $ mergeStmts exp rest
        padC    <- mapM (compile . snd) (padToList pad)
        return $ PPad ((map fst (padToList pad)) `zip` padC) expC
    compile (Stmts first rest) = do
        firstC  <- compile first
        restC   <- compileRest rest
        return $ PStmts firstC restC
    compile exp = do
        liftIO $ do
            putStrLn "*** Unknown expression:"
            putStr "    "
            putStrLn $ show exp
        return PNil

compileRest rest = case rest of
    Stmts _ _   -> compile rest
    Noop        -> return PNil
    _           -> compile (Stmts rest Noop)

instance Compile Exp Stmt where
    compile Noop = do
        pos <- asks envPos
        return $ PPos pos
    compile (Pos pos stmt) = do
        local (\e -> e{ envPos = pos }) $ compile stmt
    compile (Val val) = do
        compile val :: Comp (PAST Literal)
        warn "Literal value used in constant expression" val
        compile Noop
    compile exp = fmap PStmt $ compile exp
    -- compile exp = error ("invalid stmt: " ++ show exp)

instance Compile Exp LValue where
    compile (Var name) = return $ PVar name
    compile exp = error ("invalid lv: " ++ show exp)

instance Compile Exp Expression where
    compile (Var name) = return . PExp $ PVar name
    compile (Val val) = fmap PLit (compile val)
    compile (Pos pos exp) = do
        local (\e -> e{ envPos = pos }) $ compile exp
    compile (App fun Nothing args) = do
        funC    <- compile fun
        argsC   <- mapM compile args
        return $ PApp funC argsC
    compile Noop = compile (Val undef)
    compile (Syn "=" [lhs, rhs]) = do
        lhsC    <- compile lhs
        rhsC    <- compile rhs
        return $ PAssign [lhsC] rhsC
    compile exp = error ("invalid exp: " ++ show exp)

instance Compile Val Literal where
    compile val = return $ PVal val

warn :: (MonadIO m, Show a) => String -> a -> m ()
warn str val = liftIO $ do
    hPutStrLn stderr $ "*** " ++ str ++ ": " ++ show val

instance Translate (PAST a) a where
    trans PNil = return []
    trans PNoop = return (StmtComment "")
    trans (PPos pos) = do
        tell [StmtLine (posName pos) (posBeginLine pos)]
        trans PNoop
    trans (PLit (PVal VUndef)) = do
        pmc     <- genLV
        return (ExpLV pmc)
    trans (PLit lit) = do
        -- generate fresh supply and things...
        litC    <- trans lit
        pmc     <- genLV
        tellIns $ InsAssign pmc (ExpLit litC)
        return (ExpLV pmc)
    trans (PVal (VStr str)) = return $ LitStr str
    trans (PVal (VInt int)) = return $ LitInt int
    trans (PVal (VNum num)) = return $ LitNum num
    trans (PVal (VRat rat)) = return $ LitNum (ratToNum rat)
    trans (PVal val) = do
        transError "Unknown val" val
    trans (PVar name) = do
        pmc     <- genLV
        tellIns $ pmc <-- "find_name" $ [lit name]
        return pmc
    trans (PStmt (PApp (PExp (PVar name)) args)) = do
        argsC   <- mapM trans args
        return $ StmtIns $ InsFun [] (lit name) argsC
    trans (PStmt (PLit (PVal VUndef))) = return $ StmtComment ""
    trans (PStmt exp) = do
        expC    <- trans exp
        return $ StmtIns $ InsExp expC
    trans (PAssign [lhs] rhs) = do
        lhsC    <- trans lhs
        rhsC    <- trans rhs
        tellIns $ InsAssign lhsC rhsC
        return (ExpLV lhsC)
    trans (PStmts this rest) = do
        thisC   <- trans this
        restC   <- trans rest
        tell (thisC:restC)
        return []
    trans (PApp fun args) = do
        funC    <- trans fun
        argsC   <- mapM trans args
        pmc     <- genLV
        tellIns $ [slurpy $ ExpLV pmc] <-& funC $ argsC
        return (ExpLV pmc)
    trans (PPad pad exps) = do
        valsC   <- mapM trans (map snd pad)
        pass $ do
            expsC   <- trans exps
            return ([], (StmtPad (map fst pad `zip` valsC) expsC:))
    trans (PExp exp) = fmap ExpLV $ trans exp
    trans x = transError "Unknown exp" x

tellIns = tell . (:[]) . StmtIns

genLV :: (RegClass a) => Trans a
genLV = do
    tvar    <- asks tReg
    pmc     <- liftIO $ liftSTM $ do
        cur <- readTVar tvar
        writeTVar tvar (cur + 1)
        return (PMC cur)
    tell [StmtIns $ InsNew pmc PerlUndef]
    return $ reg pmc

transError :: (Show a) => String -> a -> Trans b
transError str val = fail $ "*** Trans error: " ++ str ++ ": " ++ show val

genPIR' :: Eval Val
genPIR' = do
    env         <- ask
    past        <- compile (envBody env) :: (Eval (PAST [Stmt]))
    zero        <- liftSTM $ newTVar 100
    (_, pir)    <- liftIO $ (`runReaderT` MkTEnv 0 env zero) $ runWriterT (trans past)
    return . VStr . unlines $
        [ "#!/usr/bin/env parrot"
        -- , renderStyle (Style PageMode 0 0) init
        , renderStyle (Style PageMode 0 0) $ preludePIR $+$ vcat
            [ text ".sub main @MAIN"
            , text "    new_pad 0"
            , nest 4 (emit pir)
            , text ".end"
            ]
        ]

