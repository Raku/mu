{-# OPTIONS_GHC -fglasgow-exts #-}

module Pugs.Compile.PIR (genPIR') where
import Pugs.Internals
import Pugs.AST
import Pugs.Pretty
import Emit.PIR
import Text.PrettyPrint

data PAST a where
    PVal        :: Val -> PAST Literal
    PVar        :: VarName -> PAST Identifier
    PLit        :: PAST Literal -> PAST Identifier
    PStmts      :: PAST Stmt -> PAST [Stmt] -> PAST [Stmt]
    PNil        :: PAST [a]
    PNoop       :: PAST Stmt
    PPos        :: Pos -> PAST Stmt
    PApp        :: PAST Identifier -> [PAST Identifier] -> PAST Stmt

data TEnv = MkTEnv
    { tDepth    :: Int
    , tEnv      :: Env
    , tReg      :: TVar Int
    }
    deriving (Show, Eq)

type Comp a = Eval a
type Trans a = WriterT [Stmt] (ReaderT TEnv IO) a

class Compile x y where
    compile :: x -> Comp (PAST y)
    -- compile x = fail ("Unrecognized construct: " ++ show x)

{-
instance Compile Exp PAST where
    compile (Stmts this rest) = do
        thisC <- compile this
        restC <- compile rest
        return $ thisC $+$ restC


class Translate x y | x -> y where
    trans :: x -> Trans y

instance Translate PAST [Stmt] where
    trans 

-}

instance Compile Exp [Stmt] where
    compile (Stmts first rest) = do
        firstC  <- compile first
        restC   <- case rest of
            Stmts _ _   -> compile rest
            Noop        -> return PNil
            _           -> compile (Stmts rest Noop)
        return $ PStmts firstC restC
    compile exp = do
        liftIO $ do
            putStrLn "*** Unknown expression:"
            putStr "    "
            putStrLn $ show exp
        return PNil

instance Compile Exp Stmt where
    compile Noop = do
        pos <- asks envPos
        return $ PPos pos
    compile (Pos pos stmt) = do
        local (\e -> e{ envPos = pos }) $ compile stmt
    compile (Val val) = do
        lit     <- compile val :: Comp (PAST Literal)
        warn "Literal value used in constant expression" val
        compile Noop
    compile (App fun Nothing args) = do
        funC    <- compile fun
        argsC   <- mapM compile args
        return $ PApp funC argsC
    compile exp = error ("invalid exp: " ++ show exp)

instance Compile Exp Identifier where
    compile (Var name) = return $ PVar name
    compile (Val val) = fmap PLit (compile val)
    compile (Pos pos exp) = do
        local (\e -> e{ envPos = pos }) $ compile exp
    compile exp = error ("invalid ident: " ++ show exp)

instance Compile Val Literal where
    compile val = return $ PVal val

warn :: (MonadIO m, Show a) => String -> a -> m ()
warn str val = liftIO $ do
    hPutStrLn stderr $ "*** " ++ str ++ ": " ++ show val

trans :: PAST a -> Trans a
trans PNil = return []
trans PNoop = return (StmtComment "")
trans (PPos pos) = do
    tell [StmtLine (posName pos) (posBeginLine pos)]
    trans PNoop
trans (PLit lit) = do
    -- generate fresh supply and things...
    litC    <- trans lit
    tvar    <- asks tReg
    pmc     <- liftIO $ liftSTM $ do
        cur <- readTVar tvar
        writeTVar tvar (cur + 1)
        return cur
    tell    [ StmtIns $ InsNew (PMC pmc) PerlUndef
            , StmtIns $ InsAssign (PMC pmc) litC
            ]
    return (PMC pmc)
trans (PVal (VStr str)) = return $ LitStr str
trans (PVal _) = error "Unknown val"
trans (PVar var) = error "Unknown var"
trans (PApp (PVar name) args) = do
    argsC <- mapM trans args
    return $ StmtIns $ InsFun [] name argsC
trans (PStmts this rest) = do
    thisC <- trans this
    restC <- trans rest
    tell (thisC:restC)
    return []

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

