{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans -funbox-strict-fields -cpp #-}

module Pugs.Compile.PIR (genPIR') where
import Pugs.Compile.Parrot
import Pugs.Internals
import Pugs.AST
import Pugs.AST.Internals
import Emit.Common
import Pugs.Types
import Emit.PIR
import Pugs.Pretty
import Text.PrettyPrint

#ifndef HADDOCK
data PAST a where
    PNil        :: PAST [a]
    PNoop       :: PAST Stmt

    PRaw        :: !Exp -> PAST Stmt -- XXX HACK!
    PRawName    :: !VarName -> PAST Expression -- XXX HACK!

    PExp        :: !(PAST LValue) -> PAST Expression 
    PLit        :: !(PAST Literal) -> PAST Expression
    PPos        :: !Pos -> !Exp -> PAST a -> PAST a
    PStmt       :: !(PAST Expression) -> PAST Stmt 
    PThunk      :: !(PAST Expression) -> PAST Expression 
    PBlock      :: !(PAST [Stmt]) -> PAST Expression 

    PVal        :: !Val -> PAST Literal
    PVar        :: !VarName -> PAST LValue

    PStmts      :: !(PAST Stmt) -> PAST [Stmt] -> PAST [Stmt]
    PApp        :: !PCxt -> !(PAST Expression) -> ![PAST Expression] -> PAST LValue
    PAssign     :: ![PAST LValue] -> !(PAST Expression) -> PAST Expression
    PBind       :: ![PAST LValue] -> !(PAST Expression) -> PAST Expression
    PPad        :: ![(VarName, PAST Expression)] -> !(PAST [Stmt]) -> PAST [Stmt]

    PSub        :: !SubName -> !(PAST [Stmt]) -> PAST Decl
#endif

data PCxt = PCxtVoid | PCxtLValue !Type | PCxtItem !Type | PCxtSlurpy !Type
    deriving (Show, Eq, Typeable)

{-
pcLV, pcItem, pcSlurpy :: PCxt
pcLV        = PCxtLValue anyType
pcItem      = PCxtItem anyType
pcSlurpy    = PCxtSlurpy anyType
-}

instance Show (PAST a) where
    show (PVal x) = "(PVal " ++ show x ++ ")"
    show (PVar x) = "(PVar " ++ show x ++ ")"
    show (PLit x) = "(PLit " ++ show x ++ ")"
    show (PStmts x y) = "(PStmts " ++ show x ++ " " ++ show y ++ ")"
    show PNil = "PNil"
    show PNoop = "PNoop"
    show (PPos x y z) = "(PPos " ++ show x ++ " " ++ show y ++ " " ++ show z ++ ")"
    show (PApp x y z) = "(PApp " ++ show x ++ " " ++ show y ++ " " ++ show z ++ ")"
    show (PExp x) = "(PExp " ++ show x ++ ")"
    show (PStmt x) = "(PStmt " ++ show x ++ ")"
    show (PAssign x y) = "(PAssign " ++ show x ++ " " ++ show y ++ ")"
    show (PBind x y) = "(PBind " ++ show x ++ " " ++ show y ++ ")"
    show (PPad x y) = "(PPad " ++ show x ++ " " ++ show y ++ ")"
    show (PThunk x) = "(PThunk " ++ show x ++ ")"
    show (PBlock x) = "(PBlock " ++ show x ++ ")"
    show (PRaw x) = "(PRaw " ++ show x ++ ")"
    show (PRawName x) = "(PRawName " ++ show x ++ ")"
    show (PSub x y) = "(PSub " ++ show x ++ " " ++ show y ++ ")"

data TEnv = MkTEnv
    { tLexDepth :: !Int
    , tTokDepth :: !Int
    , tEnv      :: !Env
    , tReg      :: !(TVar (Int, String))
    , tLabel    :: !(TVar Int)
    }
    deriving (Show, Eq)

type Comp a = Eval a
type CompMonad = EvalT (ContT Val (ReaderT Env SIO))
type Trans a = WriterT [Stmt] (ReaderT TEnv IO) a

class (Show a, Typeable b) => Compile a b where
    compile :: a -> Comp b
    compile x = fail ("Unrecognized construct: " ++ show x)

class (Show a, Typeable b) => Translate a b | a -> b where
    trans :: a -> Trans b
    trans _ = fail "Untranslatable construct!"

instance Compile (Var, [(TVar Bool, TVar VRef)]) (PAST Decl) where
    compile = compError

instance Compile Pad (PAST PIR) where
    compile = compError

instance Compile Pad [PAST Decl] where
    compile pad = do
        entries' <- mapM canCompile entries
        mapM compile $ catMaybes entries'
        where
        entries = sortBy padSort $ padToList pad
        canCompile (name@('&':_), [(_, sym)]) = do
            ref <- liftSTM $ readTVar sym
            case ref of
                MkRef (ICode cv)
                    -> doCode name =<< code_fetch cv
                MkRef (IScalar sv) | scalar_iType sv == mkType "Scalar::Const"
                    -> doCode name =<< fromVal =<< scalar_fetch sv
                _ -> return Nothing
        canCompile _ = return Nothing
        doCode name vsub = return $ if subType vsub == SubPrim
            then Nothing
            else Just (name, vsub)


instance Compile ([Char], VCode) (PAST Decl) where
    compile (name, MkCode{ subBody = Syn "block" [body] }) = do
        bodyC <- enter cxtItemAny $ compile body
        return $ PSub name bodyC
    compile x = compError x

{-
instance Compile [(TVar Bool, TVar VRef)] (PAST Expression) where
    compile _ = return (PLit $ PVal undef)
-}

instance Compile (String, [(TVar Bool, TVar VRef)]) (PAST Expression) where
    compile (name, _) = return $ PRawName name

instance Compile Exp (PAST [Stmt]) where
    compile (Pos pos rest) = fmap (PPos pos rest) $ compile rest
    compile (Cxt cxt rest) = enter cxt $ compile rest
    compile (Stmts (Pad SMy pad exp) rest) = do
        expC    <- compile $ mergeStmts exp rest
        padC    <- mapM compile (padToList pad)
        return $ PPad ((map fst (padToList pad)) `zip` padC) expC
    compile exp = compileStmts exp

class EnterClass m a | m -> a where
    enter :: a -> m b -> m b

instance EnterClass CompMonad Cxt where
    enter cxt = local (\e -> e{ envContext = cxt })

compileStmts :: Exp -> Comp (PAST [Stmt])
compileStmts exp = case exp of
    Stmts this rest -> do
        let ent | rest == Noop  = id
                | otherwise     = enter cxtVoid
        thisC   <- ent $ compile this
        restC   <- compileStmts rest
        return $ PStmts thisC restC
    Noop        -> return PNil
    _           -> compile (Stmts exp Noop)

instance Compile Val (PAST Stmt) where
    compile = fmap PStmt . compile . Val

instance Compile Exp (PAST Stmt) where
    compile (Pos pos rest) = fmap (PPos pos rest) $ compile rest
    compile (Cxt cxt rest) = enter cxt $ compile rest
    compile Noop = return PNoop
    compile (Val val) = do
        cxt     <- asks envContext
        if isVoidCxt cxt
            then do
                unless (val == VBool True) $
                    warn "Useless use of a constant in void context" val
                compile Noop
            else compile val
    compile (Syn "loop" [exp]) =
        compile (Syn "loop" $ [emptyExp, Val (VBool True), emptyExp, exp])
    compile (Syn "loop" [pre, cond, post, (Syn "block" [body])]) = do
        preC    <- compile pre
        condC   <- compile cond
        bodyC   <- compile body
        postC   <- compile post
        funC    <- compile (Var "&statement_control:loop")
        return $ PStmt $ PExp $ PApp PCxtVoid funC [preC, PBlock condC, PBlock bodyC, PBlock postC]
    compile exp = fmap PStmt $ compile exp

askPCxt :: Eval PCxt
askPCxt = do
    env <- ask
    return $ if envLValue env
        then PCxtLValue (typeOfCxt $ envContext env)
        else case envContext env of
            CxtVoid         -> PCxtVoid
            CxtItem typ     -> PCxtItem typ
            CxtSlurpy typ   -> PCxtSlurpy typ

instance Compile Exp (PAST LValue) where
    compile (Pos pos rest) = fmap (PPos pos rest) $ compile rest
    compile (Cxt cxt rest) = enter cxt $ compile rest
    compile (Var name) = return $ PVar name
    compile (App fun Nothing args) = do
        cxt     <- askPCxt
        funC    <- compile fun
        argsC   <- mapM (enter cxtItemAny . compile) args
        return $ PApp cxt funC argsC
    compile exp@(Syn "if" _) = compConditional exp
    compile exp@(Syn "unless" _) = compConditional exp
    compile exp = error ("Invalid LValue: " ++ show exp)

compConditional :: Exp -> Comp (PAST LValue)
compConditional (Syn name [cond, true, false]) = do
    cxt     <- askPCxt
    condC   <- compile cond
    trueC   <- compile true
    falseC  <- compile false
    funC    <- compile (Var $ "&statement_control:" ++ name)
    return $ PApp cxt funC [condC, PThunk trueC, PThunk falseC]
compConditional exp = compError exp

instance Compile Exp (PAST Expression) where
    compile (Pos pos rest) = fmap (PPos pos rest) $ compile rest
    compile (Cxt cxt rest) = enter cxt $ compile rest
    compile (Var name) = return . PExp $ PVar name
    compile (Val val) = fmap PLit (compile val)
    compile exp@(App _ _ _) = fmap PExp $ compile exp
    compile exp@(Syn "if" _) = fmap PExp $ compile exp
    compile Noop = compile (Val undef)
    compile (Syn "=" [lhs, rhs]) = do
        lhsC    <- compile lhs
        rhsC    <- compile rhs
        return $ PAssign [lhsC] rhsC
    compile (Syn "block" [body]) = do
        cxt     <- askPCxt
        bodyC   <- compile body
        return $ PExp $ PApp cxt (PBlock bodyC) []
    compile (Syn "sub" [Val (VCode sub)]) = do
        -- XXX I'd like to lambda lift... :-/
        cxt     <- askPCxt
        bodyC   <- compile (subBody sub)
        return $ PExp $ PApp cxt (PBlock bodyC) []
    compile exp = compError exp

compError :: forall a b. Compile a b => a -> Comp b
compError = die $ "Compile error -- invalid"
    ++ (dropWhile (not . isSpace) . show $ typeOf (undefined :: b))

transError :: forall a b. Translate a b => a -> Trans b
transError = die $ "Translate error -- invalid "
    ++ (show $ typeOf (undefined :: b))

instance Compile Val (PAST Literal) where
    compile val = return $ PVal val

die :: (MonadIO m, Show a) => String -> a -> m b
die x y = do
    warn x y
    liftIO $ exitFailure

warn :: (MonadIO m, Show a) => String -> a -> m ()
warn str val = liftIO $ do
    hPutStrLn stderr $ "*** " ++ str ++ ":\n    " ++ show val

instance Typeable1 PAST where
    typeOf1 _ = typeOf ()

instance (Typeable a) => Translate (PAST a) a where
    trans PNil = return []
    trans PNoop = return (StmtComment "")
    trans (PPos pos exp rest) = do
        -- tell [StmtLine (posName pos) (posBeginLine pos)]
        dep <- asks tTokDepth
        tell [StmtComment $ (replicate dep ' ') ++ "{{{ " ++ pretty exp]
        x   <- local (\e -> e{ tTokDepth = dep + 1 }) $ trans rest
        tell [StmtComment $ (replicate dep ' ') ++ "}}} " ++ pretty pos]
        return x
    trans (PLit (PVal VUndef)) = do
        pmc     <- genLV "undef"
        return (ExpLV pmc)
    trans (PLit lit) = do
        -- generate fresh supply and things...
        litC    <- trans lit
        pmc     <- genLV "lit"
        tellIns $ pmc <== ExpLit litC
        return (ExpLV pmc)
    trans (PVal (VStr str)) = return $ LitStr str
    trans (PVal (VInt int)) = return $ LitInt int
    trans (PVal (VNum num)) = return $ LitNum num
    trans (PVal (VRat rat)) = return $ LitNum (ratToNum rat)
    trans val@(PVal _) = transError val
    trans (PVar name) = do
        pmc     <- genLV "var"
        tellIns $ pmc <-- "find_name" $ [lit name]
        return pmc
{- XXX - this interferes with the prototype checking :-(
    trans (PStmt (PExp (PApp PCxtVoid (PExp (PVar name)) args))) = do
        argsC   <- mapM trans args
        return $ StmtIns $ InsFun [] (lit name) argsC
-}
    trans (PStmt (PLit (PVal VUndef))) = return $ StmtComment ""
    trans (PStmt exp) = do
        expC    <- trans exp
        return $ StmtIns $ InsExp expC
    trans (PAssign [lhs] rhs) = do
        lhsC    <- trans lhs
        rhsC    <- trans rhs
        tellIns $ lhsC <== rhsC
        return (ExpLV lhsC)
    trans (PStmts this rest) = do
        thisC   <- trans this
        tell [thisC]
        trans rest
    trans (PApp _ exp@(PBlock _) []) = do
        blockC  <- trans exp
        [appC] <- genLabel ["invokeBlock"]
        tell $ map StmtIns $ callBlock appC blockC 
        return tempPMC
    trans (PApp _ fun args) = do
        funC    <- case fun of
            PExp (PVar name) -> return $ lit name
            _           -> trans fun
        argsC   <- if isLogicalLazy fun
            then mapM trans (head args : map PThunk (tail args))
            else mapM trans args
        pmc     <- genLV "app"
        -- XXX - probe if funC is slurpy, then modify ExpLV pmc accordingly
        tellIns $ [reg pmc] <-& funC $ argsC
        return pmc
        where
        -- XXX HACK
        isLogicalLazy (PExp (PVar "&infix:or"))     = True
        isLogicalLazy (PExp (PVar "&infix:and"))    = True
        isLogicalLazy (PExp (PVar "&infix:||"))     = True
        isLogicalLazy (PExp (PVar "&infix:&&"))     = True
        isLogicalLazy _ = False
    trans (PPad pad exps) = do
        valsC   <- mapM trans (map snd pad)
        pass $ do
            expsC   <- trans exps
            return ([], (StmtPad (map fst pad `zip` valsC) expsC:))
    trans (PExp exp) = fmap ExpLV $ trans exp
    trans (PBlock body) = do
        [begC, endC] <- genLabel ["blockBegin", "blockEnd"]
        this    <- genPMC "block"
        tellIns $ "newsub" .- [reg this, bare ".Continuation", bare begC]
        tellIns $ "goto" .- [bare endC]
        tellLabel begC
        cc      <- genPMC "cc"
        fetchCC cc (reg this)
        trans body  -- XXX - consistency check
        bodyC   <- lastPMC
        tellIns $ if parrotBrokenXXX
            then "store_global" .- [tempSTR, bodyC] -- XXX HACK
            else "set_args" .- [lit "(0b10)", bodyC]
        tellIns $ "invoke" .- [reg cc]
        tellLabel endC
        return (ExpLV this)
    trans (PThunk exp) = do
        [begC, sndC, retC, endC] <- genLabel ["thunkBegin", "thunkAgain", "thunkReturn", "thunkEnd"]
        this    <- genPMC "block"
        tellIns $ "newsub" .- [reg this, bare ".Continuation", bare begC]
        tellIns $ "goto" .- [bare endC]
        tellLabel begC
        cc      <- genPMC "cc"
        fetchCC cc (reg this)
        expC    <- trans exp
        tellIns $ "set_addr" .- [reg this, bare sndC]
        tellIns $ "goto" .- [bare retC]
        tellLabel sndC
        fetchCC cc (reg this)
        tellLabel retC
        tellIns $ if parrotBrokenXXX
            then "store_global" .- [tempSTR, expC] -- XXX HACK
            else "set_args" .- [lit "(0b10)", expC]
        tellIns $ "invoke" .- [reg cc]
        tellLabel endC
        return (ExpLV this)
    trans (PRaw exp) = do
        env <- asks tEnv
        raw <- liftIO $ runEvalIO env{ envStash = "$P0" } $ do
            doc <- compile' exp
            return $ VStr (render doc)
        return $ StmtRaw (text $ vCast raw)
    trans (PRawName name) = do
        -- generate fresh supply and things...
        pmc     <- genName name
        return (ExpLV pmc)
    trans (PSub name body) = do
        (_, stmts)  <- listen $ do
            trans body
            bodyC <- lastPMC
            tellIns $ "set_returns" .- retSigList [bodyC]
            tellIns $ "returncc" .- []
        return (DeclSub name [] stmts)
    trans x = transError x

fetchCC :: LValue -> Expression -> Trans ()
fetchCC cc begC | parrotBrokenXXX = do
    tellIns $ tempINT   <-- "get_addr" $ [begC]
    tellIns $ tempSTR   <:= tempINT
    tellIns $ "find_global" .- [reg cc, tempSTR]
fetchCC cc _ = do
    tellIns $ "get_params" .- sigList [reg cc]

tellIns :: Ins -> Trans ()
tellIns = tell . (:[]) . StmtIns

tellLabel :: String -> Trans ()
tellLabel name = tellIns $ InsLabel name

lastPMC :: (RegClass a) => Trans a
lastPMC = do
    tvar    <- asks tReg
    name'   <- liftIO $ liftSTM $ do
        (cur, name) <- readTVar tvar
        return $ "P" ++ show cur ++ "_" ++ name
    return $ reg (VAR name')

genPMC :: (RegClass a) => String -> Trans a
genPMC name = do
    tvar    <- asks tReg
    name'   <- liftIO $ liftSTM $ do
        (cur, _) <- readTVar tvar
        writeTVar tvar (cur + 1, name)
        return $ "P" ++ show (cur + 1) ++ "_" ++ name
    tellIns $ InsLocal RegPMC name'
    return $ reg (VAR name')

genLV :: (RegClass a) => String -> Trans a
genLV name = do
    pmc <- genPMC name
    tellIns $ InsNew pmc PerlUndef
    return $ reg pmc

genLabel :: [String] -> Trans [LabelName]
genLabel names = do
    tvar    <- asks tLabel
    cnt     <- liftIO $ liftSTM $ do
        cur <- readTVar tvar
        writeTVar tvar (cur + 1)
        return cur
    return $ map (\name -> "LABEL_" ++ show cnt ++ "_" ++ name) names

genName :: (RegClass a) => String -> Trans a
genName name = do
    let var = render $ varText name
    tellIns $ InsLocal RegPMC var
    tellIns $ InsNew (VAR var) (read $ render $ varInit name)
    return $ reg (VAR var)

genPIR' :: Eval Val
genPIR' = do
    tenv        <- initTEnv
    glob        <- askGlobal
    main        <- asks envBody
    globPAST    <- compile glob
    mainPAST    <- compile main
    globPIR     <- runTransGlob tenv globPAST :: Eval [Decl]
    mainPIR     <- runTransMain tenv mainPAST :: Eval [Stmt]
    return . VStr . unlines $
        [ "#!/usr/bin/env parrot"
        -- , renderStyle (Style PageMode 0 0) init
        , renderStyle (Style PageMode 0 0) $ preludePIR $+$ vcat
            [ emit $ namespace "main"
            , emit globPIR
            , text ".sub init @MAIN, @ANON"
            , text "    new_pad 0"
            , text "    main()"
            , text ".end"
            , text ".sub main @ANON"
            , nest 4 (emit mainPIR)
            , text ".end"
            ]
        ]

runTransGlob :: TEnv -> [PAST Decl] -> Eval [Decl]
runTransGlob tenv = mapM $ fmap fst . liftIO . (`runReaderT` tenv) . runWriterT . trans

runTransMain :: TEnv -> PAST [Stmt] -> Eval [Stmt]
runTransMain tenv = fmap snd . liftIO . (`runReaderT` tenv) . runWriterT . trans

initTEnv :: Eval TEnv
initTEnv = do
    env         <- ask
    zero        <- liftSTM $ newTVar (0, "")
    none        <- liftSTM $ newTVar 0
    return $ MkTEnv
        { tLexDepth = 0
        , tTokDepth = 0
        , tEnv      = env
        , tReg      = zero
        , tLabel    = none
        }
