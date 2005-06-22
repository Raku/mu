{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances -fno-warn-orphans -funbox-strict-fields -cpp #-}
{-# OPTIONS_GHC -#include "UnicodeC.h" #-}

{-|
    Compiler interface.

>   And words unheard were spoken then
>   Of folk and Men and Elven-kin,
>   Beyond the world were visions showed
>   Forbid to those that dwell therein...
-}

module Pugs.Compile where
import Pugs.AST
import Pugs.Internals
import Pugs.Types
import Pugs.Eval
import Emit.PIR
import Text.PrettyPrint

{-|
    The plan here is to first compile the environment (subroutines,
    statements, etc.) to an abstract syntax tree ('PIL' -- Pugs Intermediate
    Language) using the 'compile' function and 'Compile' class.
-}

#ifndef HADDOCK
-- Type-indexed with GADT; it is a bit too baroque -- refactor toward ANF?
data (Typeable a) => PIL a where
    PNil        :: PIL [a]
    PNoop       :: PIL Stmt

    PRawName    :: !VarName -> PIL Expression -- XXX HACK!

    PExp        :: !(PIL LValue) -> PIL Expression 
    PLit        :: !(PIL Literal) -> PIL Expression
    PPos        :: !Pos -> !Exp -> !(PIL a) -> PIL a
    PStmt       :: !(PIL Expression) -> PIL Stmt 
    PThunk      :: !(PIL Expression) -> PIL Expression 
    PCode       :: !SubType -> ![TParam] -> !(PIL [Stmt]) -> PIL Expression 

    PVal        :: !Val -> PIL Literal
    PVar        :: !VarName -> PIL LValue

    PStmts      :: !(PIL Stmt) -> !(PIL [Stmt]) -> PIL [Stmt]
    PApp        :: !TCxt -> !(PIL Expression) -> ![PIL Expression] -> PIL LValue
    PAssign     :: ![PIL LValue] -> !(PIL Expression) -> PIL LValue
    PBind       :: ![PIL LValue] -> !(PIL Expression) -> PIL LValue
    PPad        :: !Scope -> ![(VarName, PIL Expression)] -> !(PIL [Stmt]) -> PIL [Stmt]

    PSub        :: !SubName -> !SubType -> ![TParam] -> !(PIL [Stmt]) -> PIL Decl
#endif

instance Typeable1 PIL where
    typeOf1 _ = typeOf ()

data TParam = MkTParam
    { tpParam   :: !Param
    , tpDefault :: !(Maybe (PIL Expression))
    }
    deriving (Show, Typeable)

data TCxt
    = TCxtVoid | TCxtLValue !Type | TCxtItem !Type | TCxtSlurpy !Type
    | TTailCall !TCxt
    deriving (Show, Eq, Typeable)

tcVoid, tcLValue :: TCxt
tcVoid      = TCxtVoid
tcLValue    = TCxtLValue anyType

{-
tcItem, tcSlurpy :: TCxt
tcItem      = TCxtItem anyType
tcSlurpy    = TCxtSlurpy anyType
-}

instance Show (PIL a) where
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
    show (PThunk x) = "(PThunk " ++ show x ++ ")"
    show (PRawName x) = "(PRawName " ++ show x ++ ")"
    show (PPad x y z) = unwords ["(PPad", show x, show y, show z, ")"]
    show (PCode x y z) = unwords ["(PCode", show x, show y, show z, ")"]
    show (PSub x y z w) = unwords ["(PSub", show x, show y, show z, show w, ")"]

data TEnv = MkTEnv
    { tLexDepth :: !Int                 -- ^ Lexical scope depth
    , tTokDepth :: !Int                 -- ^ Exp nesting depth
    , tCxt      :: !TCxt                -- ^ Current context
    , tReg      :: !(TVar (Int, String))-- ^ Register name supply
    , tLabel    :: !(TVar Int)          -- ^ Label name supply
    }
    deriving (Show, Eq)

type Comp a = Eval a
type CompMonad = EvalT (ContT Val (ReaderT Env SIO))

{-| Currently only 'Exp' → 'PIL' -}
class (Show a, Typeable b) => Compile a b where
    compile :: a -> Comp b
    compile x = fail ("Unrecognized construct: " ++ show x)

-- Compile instances
instance Compile (Var, [(TVar Bool, TVar VRef)]) (PIL Decl) where
    compile = compError

instance Compile Param TParam where
    compile prm = do
        defC <- if isOptional prm
            then fmap Just $ compile (paramDefault prm)
            else return Nothing
        return $ MkTParam
            { tpParam = prm
            , tpDefault = defC
            }

{-| Compiles a 'Pad' to a list of 'PIL Decl's. Currently, only subroutines and
    @\@*END@ are compiled. -}
instance Compile Pad [PIL Decl] where
    compile pad = do
        entries' <- mapM canCompile entries
        return $ concat entries'
        where
        entries = sortBy padSort $ padToList pad
        canCompile (name@('&':_), [(_, sym)]) = do
            ref <- liftSTM $ readTVar sym
            case ref of
                MkRef (ICode cv)
                    -> doCode name =<< code_fetch cv
                MkRef (IScalar sv) | scalar_iType sv == mkType "Scalar::Const"
                    -> doCode name =<< fromVal =<< scalar_fetch sv
                _ -> return []
        canCompile ("@*END", [(_, sym)]) = do
            ref     <- liftSTM $ readTVar sym
            cvList  <- fromVals =<< readRef ref :: Comp [VCode]
            decls   <- eachM cvList $ \(i, cv) -> do
                compile (("&*END_" ++ show i), cv) :: Comp [PIL Decl]
            compile ("&*END", concat decls)
        canCompile ((_:twigil:_), _) | not (isAlphaNum twigil) = return []
        canCompile (name, [(_, sym)]) = do
            -- translate them into store_global calls?
            -- placing them each into one separate init function?
            val     <- readRef =<< liftSTM (readTVar sym)
            valC    <- compile val
            let assignC = PAssign [PVar name'] valC
                bodyC   = PStmts (PStmt . PExp $ assignC) PNil
                initL   = "__init_" ++ (render $ varText name)
                name' | ':' `elem` name = name
                      | otherwise = "main::" ++ name -- XXX wrong
            return [PSub initL SubPrim [] bodyC]
        canCompile _ = return []
        doCode name vsub = case subBody vsub of
            Prim _  -> return []
            _       -> compile (name, vsub)

eachM :: (Monad m) => [a] -> ((Int, a) -> m b) -> m [b]
eachM = forM . ([0..] `zip`)

instance Compile (SubName, [PIL Decl]) [PIL Decl] where
    compile (name, decls) = do
        let bodyC = [ PStmts . PStmt . PExp $ PApp tcVoid (PExp (PVar sub)) []
                    | PSub sub _ _ _ <- decls
                    ]
        return (PSub name SubPrim [] (combine bodyC PNil):decls)

instance Compile (SubName, VCode) [PIL Decl] where
    compile (name, vsub) | packageOf name /= packageOf (subName vsub) = do
        let storeC  = PBind [PVar $ qualify name] (PExp . PVar . qualify $ subName vsub)
            bodyC   = PStmts (PStmt . PExp $ storeC) PNil
            exportL = "__export_" ++ (render $ varText name)
        return [PSub exportL SubPrim [] bodyC]
    compile (name, vsub) = do
        bodyC   <- enter cxtItemAny . compile $ case subBody vsub of
            Syn "block" [body]  -> body
            body                -> body
        paramsC <- compile $ subParams vsub
        return [PSub name (subType vsub) paramsC bodyC]

instance Compile (String, [(TVar Bool, TVar VRef)]) (PIL Expression) where
    compile (name, _) = return $ PRawName name

instance Compile Exp (PIL [Stmt]) where
    compile (Pos pos rest) = fmap (PPos pos rest) $ compile rest
    compile (Cxt cxt rest) = enter cxt $ compile rest
    compile (Stmts (Pad SOur _ exp) rest) = do
        compile $ mergeStmts exp rest
    compile (Stmts (Pad _ pad exp) rest) = do
        expC    <- compile $ mergeStmts exp rest
        padC    <- compile $ padToList pad
        return $ PPad SMy ((map fst $ padToList pad) `zip` padC) expC
    compile exp = compileStmts exp

class EnterClass m a where
    enter :: a -> m b -> m b

instance EnterClass CompMonad VCode where
    enter sub = local (\e -> e{ envLValue = subLValue sub, envContext = CxtItem (subReturns sub) })

instance EnterClass CompMonad Cxt where
    enter cxt = local (\e -> e{ envContext = cxt })

compileStmts :: Exp -> Comp (PIL [Stmt])
compileStmts exp = case exp of
    Stmts this Noop -> do
        thisC   <- compile this
        return $ PStmts (tailCall thisC) PNil
        where
        tailCall (PStmt (PExp (PApp cxt fun args)))
            = PStmt $ PExp $ PApp (TTailCall cxt) fun args
        tailCall (PPos pos exp x) = PPos pos exp (tailCall x)
        tailCall x = x
    Stmts this rest -> do
        thisC   <- enter cxtVoid $ compile this
        restC   <- compileStmts rest
        return $ PStmts thisC restC
    Noop        -> return PNil
    _           -> compile (Stmts exp Noop)

instance Compile Val (PIL Stmt) where
    compile = fmap PStmt . compile . Val

instance Compile Val (PIL Expression) where
    compile = compile . Val

instance Compile Exp (PIL Stmt) where
    compile (Pos pos rest) = fmap (PPos pos rest) $ compile rest
    compile (Cxt cxt rest) = enter cxt $ compile rest
    compile Noop = return PNoop
    compile (Val val) = do
        cxt     <- asks envContext
        if isVoidCxt cxt
            then case val of
                VBool True      -> compile Noop
                _               -> do
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
        return . PStmt . PExp $ PApp TCxtVoid funC
            [preC, pBlock condC, pBlock bodyC, pBlock postC]
    compile exp@(Syn "unless" _) = fmap (PStmt . PExp) $ compConditional exp
    compile exp@(Syn "while" _) = compLoop exp
    compile exp@(Syn "until" _) = compLoop exp
    compile exp@(Syn "postwhile" _) = compLoop exp
    compile exp@(Syn "postuntil" _) = compLoop exp
    compile (Syn "for" [exp, body]) = do
        expC    <- compile exp
        bodyC   <- compile body
        funC    <- compile (Var "&statement_control:for")
        return . PStmt . PExp $ PApp TCxtVoid funC [expC, bodyC]
    compile (Syn "given" _) = compile (Var "$_") -- XXX
    compile (Syn "when" _) = compile (Var "$_") -- XXX
    compile exp = fmap PStmt $ compile exp

pBlock :: PIL [Stmt] -> PIL Expression
pBlock = PCode SubBlock []

{-
subTCxt :: VCode -> Eval TCxt
subTCxt sub = return $ if subLValue sub
    then TCxtLValue (subReturns sub)
    else TCxtItem (subReturns sub)
-}

askTCxt :: Eval TCxt
askTCxt = do
    env <- ask
    return $ if envLValue env
        then TCxtLValue (typeOfCxt $ envContext env)
        else case envContext env of
            CxtVoid         -> TCxtVoid
            CxtItem typ     -> TCxtItem typ
            CxtSlurpy typ   -> TCxtSlurpy typ

instance (Show (m a), FunctorM m, Typeable1 m, Compile a b) => Compile (m a) (m b) where
    compile = fmapM compile

instance (Compile a b, Compile a c) => Compile [a] (b, c) where
    compile [x, y] = do { x' <- compile x ; y' <- compile y; return (x', y') }
    compile x = compError x

instance (Compile a b, Compile a c, Compile a d) => Compile [a] (b, c, d) where
    compile [x, y, z] = do { x' <- compile x ; y' <- compile y; z' <- compile z; return (x', y', z') }
    compile x = compError x

instance Compile Exp (PIL LValue) where
    compile (Pos pos rest) = fmap (PPos pos rest) $ compile rest
    compile (Cxt cxt rest) = enter cxt $ compile rest
    compile (Var name) = return $ PVar name
    compile (Syn (sigil:"::()") exps) = do
        compile $ App (Var "&Pugs::Internals::symbolic_deref") Nothing $
            (Val . VStr $ sigil:""):exps
    compile (App (Var "&goto") (Just inv) args) = do
        cxt     <- askTCxt
        funC    <- compile inv
        argsC   <- enter cxtItemAny $ compile args
        return $ PApp (TTailCall cxt) funC argsC
    compile (App fun (Just inv) args) = do
        compile (App fun Nothing (inv:args)) -- XXX WRONG
    compile (App fun Nothing args) = do
        cxt     <- askTCxt
        funC    <- compile fun
        argsC   <- enter cxtItemAny $ compile args
        return $ PApp cxt funC argsC
    compile exp@(Syn "if" _) = compConditional exp
    compile (Syn "{}" (x:xs)) = compile $ App (Var "&postcircumfix:{}") (Just x) xs
    compile (Syn "[]" (x:xs)) = do
        compile (App (Var "&postcircumfix:[]") (Just x) xs)
    compile (Syn "," exps) = do
        compile (App (Var "&infix:,") Nothing exps)
    compile (Syn "\\[]" exps) = do
        compile (App (Var "&circumfix:[]") Nothing exps)
    compile (Syn "\\{}" exps) = do
        compile (App (Var "&circumfix:{}") Nothing exps)
    compile (Syn "=" [lhs, rhs]) = do
        lhsC <- enterLValue $ compile lhs
        rhsC <- enterRValue $ compile rhs
        return $ PAssign [lhsC] rhsC
    compile (Syn ":=" exps) = do
        (lhsC, rhsC) <- enterLValue $ compile exps
        return $ PBind [lhsC] rhsC
    compile (Syn syn [lhs, exp]) | last syn == '=' = do
        let op = "&infix:" ++ init syn
        compile $ Syn "=" [lhs, App (Var op) Nothing [lhs, exp]]
    compile exp = compError exp

compLoop :: Exp -> Comp (PIL Stmt)
compLoop (Syn name [cond, body]) = do
    cxt     <- askTCxt
    condC   <- enter (CxtItem $ mkType "Bool") $ compile cond
    bodyC   <- enter CxtVoid $ compile body
    funC    <- compile (Var $ "&statement_control:" ++ name)
    return . PStmt . PExp $ PApp cxt funC [pBlock condC, pBlock bodyC]
compLoop exp = compError exp

{-| Compiles a conditional 'Syn' (@if@ and @unless@) to a call to an
    appropriate function call (@&statement_control:if@ or
    @&statement_control:unless@). -}
compConditional :: Exp -> Comp (PIL LValue)
compConditional (Syn name exps) = do
    [condC, trueC, falseC] <- compile exps
    funC    <- compile $ Var ("&statement_control:" ++ name)
    cxt     <- askTCxt
    return $ PApp cxt funC [condC, PThunk trueC, PThunk falseC]
compConditional exp = compError exp

{-| Compiles various 'Exp's to 'PIL Expression's. -}
instance Compile Exp (PIL Expression) where
    compile (Pos pos rest) = fmap (PPos pos rest) $ compile rest
    compile (Cxt cxt rest) = enter cxt $ compile rest
    compile (Var name) = return . PExp $ PVar name
    compile exp@(Val (VCode _)) = compile $ Syn "sub" [exp]
    compile (Val val) = fmap PLit $ compile val
    compile Noop = compile (Val undef)
    compile (Syn "block" [body]) = do
        cxt     <- askTCxt
        bodyC   <- compile body
        return $ PExp $ PApp cxt (pBlock bodyC) []
    compile (Syn "sub" [Val (VCode sub)]) = do
        bodyC   <- enter sub $ compile $ case subBody sub of
            Syn "block" [exp]   -> exp
            exp                 -> exp
        paramsC <- compile $ subParams sub
        return $ PCode (subType sub) paramsC bodyC
    compile (Syn "module" _) = compile Noop
    compile (Syn "match" exp) = compile $ Syn "rx" exp -- wrong
    compile (Syn "//" exp) = compile $ Syn "rx" exp
    compile (Syn "rx" [exp, _]) = compile exp -- XXX WRONG - use PCRE
    compile (Syn "subst" [exp, _, _]) = compile exp -- XXX WRONG - use PCRE
    compile exp@(App _ _ _) = fmap PExp $ compile exp
    compile exp@(Syn _ _) = fmap PExp $ compile exp
    compile exp = compError exp

compError :: forall a b. Compile a b => a -> Comp b
compError = die $ "Compile error -- invalid "
    ++ (show $ typeOf (undefined :: b))

{-| Compiles a 'Val' to a 'PIL Literal'. -}
instance Compile Val (PIL Literal) where
    compile val = return $ PVal val

die :: (MonadIO m, Show a) => String -> a -> m b
die x y = do
    warn x y
    liftIO $ exitFailure

warn :: (MonadIO m, Show a) => String -> a -> m ()
warn str val = liftIO $ do
    hPutStrLn stderr $ "*** " ++ str ++ ":\n    " ++ show val

-- utility functions
padSort :: (Var, [(TVar Bool, TVar VRef)]) -> (String, [(a, b)]) -> Ordering
padSort (a, [(_, _)]) (b, [(_, _)])
    | (head a == ':' && head b == '&') = LT
    | (head b == ':' && head a == '&') = GT
    | otherwise = GT
padSort _ _ = EQ

varText :: String -> Doc
varText ('$':name)  = text $ "s__" ++ escaped name
varText ('@':name)  = text $ "a__" ++ escaped name
varText ('%':name)  = text $ "h__" ++ escaped name
varText ('&':name)  = text $ "c__" ++ escaped name
varText x           = error $ "invalid name: " ++ x

packageOf :: String -> String
packageOf name = case isQualified name of
    Just (pkg, _)   -> pkg
    _               -> "main"

qualify :: String -> String
qualify name = case isQualified name of
    Just _  -> name
    _       -> let (sigil, name') = span (not . isAlphaNum) name
        in sigil ++ "main::" ++ name'

isQualified :: String -> Maybe (String, String)
isQualified name | Just (post, pre) <- breakOnGlue "::" (reverse name) =
    let (sigil, pkg) = span (not . isAlphaNum) preName
        name'       = possiblyFixOperatorName (sigil ++ postName)
        preName     = reverse pre
        postName    = reverse post
    in Just (pkg, name')
isQualified _ = Nothing
