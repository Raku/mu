{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances -fno-warn-orphans -funbox-strict-fields -cpp #-}
{-# OPTIONS_GHC -#include "UnicodeC.h" #-}

{-|
    Compiler interface.

>   And words unheard were spoken then
>   Of folk and Men and Elven-kin,
>   Beyond the world were visions showed
>   Forbid to those that dwell therein...
-}

module Pugs.Compile (
    PIL_Stmts(..), PIL_Stmt(..), PIL_Expr(..), PIL_Decl(..), PIL_Literal(..), PIL_LValue(..),
    Compile(..),
    TEnv(..), initTEnv,
    TCxt(..), tcVoid, tcLValue,
    TParam(..),
    EnterClass(..),
    die, varText,
) where
import Pugs.AST
import Pugs.Internals
import Pugs.Types
import Pugs.Eval
import Pugs.Eval.Var
import Pugs.Monads
import Pugs.PIL1
import Emit.PIR
import Text.PrettyPrint

tcVoid, tcLValue :: TCxt
tcVoid      = TCxtVoid
tcLValue    = TCxtLValue anyType

{-
tcItem, tcSlurpy :: TCxt
tcItem      = TCxtItem anyType
tcSlurpy    = TCxtSlurpy anyType
-}

type Comp a = Eval a
type CompMonad = EvalT (ContT Val (ReaderT Env SIO))

{-| Currently only 'Exp' → 'PIL' -}
class (Show a, Typeable b) => Compile a b where
    compile :: a -> Comp b
    compile x = fail ("Unrecognized construct: " ++ show x)

-- Compile instances
instance Compile (Var, [(TVar Bool, TVar VRef)]) (PIL_Decl) where
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

{-| Compiles a 'Pad' to a list of 'PIL_Decl's. Currently, only subroutines and
    @\@*END@ are compiled. -}
instance Compile Pad [PIL_Decl] where
    compile pad = do
        entries' <- mapM canCompile entries
        return $ concat entries'
        where
        entries = sortBy padSort $ padToList pad
        canCompile (name@('&':_), xs) | length xs > 1 = do
            liftM concat $ mapM (\x -> canCompile (name, [x])) xs
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
                compile (("&*END_" ++ show i), cv) :: Comp [PIL_Decl]
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

instance Compile (SubName, [PIL_Decl]) [PIL_Decl] where
    compile (name, decls) = do
        let bodyC = [ PStmts . PStmt . PExp $ PApp tcVoid (PExp (PVar sub)) Nothing []
                    | PSub sub _ _ _ <- decls
                    ]
        return (PSub name SubPrim [] (combine bodyC PNil):decls)

instance Compile (SubName, VCode) [PIL_Decl] where
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

instance Compile (String, [(TVar Bool, TVar VRef)]) (PIL_Expr) where
    compile (name, _) = return $ PRawName name

instance Compile Exp (PIL_Stmts) where
    compile (Pos _ rest) = compile rest -- fmap (PPos pos rest) $ compile rest
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

compileStmts :: Exp -> Comp (PIL_Stmts)
compileStmts exp = case exp of
    Stmts this Noop -> do
        thisC   <- compile this
        return $ PStmts (tailCall thisC) PNil
        where
        tailCall (PStmt (PExp (PApp cxt fun inv args)))
            = PStmt $ PExp $ PApp (TTailCall cxt) fun inv args
        tailCall (PPos pos exp x) = PPos pos exp (tailCall x)
        tailCall x = x
    Stmts this (Syn "namespace" [Val (VStr pkg), rest]) -> do
        thisC   <- enter cxtVoid $ compile this
        restC   <- enterPackage pkg $ compileStmts rest
        return $ PStmts thisC restC
    Stmts this rest -> do
        thisC   <- enter cxtVoid $ compile this
        restC   <- compileStmts rest
        return $ PStmts thisC restC
    Noop        -> return PNil
    _           -> compile (Stmts exp Noop)

instance Compile Val (PIL_Stmt) where
    compile = fmap PStmt . compile . Val

instance Compile Val (PIL_Expr) where
    compile = compile . Val

instance Compile Exp (PIL_Stmt) where
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
        -- loop ...; ; ... {...} ->
        -- loop ...; bool::true; ... {...}
        let cond' | unwrap cond == Noop
                  = return $ PStmts (PStmt . PLit . PVal $ VBool True) PNil
                  | otherwise
                  = compile cond
        condC   <- cond'
        bodyC   <- compile body
        postC   <- compile post
        funC    <- compile (Var "&statement_control:loop")
        return . PStmt . PExp $ PApp TCxtVoid funC Nothing
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
        return . PStmt . PExp $ PApp TCxtVoid funC Nothing [expC, bodyC]
    compile (Syn "given" _) = compile (Var "$_") -- XXX
    compile (Syn "when" _) = compile (Var "$_") -- XXX
    compile exp = fmap PStmt $ compile exp

pBlock :: PIL_Stmts -> PIL_Expr
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

instance Compile Exp (PIL_LValue) where
    compile (Pos _ rest) = compile rest -- fmap (PPos pos rest) $ compile rest
    compile (Cxt cxt rest) = enter cxt $ compile rest
    compile (Var name) = return $ PVar name
    compile (Syn (sigil:"::()") exps) = do
        compile $ App (Var "&Pugs::Internals::symbolic_deref") Nothing $
            (Val . VStr $ sigil:""):exps
    compile (App (Var "&goto") (Just inv) args) = do
        cxt     <- askTCxt
        funC    <- compile inv
        argsC   <- enter cxtItemAny $ compile args
        return $ PApp (TTailCall cxt) funC Nothing argsC
    compile (App fun inv args) = do
        cxt     <- askTCxt
        funC    <- compile fun
        invC    <- maybeM (return inv) compile
        argsC   <- enter cxtItemAny $ compile args
        if isLogicalLazy funC
            then return $ PApp cxt funC invC (head argsC:map PThunk (tail argsC))
            else return $ PApp cxt funC invC argsC
        where
        -- XXX HACK
        isLogicalLazy (PExp (PVar "&infix:or"))  = True
        isLogicalLazy (PExp (PVar "&infix:and")) = True
        isLogicalLazy (PExp (PVar "&infix:||"))  = True
        isLogicalLazy (PExp (PVar "&infix:&&"))  = True
        isLogicalLazy (PExp (PVar "&infix://"))  = True
        isLogicalLazy (PExp (PVar "&infix:err"))  = True
        isLogicalLazy _ = False
    compile exp@(Syn "if" _) = compConditional exp
    compile (Syn "{}" (x:xs)) = compile $ App (Var "&postcircumfix:{}") (Just x) xs
    compile (Syn "[]" (x:xs)) = do
        compile (App (Var "&postcircumfix:[]") (Just x) xs)
    compile (Syn "," exps) = do
        compile (App (Var "&infix:,") Nothing exps)
    -- Minor hack, my $a = [] is parsed as my $a = [Noop], resulting in my $a =
    -- [undef], which is wrong.
    compile (Syn "\\[]" [Noop]) = do
        compile (App (Var "&circumfix:[]") Nothing [])
    compile (Syn "\\[]" exps) = do
        compile (App (Var "&circumfix:[]") Nothing exps)
    compile (Syn name@(sigil:"{}") exps) | (sigil ==) `any` "$@%&" = do
        compile (App (Var $ "&circumfix:" ++ name) Nothing exps)
    compile (Syn "\\{}" exps) = do
        compile (App (Var "&circumfix:{}") Nothing exps)
    compile (Syn "*" exps) = do
        compile (App (Var "&prefix:*") Nothing exps)
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
    compile (Syn "but" [obj, block]) =
        compile $ App (Var "&Pugs::Internals::but_block") Nothing [obj, block]
    compile exp = compError exp

compLoop :: Exp -> Comp (PIL_Stmt)
compLoop (Syn name [cond, body]) = do
    cxt     <- askTCxt
    condC   <- enter (CxtItem $ mkType "Bool") $ compile cond
    bodyC   <- enter CxtVoid $ compile body
    funC    <- compile (Var $ "&statement_control:" ++ name)
    return . PStmt . PExp $ PApp cxt funC Nothing [pBlock condC, pBlock bodyC]
compLoop exp = compError exp

{-| Compiles a conditional 'Syn' (@if@ and @unless@) to a call to an
    appropriate function call (@&statement_control:if@ or
    @&statement_control:unless@). -}
compConditional :: Exp -> Comp (PIL_LValue)
compConditional (Syn name exps) = do
    [condC, trueC, falseC] <- compile exps
    funC    <- compile $ Var ("&statement_control:" ++ name)
    cxt     <- askTCxt
    return $ PApp cxt funC Nothing [condC, PThunk trueC, PThunk falseC]
compConditional exp = compError exp

{-| Compiles various 'Exp's to 'PIL_Expr's. -}
instance Compile Exp (PIL_Expr) where
    compile (Pos _ rest) = compile rest -- fmap (PPos pos rest) $ compile rest
    compile (Cxt cxt rest) = enter cxt $ compile rest
    compile (Var name) = return . PExp $ PVar name
    compile exp@(Val (VCode _)) = compile $ Syn "sub" [exp]
    compile (Val val) = fmap PLit $ compile val
    compile Noop = compile (Val undef)
    compile (Syn "block" [body]) = do
        cxt     <- askTCxt
        bodyC   <- compile body
        return $ PExp $ PApp cxt (pBlock bodyC) Nothing []
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

{-| Compiles a 'Val' to a 'PIL_Literal'. -}
instance Compile Val (PIL_Literal) where
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

initTEnv :: Eval TEnv
initTEnv = do
    initReg <- liftSTM $ newTVar (0, "")
    initLbl <- liftSTM $ newTVar 0
    return $ MkTEnv
        { tLexDepth = 0
        , tTokDepth = 0
        , tCxt      = tcVoid
        , tReg      = initReg
        , tLabel    = initLbl
        }

