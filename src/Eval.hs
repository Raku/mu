{-# OPTIONS -fglasgow-exts #-}

{-
    Evaluation and reduction engine.

    Home is behind, the world ahead,
    And there are many paths to tread
    Through shadows to the edge of night,
    Until the stars are all alight.
    Then world behind and home ahead,
    We'll wander back to home and bed...
-}

module Eval where
import Internals
import Prelude hiding ( exp )

import AST
import Junc
import Bind
import Prim
import Context
import Monads
import Pretty

emptyEnv :: (MonadIO m) => m Env
emptyEnv = do
    ref  <- liftIO $ newIORef emptyFM
    uniq <- liftIO $ newUnique
    return $ Env
        { envContext = "Void"
        , envLexical = []
        , envGlobal  = initSyms
        , envClasses = initTree
        , envEval    = evaluate
        , envCC      = return
        , envCaller  = Nothing
        , envDepth   = 0
        , envID      = uniq
        , envBody    = Val VUndef
        , envDebug   = Just ref -- Set to "Nothing" to disable debugging
        }

-- Evaluation ---------------------------------------------------------------

-- debug :: (Pretty a) => String -> String -> a -> Eval ()
debug key fun str a = do
    rv <- asks envDebug
    case rv of
        Nothing -> return ()
        Just ref -> liftIO $ do
            fm <- readIORef ref
            let val = fun $ lookupWithDefaultFM fm "" key
            writeIORef ref (addToFM fm key val)
            putStrLn ("***" ++ val ++ str ++ ": " ++ pretty a)

evaluate :: Exp -> Eval Val
evaluate (Val (VSub sub)) = do
    cxt <- asks envContext
    if cxt == "Void" && subType sub == SubBlock
        then do
            exp <- apply sub [] []
            evalExp exp
        else do
            pad <- asks envLexical
            return $ VSub sub{ subPad = pad } -- closure!
evaluate (Val val) = return val
evaluate exp = do
    debug "indent" (' ':) "Evl" exp
    exp' <- local (\e -> e{ envBody = exp }) reduce
    debug "indent" (tail) " Ret" exp'
    return $ case exp' of
        Val v       -> v
        otherwise   -> VError "Invalid expression" exp'

evalExp :: Exp -> Eval Val
evalExp exp = do
    evl <- asks envEval
    evl exp

evalSym :: Symbol -> Eval (String, Val)
evalSym (Symbol _ name vexp) = do
    val <- evalExp vexp
    return (name, val)

enterEvalContext cxt = enterContext cxt . evalExp

-- Reduction ---------------------------------------------------------------

reduce :: Eval Exp
reduce = do
    env@Env{ envBody = body } <- ask
    doReduce env body

reduceExp :: Exp -> Eval Exp
reduceExp exp = do
    env <- ask
    doReduce env exp

retVal :: Val -> Eval Exp
retVal val = return $ Val val

reduceStatements :: [Exp] -> Eval Exp
reduceStatements [] = retVal VUndef
reduceStatements [exp] = do
    val <- evalExp exp
    retVal val
reduceStatements (exp:rest)
    | Syn "sym" [Sym sym@(Symbol SGlobal _ _)] <- exp = do
        local (\e -> e{ envGlobal = (sym:envGlobal e) }) $ do
            reduceStatements rest
    | Syn "sym" [Sym sym@(Symbol SMy _ _)] <- exp = do
        enterLex [sym] $ do
            reduceStatements rest
    | Syn syn [Var name, exp'] <- exp
    , (syn == ":=" || syn == "::=") = do
        lex <- asks envLexical
        case findSym name lex of
            Just _  -> do
                let sym = (Symbol SMy name exp')
                enterLex [sym] $ do
                    reduceStatements rest
            Nothing -> do
                let sym = (Symbol SGlobal name exp')
                local (\e -> e{ envGlobal = (sym:envGlobal e) }) $ do
                    reduceStatements rest
    | otherwise = do
        val <- enterContext "Void" $ evalExp exp
        processVal val $ do
            reduceStatements rest
    where
    processVal val action = case val of
        VError _ _  -> retVal val
        _           -> action

doReduce :: Env -> Exp -> Eval Exp

-- Reduction for variables
doReduce Env{ envLexical = lex, envGlobal = glob } exp@(Var var)
    | Just vexp <- findSym var lex
    = reduceExp vexp
    | Just vexp <- findSym var glob
    = reduceExp vexp
    | Just vexp <- findSym (toGlobal var) glob
    = reduceExp vexp
    | otherwise
    = retVal $ VError ("Undefined variable " ++ var) exp

-- Reduction for syntactic constructs
doReduce env@Env{ envContext = cxt } exp@(Syn name exps) = case name of
    ";" -> do
        let (global, local) = partition isGlobalExp exps
        reduceStatements (global ++ local)
    "sym" -> do
        let [Sym (Symbol _ _ exp)] = exps
        val     <- evalExp exp
        retVal VUndef
    ":=" -> do
        let [Var var, exp] = exps
        val     <- enterEvalContext (cxtOfSigil $ head var) exp
        retVal val
    "::=" -> do -- XXX wrong
        let [Var var, exp] = exps
        val     <- evalExp exp
        retVal VUndef -- XXX wrong
    "=>" -> do
        let [keyExp, valExp] = exps
        key     <- enterEvalContext "Scalar" keyExp
        val     <- evalExp valExp
        retVal $ VPair key val
    "," -> do
        vals    <- mapM (enterEvalContext "List") exps
        retVal $ VList vals
    "[]" -> do
        let (listExp:rangeExp:errs) = exps
        list    <- enterEvalContext "List" listExp
        range   <- enterEvalContext "List" rangeExp
        let slice = unfoldr (doSlice errs $ vCast list) (map vCast $ vCast range)
        retVal $ VList slice
    "gather" -> do
        val     <- enterEvalContext "List" exp
        -- ignore val
        retVal val
    _ -> do
        retVal $ VError "Unknown syntactic construct" exp
    where
    doSlice :: [Exp] -> [Val] -> [VInt] -> Maybe (Val, [VInt])
    doSlice errs vs (n:ns)
        | (v:_)         <- n `genericDrop` vs
        = Just (v, ns)
        | ((Val err):_) <- errs
        = Just (err, ns)
        | otherwise
        = Nothing
    doSlice _ _ _ = Nothing

doReduce env@Env{ envClasses = cls, envContext = cxt, envLexical = lex, envGlobal = glob } exp@(App name invs args) = do
    subSyms <- mapM evalSym [ sym | sym <- lex ++ glob, head (symName sym) == '&' ]
    case findSub subSyms name of
        Just sub    -> applySub subSyms sub invs args
        otherwise   -> retVal $ VError ("No compatible subroutine found: " ++ name) exp
    where
    applySub subSyms sub invs args
        -- list-associativity
        | Sub{ subAssoc = "list" }      <- sub
        , (App name' invs' args'):rest  <- args
        , name == name'
        , null invs'
        = applySub subSyms sub [] (args' ++ rest)
        -- fix subParams to agree with number of actual arguments
        | Sub{ subAssoc = "list", subParams = (p:_) }   <- sub
        , null invs
        = apply sub{ subParams = (length args) `replicate` p } [] args
        -- chain-associativity
        | Sub{ subAssoc = "chain", subFun = fun, subParams = prm }   <- sub
        , (App name' invs' args'):rest              <- args
        , Just sub'                                 <- findSub subSyms name'
        , Sub{ subAssoc = "chain", subFun = fun', subParams = prm' } <- sub'
        , null invs'
        = applySub subSyms sub{ subParams = prm ++ tail prm', subFun = Prim $ chainFun prm' fun' prm fun } [] (args' ++ rest)
        -- fix subParams to agree with number of actual arguments
        | Sub{ subAssoc = "chain", subParams = (p:_) }   <- sub
        , null invs
        = apply sub{ subParams = (length args) `replicate` p } [] args -- XXX Wrong
        -- normal application
        | otherwise
        = apply sub invs args
    findSub subSyms name = case sort (subs subSyms name) of
        ((_, sub):_)    -> Just sub
        _               -> Nothing
    subs subSyms name = [
        ( (isGlobal, subT, isMulti sub, bound, distance, order)
        , fromJust fun
        )
        | ((n, val), order) <- subSyms `zip` [0..]
        , let sub@(Sub{ subType = subT, subReturns = ret, subParams = prms }) = vCast val
        , (n ==) `any` [name, toGlobal name]
        , let isGlobal = '*' `elem` n
        , let fun = arityMatch sub (invs ++ args) -- XXX Wrong
        , isJust fun
        , deltaFromCxt ret /= 0
        , let invocants = filter isInvocant prms
        , let prms' = if null invocants then prms else invocants
        , let distance = (deltaFromCxt ret : map (deltaFromScalar . paramContext) prms')
        , let bound = either (const False) (const True) $ bindParams prms invs args
        ]
    deltaFromCxt            = deltaType cls cxt
    deltaFromScalar ('*':x) = deltaFromScalar x
    deltaFromScalar x       = deltaType cls x "Scalar"

doReduce env (Parens exp) = doReduce env exp
doReduce _ other = return other

-- OK... Now let's implement the hideously clever autothreading algorithm.
-- First pass - thread thru all() and none()
-- Second pass - thread thru any() and one()

chainFun :: Params -> Exp -> Params -> Exp -> [Val] -> Eval Val
chainFun p1 f1 p2 f2 (v1:v2:vs) = do
    val <- applyExp (chainArgs p1 [v1, v2]) f1
    case val of
        VBool False -> return val
        _           -> applyExp (chainArgs p2 (v2:vs)) f2
    where
    chainArgs prms vals = map chainArg (prms `zip` vals)
    chainArg (p, v) = ApplyArg (paramName p) v False

applyExp :: [ApplyArg] -> Exp -> Eval Val
applyExp bound (Prim f)
    = f [ argValue arg | arg <- bound, (argName arg !! 1) /= '_' ]
applyExp bound body = do
    -- XXX - resetT here -- XXX - Wrong -- XXX - FIXME
    enterLex formal $ evalExp body
    where
    formal = filter (not . null . symName) $ map argNameValue bound
    argNameValue (ApplyArg name val _) = Symbol SMy name (Val val)

apply :: VSub -> [Exp] -> [Exp] -> Eval Exp
apply sub invs args = do
    env <- ask
    doApply env sub invs args

-- XXX - faking application of lexical contexts
-- XXX - what about defaulting that depends on a junction?
doApply :: Env -> VSub -> [Exp] -> [Exp] -> Eval Exp
doApply env@Env{ envClasses = cls } sub@Sub{ subParams = prms, subFun = fun } invs args =
    case bindParams prms invs args of
        Left errMsg     -> retVal $ VError errMsg (Val VUndef)
        Right bindings  -> do
            bound <- doBind bindings
            val <- (`juncApply` bound) $ \realBound -> do
                enterSub sub $ do
                    applyExp realBound fun
            retVal val
    where
    doBind :: [(Param, Exp)] -> Eval [ApplyArg]
    doBind [] = return []
    doBind ((prm, exp):rest) = do
        (val, coll) <- expToVal prm exp
        let name = paramName prm
            arg = ApplyArg name val coll
        restArgs <- enterLex [Symbol SMy name (Val val)] $ do
            doBind rest
        return (arg:restArgs)
    expToVal Param{ isSlurpy = slurpy, paramContext = cxt } exp = do
        val <- enterEvalContext cxt exp
        return (val, (slurpy || isCollapsed cxt))
    isCollapsed cxt
        | isaType cls "Bool" cxt        = True
        | isaType cls "Junction" cxt    = True
        | isaType cls cxt "Any"         = True
        | otherwise                     = False

toGlobal name
    | (sigil, identifier) <- break (\x -> isAlpha x || x == '_') name
    , last sigil /= '*'
    = sigil ++ ('*':identifier)
    | otherwise = name

isGlobalExp (Syn name _) = name `elem` (words "::=")
isGlobalExp _ = False

findSym :: String -> Pad -> Maybe Exp
findSym name pad
    | Just s <- find ((== name) . symName) pad
    = Just $ symExp s
    | otherwise
    = Nothing

arityMatch sub@Sub{ subAssoc = assoc, subParams = prms } args
    | assoc == "list"               = Just sub
    | isJust $ find isSlurpy prms
    , assoc == "pre"                = Just sub
--  | (length prms == length args)  = Just sub -- XXX optionals
--  | (length prms >= length args)  = Just sub
    | otherwise                     = Just sub
    | otherwise                     = Nothing
