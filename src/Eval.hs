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

import AST
import Junc
import Bind
import Prim
import Context
import Monads
import Pretty

emptyEnv :: (MonadIO m) => m Env
emptyEnv = do
    uniq <- liftIO newUnique
    return $ Env
        { envContext = "List"
        , envPad     = initSyms
        , envClasses = initTree
        , envEval    = evaluate
        , envCC      = return
        , envDepth   = 0
        , envID      = uniq
        , envBody    = Val VUndef
        }

-- Evaluation ---------------------------------------------------------------

debug :: (Pretty a) => String -> a -> Eval ()
debug str a = do
    liftIO $ putStrLn ("*** " ++ str ++ ": " ++ pretty a)

evaluate :: Exp -> Eval Val
evaluate exp = do
    debug "Evaluating" exp
    val <- local (\e -> e { envBody = exp }) reduce
    return $ case val of
        Val v       -> v
        otherwise   -> VError "Invalid expression" exp

evalEnv :: Exp -> Eval Val
evalEnv exp = do
    evl <- asks envEval
    evl exp

evalEnvWithContext :: Cxt -> Exp -> Eval Val
evalEnvWithContext cxt exp = do
    local (\e -> e { envContext = cxt }) $ evalEnv exp

-- Reduction ---------------------------------------------------------------

reduce :: Eval Exp
reduce = do
    env@Env{ envBody = body } <- ask
    doReduce env body

retVal :: Val -> Eval Exp
retVal val = return $ Val val

reduceStatements [] = retVal VUndef
reduceStatements [exp] = do
    val <- evalEnv exp
    retVal val
reduceStatements (exp:rest)
    | Syn name [Var var _, exp'] <- exp
    , name == ":=" || name == "::=" 
    = do
        val <- evalEnvWithContext (cxtOfSigil (head var)) exp
        case val of
            VError _ _  -> retVal val
            _           -> enterLex [Symbol SMy var val] $ reduceStatements rest
    | otherwise
    = do { evalEnvWithContext "Any" exp; reduceStatements rest }

doReduce :: Env -> Exp -> Eval Exp

-- Reduction for variables
doReduce Env{ envPad = pad } exp@(Var var _)
    | Just val <- findSym var pad
    = retVal val
    | Just val <- findSym (toGlobal var) pad
    = retVal val
    | otherwise
    = retVal $ VError ("Undefined variable " ++ var) exp

-- Reduction for syntactic constructs
doReduce env@Env{ envContext = cxt } exp@(Syn name exps) = case name of
    ";" -> do
        let (global, local) = partition isGlobalExp exps
        reduceStatements (global ++ local)
    ":=" -> do
        let [Var var _, exp] = exps
        val     <- evalEnv exp
        retVal val
    "::=" -> do -- XXX wrong
        let [Var var _, exp] = exps
        val     <- evalEnv exp
        retVal VUndef -- XXX wrong
    "=>" -> do
        let [keyExp, valExp] = exps
        key     <- evalEnv keyExp
        val     <- evalEnv valExp
        retVal $ VPair key val
    "," -> do
        vals    <- mapM (evalEnvWithContext "List") exps
        retVal $ VList vals
    "[]" -> do
        let (listExp:rangeExp:errs) = exps
        list    <- evalEnvWithContext "List" listExp
        range   <- evalEnvWithContext "List" rangeExp
        let slice = unfoldr (doSlice errs $ vCast list) (map vCast $ vCast range)
        retVal $ VList slice
    "gather" -> do
        val     <- evalEnvWithContext "List" exp
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

doReduce env@Env{ envClasses = cls, envContext = cxt, envPad = pad } exp@(App name invs args) = do
    case findSub name of
        Just sub    -> applySub sub invs args
        otherwise   -> retVal $ VError ("No compatible subroutine found: " ++ name) exp
    where
    applySub sub invs args
        -- list-associativity
        | Sub{ subAssoc = "list" }      <- sub
        , (App name' invs' args'):rest  <- args
        , name == name'
        , null invs'
        = applySub sub [] (args' ++ rest)
        -- fix subParams to agree with number of actual arguments
        | Sub{ subAssoc = "list", subParams = (p:_) }   <- sub
        , null invs
        = apply sub{ subParams = (length args) `replicate` p } [] args
        -- chain-associativity
        | Sub{ subAssoc = "chain", subFun = fun, subParams = prm }   <- sub
        , (App name' invs' args'):rest              <- args
        , Just sub'                                 <- findSub name'
        , Sub{ subAssoc = "chain", subFun = fun', subParams = prm' } <- sub'
        , null invs'
        = applySub sub{ subParams = prm ++ tail prm', subFun = Prim $ chainFun prm' fun' prm fun } [] (args' ++ rest)
        -- fix subParams to agree with number of actual arguments
        | Sub{ subAssoc = "chain", subParams = (p:_) }   <- sub
        , null invs
        = apply sub{ subParams = (length args) `replicate` p } [] args -- XXX Wrong
        -- normal application
        | otherwise
        = apply sub invs args
    findSub name
        | ((_, sub):_) <- sort (subs name)  = Just sub
        | otherwise                         = Nothing
    subs name = [
        ( (isGlobal, subT, isMulti sub, bound, distance, order)
        , fromJust fun
        )
        | ((Symbol _ n val), order) <- pad `zip` [0..]
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
    val <- applyFun (chainArgs p1 [v1, v2]) f1
    case val of
        VBool False -> return val
        _           -> applyFun (chainArgs p2 (v2:vs)) f2
    where
    chainArgs prms vals = map chainArg (prms `zip` vals)
    chainArg (p, v) = ApplyArg (paramName p) v False

applyFun :: [ApplyArg] -> Exp -> Eval Val
applyFun bound (Prim f)
    = f [ argValue arg | arg <- bound, (argName arg !! 1) /= '_' ]
applyFun bound body = do
    -- XXX - resetT here
    enterLex formal $ evalEnv body
    where
    formal = filter (not . null . symName) $ map argNameValue bound
    argNameValue (ApplyArg name val _) = Symbol SMy name val

apply :: VSub -> [Exp] -> [Exp] -> Eval Exp
apply sub invs args = do
    env <- ask
    doApply env sub invs args

-- XXX - faking application of lexical contexts
-- XXX - what about defaulting that depends on a junction?
doApply :: Env -> VSub -> [Exp] -> [Exp] -> Eval Exp
doApply env@Env{ envClasses = cls } Sub{ subParams = prms, subFun = fun } invs args =
    case bindParams prms invs args of
        Left errMsg     -> retVal $ VError errMsg (Val VUndef)
        Right bindings  -> do
            bound <- doBind bindings
            retVal =<< juncApply (`applyFun` fun) bound
            -- juncApply eval (reverse . fst $ foldl doBind ([],env) bindings)
    where
    doBind :: [(Param, Exp)] -> Eval [ApplyArg]
    doBind [] = return []
    doBind ((prm, exp):rest) = do
        (val, coll) <- expToVal prm exp
        let name = paramName prm
            arg = ApplyArg name val coll
        restArgs <- enterLex [Symbol SMy name val] $ do
            doBind rest
        return (arg:restArgs)
    expToVal Param{ isSlurpy = slurpy, paramContext = cxt } exp = do
        val <- evalEnvWithContext cxt exp
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

isGlobalExp (Syn name _) = name `elem` (words ":= ::=")
isGlobalExp _ = False

findSym :: String -> Pad -> Maybe Val
findSym name pad
    | Just s <- find ((== name) . symName) pad
    = Just $ symValue s
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
