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
evaluate (Val (MVal mv)) = do
    rv <- liftIO (readIORef mv)
    evaluate (Val rv)
evaluate (Val val) = return val
evaluate exp = do
    debug "indent" (' ':) "Evl" exp
    exp' <- local (\e -> e{ envBody = exp }) reduce
    debug "indent" (tail) " Ret" exp'
    case exp' of
        Val v       -> return v
        otherwise   -> retError "Invalid expression" exp'

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

retError str exp = do
    shiftT $ \_ -> return $ VError str exp

newMVal val@(MVal r) = newMVal =<< liftIO (readIORef r)
newMVal val = do
    mval <- liftIO $ newIORef val
    return $ MVal mval

writeMVal l (MVal r)    = writeMVal l =<< liftIO (readIORef r)
writeMVal (MVal l) r    = liftIO $ writeIORef l r
writeMVal _ _           = error "Can't write a constant item"

reduceStatements :: ([Exp], Exp) -> Eval Exp
reduceStatements ([], exp) = reduceExp exp
reduceStatements ((exp:rest), _)
    | Syn "sym" (Sym sym@(Symbol _ name (Syn "mval" [_, vexp])):other) <- exp = do
        val <- enterEvalContext (cxtOfSigil $ head name) vexp
        mval <- newMVal val
        reduceStatements ((Syn "sym" (other ++ [Sym sym{ symExp = Val mval }]):rest), Val mval)
    | Syn "sym" [Sym sym@(Symbol SGlobal _ vexp)] <- exp = do
        local (\e -> e{ envGlobal = (sym:envGlobal e) }) $ do
            reduceStatements (rest, vexp)
    | Syn "sym" syms <- exp = do
        enterLex [ sym | Sym sym@(Symbol SMy _ _) <- syms] $ do
            reduceStatements (rest, (Syn "sym" syms))
    | Syn syn [Var name, vexp] <- exp
    , (syn == ":=" || syn == "::=") = do
        lex <- asks envLexical
        case findSym name lex of
            Just _  -> do
                let sym = (Symbol SMy name vexp)
                enterLex [sym] $ do
                    reduceStatements (rest, vexp)
            Nothing -> do
                let sym = (Symbol SGlobal name vexp)
                local (\e -> e{ envGlobal = (sym:envGlobal e) }) $ do
                    reduceStatements (rest, vexp)
    | null rest = do
        cxt <- asks envContext
        val <- evalExp exp
        return (Val val)
    | otherwise = do
        val <- enterContext "Void" $ evalExp exp
        processVal val $ do
            reduceStatements (rest, Val val)
    where
    processVal val action = case val of
        VError str exp  -> retError str exp
        _               -> action

evalVar name = do
    env <- ask
    case findVar env name of
        Nothing -> retError ("Undefined variable " ++ name) (Val VUndef)
        Just (Val val)  -> return val
        Just exp        -> evalExp exp -- XXX Wrong

findVar Env{ envLexical = lex, envGlobal = glob } name
    | Just vexp <- findSym name lex
    = Just vexp
    | Just vexp <- findSym name glob
    = Just vexp
    | Just vexp <- findSym (toGlobal name) glob
    = Just vexp
    | otherwise
    = Nothing
    
doReduce :: Env -> Exp -> Eval Exp

doReduce env exp@(Val (MVal mv)) = do
    cxt <- asks envContext
    if cxt == "LValue"
        then return exp
        else do
            rv <- liftIO (readIORef mv)
            doReduce env (Val rv)

-- Reduction for variables
doReduce env exp@(Var name)
    | Just vexp <- findVar env name
    = reduceExp vexp
    | otherwise
    = retError ("Undefined variable " ++ name) exp

-- Reduction for syntactic constructs
doReduce env@Env{ envContext = cxt } exp@(Syn name exps) = case name of
    ";" -> do
        let (global, local) = partition isGlobalExp exps
        reduceStatements (global ++ local, Val VUndef)
    "sym" -> do
        mapM_ evalExp [ exp | Sym (Symbol _ _ exp) <- exps ]
        retVal VUndef
    "mval" -> do
        let [Var name, exp] = exps
        val     <- enterEvalContext (cxtOfSigil $ head name) exp
        retVal =<< newMVal val
    "if" -> do
        let [cond, bodyIf, bodyElse] = exps
        vbool     <- enterEvalContext "Bool" cond
        if (vCast vbool)
            then doReduce env bodyIf
            else doReduce env bodyElse
    "for" -> do
        let [list, body] = exps
        vlist <- enterEvalContext "List" list
        vsub  <- enterEvalContext "Code" body
        let vals = concatMap vCast $ vCast vlist
            runBody [] = retVal VUndef
            runBody (v:vs) = do
                doApply env (vCast vsub) [] [Val v]
                runBody vs
        runBody vals
    "loop" -> do
        let [pre, cond, post, body] = exps
        evalExp pre
        -- enter the block
        -- first, run pre and enter its lexical context
        -- reduceStatements (pre:, Val VUndef)
        let runBody = do
            valBody <- evalExp body
            valPost <- evalExp post
            vbool   <- enterEvalContext "Bool" cond
            case valBody of
                VError _ _ -> shiftT $ \_ -> return valBody
                _ | VError _ _ <- valPost -> shiftT $ \_ -> return valPost
                _ | not (vCast vbool) -> shiftT $ \_ -> return valBody
                _ -> runBody
        val <- resetT runBody
        retVal val
    "=" -> do
        case exps of
            [Var name, exp] -> do
                val <- evalVar name
                val' <- enterEvalContext (cxtOfSigil $ head name) exp
                writeMVal val val'
                retVal val'
            [Syn "[]" [Var name, indexExp], exp] -> do
                listMVal <- evalVar name
                listVal  <- readMVal listMVal
                indexVal <- evalExp indexExp
                val' <- enterEvalContext (cxtOfSigil $ head name) exp
                let index   = (vCast indexVal :: Integer)
                    list    = concatMap vCast (vCast listVal)
                    pre     = genericTake index (list ++ repeat VUndef)
                    post    = genericDrop (index + 1) list
                writeMVal listMVal $ VList (pre ++ [val'] ++ post)
                retVal val'
    ":=" -> do
        let [Var name, exp] = exps
        val     <- enterEvalContext (cxtOfSigil $ head name) exp
        retVal val
    "::=" -> do -- XXX wrong
        let [Var name, exp] = exps
        val     <- evalExp exp
        retVal VUndef -- XXX wrong
    "=>" -> do
        let [keyExp, valExp] = exps
        key     <- enterEvalContext "Scalar" keyExp
        val     <- evalExp valExp
        retVal $ VPair key val
    "," -> do
        vals    <- mapM (enterEvalContext "List") exps
        retVal $ VList $ concatMap vCast vals
    "cxt" -> do
        let [cxtExp, exp] = exps
        cxt     <- enterEvalContext "Str" cxtExp
        retVal =<< enterEvalContext (vCast cxt) exp
    "[]" -> do
        let (listExp:rangeExp:errs) = exps
        list    <- enterEvalContext "List" listExp
        range   <- enterEvalContext "List" rangeExp
        let slice = unfoldr (doSlice errs $ vCast list) (map vCast $ vCast range)
        cls     <- asks envClasses
        if isaType cls "Scalar" cxt
            then retVal $ last (VUndef:slice)
            else retVal $ VList slice
    "()" -> do
        let [subExp, Syn "invs" invs, Syn "args" args] = exps
        sub     <- enterEvalContext "Code" subExp
        apply (vCast sub) invs args
    "gather" -> do
        val     <- enterEvalContext "List" exp
        -- ignore val
        retVal val
    _ -> retError "Unknown syntactic construct" exp
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
    lens    <- mapM argSlurpLen (invs ++ args)
    case findSub (sum lens) subSyms name of
        Just sub    -> applySub subSyms sub invs args
        otherwise   -> retError ("No compatible subroutine found: " ++ name) exp
    where
    argSlurpLen (Val listMVal) = do
        listVal  <- readMVal listMVal
        return $ vCast listVal
    argSlurpLen (Var name) = do
        listMVal <- evalVar name
        listVal  <- readMVal listMVal
        return $ vCast listVal
    argSlurpLen arg = return 1
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
        , Just sub'                                 <- findSub 2 subSyms name'
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
    findSub slurpLen subSyms name = case sort (subs slurpLen subSyms name) of
        ((_, sub):_)    -> Just sub
        _               -> Nothing
    subs slurpLen subSyms name = [
        ( (isGlobal, subT, isMulti sub, bound, distance, order)
        , fromJust fun
        )
        | ((n, val), order) <- subSyms `zip` [0..]
        , let sub@(Sub{ subType = subT, subReturns = ret, subParams = prms }) = vCast val
        , (n ==) `any` [name, toGlobal name]
        , let isGlobal = '*' `elem` n
        , let fun = arityMatch sub (length (invs ++ args)) slurpLen
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
        Left errMsg     -> retError errMsg (Val VUndef)
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

arityMatch sub@Sub{ subAssoc = assoc, subParams = prms } argLen argSlurpLen
    | assoc == "list" || assoc == "chain"
    = Just sub
    | isNothing $ find (not . isSlurpy) prms
    , slurpLen <- length $ filter (\p -> isSlurpy p && head (paramName p) == '$') prms
    , hasArray <- isJust $ find (\p -> isSlurpy p && head (paramName p) == '@') prms
    , if hasArray then slurpLen <= argSlurpLen else slurpLen == argSlurpLen
    , assoc == "pre"
    = Just sub
    | reqLen <- length $ filter (\p -> not $ isOptional p) prms
    , optLen <- length $ filter (\p -> isOptional p) prms
    , argLen >= reqLen && argLen <= (reqLen + optLen)
    --, error $ show (prms, reqLen, optLen, argLen)
    = Just sub
    | otherwise
    = Nothing

