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

emptyEnv :: (MonadIO m) => Pad -> m Env
emptyEnv pad = do
    ref  <- liftIO $ newIORef emptyFM
    uniq <- liftIO $ newUnique
    glob <- liftIO $ newIORef (pad ++ initSyms)
    return $ Env
        { envContext = "Void"
        , envLexical = []
        , envLValue  = False
        , envGlobal  = glob
        , envClasses = initTree
        , envEval    = evaluate
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
            when (length val > 100) $ do
                hPutStrLn stderr "*** Warning: deep recursion"
            writeIORef ref (addToFM fm key val)
            putStrLn ("***" ++ val ++ str ++ ": " ++ pretty a)

evaluateMain :: Exp -> Eval Val
evaluateMain exp = do
    val     <- evaluate exp
    endAV   <- evalVar "@*END"
    subs    <- readMVal endAV
    enterContext "Void" $ do
        mapM_ evalExp [ Syn "()" [Val sub, Syn "invs" [], Syn "args" []] | sub <- vCast subs ]
    return val

evaluate :: Exp -> Eval Val
evaluate (Val v@(MVal mv)) = do
    lvalue  <- asks envLValue
    _       <- asks envContext
    if lvalue
        then return v
        else do
            rv <- liftIO (readIORef mv)
            evaluate (Val rv)
evaluate (Val (VThunk (MkThunk t))) = t
evaluate (Val val) = do
    -- context casting, go!
    cxt <- asks envContext
    lv  <- asks envLValue
    v   <- if lv then return val else readMVal val
    return $ case cxt of
        _ | VSub _ <- v -> v -- XXX Work around a bug: (Val VSub) evaluated with bad context
        "List"  -> VList (vCast v)
        "Array" -> VArray (vCast v)
        "Hash"  -> VHash (vCast v)
--      "Str"   -> VStr (vCast v)
--      "Num"   -> VNum (vCast val)
--      "Int"   -> VInt (vCast val)
        "Scalar"-> val
        _       -> val
evaluate exp = do
    debug "indent" (' ':) "Evl" exp
    val <- local (\e -> e{ envBody = exp }) $ do
        reduceExp exp
    debug "indent" (tail) " Ret" val
    case val of
        VError s e  -> retError s e
        _           -> return val

evalExp :: Exp -> Eval Val
evalExp exp = do
    evl <- asks envEval
    evl exp

evalSym :: Symbol -> Eval (String, Val)
evalSym (SymVal _ name val) =
    return (name, val)
evalSym (SymExp _ name vexp) = do
    val <- enterEvalContext (cxtOfSigil $ head name) vexp
    return (name, val)
    
enterEvalContext :: Cxt -> Exp -> Eval Val
enterEvalContext cxt = enterContext cxt . evalExp

-- Reduction ---------------------------------------------------------------

reduceExp :: Exp -> Eval Val
reduceExp exp = do
    env <- ask
    reduce env exp

retVal :: Val -> Eval Val
retVal val = evaluate (Val val)  -- casting

newMVal (MVal r) = newMVal =<< liftIO (readIORef r)
newMVal val = do
    mval <- liftIO $ newIORef val
    return $ MVal mval

-- readMVal (MVal mv) =  liftIO $ readIORef mv

addGlobalSym sym = do
    glob <- asks envGlobal
    liftIO $ do
        syms <- readIORef glob
        writeIORef glob (sym:syms)

-- XXX This is a mess. my() and our() etc should not be statement level!
reduceStatements :: [(Exp, SourcePos)] -> Exp -> Eval Val
reduceStatements [] = reduceExp
reduceStatements ((exp, pos):rest)
    | Syn ";" exps <- exp = do
        reduceStatements $ (exps `zip` repeat pos) ++ rest
    | Sym [] <- exp = \v -> do
        reduceStatements rest v
    | Sym ((SymExp scope name vexp):other) <- exp = \v -> do
        val <- enterLValue $ enterEvalContext (cxtOfSigil $ head name) vexp
        reduceStatements ((Sym (SymVal scope name val:other), pos):rest) v
    | Sym (sym@(SymVal scope _ val):other) <- exp
    , scope == SOur || scope == SGlobal = \_ -> do
        addGlobalSym sym
        reduceStatements ((Sym other, pos):rest) (Val val)
    | Sym (sym@(SymVal SMy _ val):other) <- exp = \_ -> do
        enterLex [ sym ] $ do
            reduceStatements ((Sym other, pos):rest) (Val val)
    | Syn syn [Var name, vexp] <- exp
    , (syn == ":=" || syn == "::=") = \_ -> do
        env <- ask
        let lex = envLexical env
            val = VThunk . MkThunk $ do
            local (const env{ envLValue = True }) $ do
                enterEvalContext (cxtOfSigil $ head name) vexp
        case findSym name lex of
            Just _  -> do
                let sym = (SymVal SMy name val)
                enterLex [sym] $ do
                    reduceStatements rest (Val val)
            Nothing -> do
                addGlobalSym $ SymVal SGlobal name val
                reduceStatements rest (Val val)
    | Syn "sub" [Val (VSub sub)] <- exp
    , subType sub >= SubBlock = do
        -- bare Block in statement level; run it!
        let app = Syn "()" [exp, Syn "invs" [], Syn "args" []]
        reduceStatements $ (app, pos):rest
    | Syn "dump" [] <- exp
    , null rest = \e -> do
        Env{ envGlobal = globals, envLexical = lexicals } <- ask
        liftIO $ modifyIORef globals (lexicals ++)
        reduceStatements rest e
    | null rest = \_ -> do
        _   <- asks envContext
        val <- enterLex (posSyms pos) $ reduceExp exp
        retVal val
    | otherwise = \_ -> do
        val <- enterContext "Void" $ do
            enterLex (posSyms pos) $ do
                reduceExp exp
        processVal val $ do
            reduceStatements rest $ Val val
    where 
    processVal val action = case val of
        VError str exp  -> retError str exp
        _               -> action

posSyms pos = [ SymVal SMy n v | (n, v) <- syms ]
    where
    file = sourceName pos
    line = show $ sourceLine pos
    syms =
        [ ("$?FILE", castV file)
        , ("$?LINE", castV line)
        , ("$?POSITION", castV $ pretty pos)
        ]

evalVar name = do
    env <- ask
    v <- findVar env name
    return $ case v of
        Just val -> val
        Nothing -> VError ("Undefined variable " ++ name) (Val VUndef)

enterLValue = local (\e -> e{ envLValue = True })

findVar :: Env -> Ident -> Eval (Maybe Val)
findVar env name
    | Just (package, name') <- breakOnGlue "::" name
    , Just (sig, "") <- breakOnGlue "CALLER" package =
        case (envCaller env) of
            Just caller -> findVar caller (sig ++ name')
            Nothing -> retError "cannot access CALLER:: in top level" (Var name)
    | otherwise = do
        callCC $ \foundIt -> do
            let lexSym = findSym name $ envLexical env
            when (isJust lexSym) $ foundIt lexSym
            glob <- liftIO . readIORef $ envGlobal env
            let globSym = findSym name glob
            when (isJust globSym) $ foundIt globSym
            let globSym = findSym (toGlobal name) glob
            when (isJust globSym) $ foundIt globSym
            return Nothing

reduce :: Env -> Exp -> Eval Val

-- Reduction for mutables
reduce env (Val val@(MVal _)) = do
    if envLValue env
        then retVal val
        else do
            rv <- readMVal val
            retVal rv

-- Reduction for constants
reduce _ (Val v) = do
    retVal v

-- Reduction for variables
reduce env exp@(Var name) = do
    v <- findVar env name
    case v of
        Just val -> reduce env (Val val)
        _ -> retError ("Undefined variable " ++ name) exp

reduce _ (Statements stmts) = do
    let (global, local) = partition isGlobalExp stmts
    reduceStatements (global ++ local) $ Val VUndef
    where
    isGlobalExp (Syn name _, _) = name `elem` (words "::=")
    isGlobalExp _ = False
    
-- Reduction for syntactic constructs
reduce env@Env{ envContext = cxt } exp@(Syn name exps) = case name of
    "sub" -> do
        let [exp] = exps
        (VSub sub) <- enterEvalContext "Code" exp
        retVal $ VSub sub{ subPad = envLexical env }
    "mval" -> do
        let [exp] = exps
        val     <- evalExp exp
        retVal =<< newMVal val
    "if" -> doCond id 
    "unless" -> doCond not
    "for" -> do
        let [list, body] = exps
        vlist <- enterEvalContext "List" list
        vsub  <- enterEvalContext "Code" body
        VSub sub <- fromVal vsub
        let arity = length (subParams sub)
            vals = concatMap vCast $ vCast vlist
            runBody [] = retVal VUndef
            runBody vs = do
                let (these, rest) = arity `splitAt` vs
                doApply env sub [] $ map Val these
                runBody rest
        enterLoop $ runBody vals
    "loop" -> do
        let [pre, cond, post, body] = exps
        evalExp pre
        let runBody = do
            valBody <- evalExp body
            valPost <- evalExp post
            vbool   <- enterEvalContext "Bool" cond
            case valBody of
                VError _ _ -> retVal valBody
                _ | VError _ _ <- valPost -> retVal valPost
                _ | not (vCast vbool) -> retVal valBody
                _ -> runBody
        enterLoop runBody
    "while" -> doWhileUntil id
    "until" -> doWhileUntil not
    "=" -> do
        case exps of
            [lhsExp@(Syn "," lhs), exp] -> do
                val'     <- enterEvalContext "List" exp
                -- start distribution with slurpy logic and return the lhs
                mapM_ evalExp [ Syn "=" [l, Val v] | l <- lhs | v <- vCast val' ]
                lhsVal   <- enterLValue $ enterEvalContext "List" lhsExp
                retVal lhsVal
            [Var name, exp] -> do
                val     <- evalVar name
                val'    <- enterEvalContext (cxtOfSigil $ head name) exp
                writeMVal val val'
                retVal val'
            [Syn "[]" [Var name, indexExp], exp] -> do
                listMVal <- evalVar name
                listVal  <- readMVal listMVal
                indexVal <- evalExp indexExp
                -- XXX Wrong -- the Context here should be "cxtFromExp exp"
                -- so that @x[1,] imposes List context by @x[1] imposes Scalar.
                val'     <- enterEvalContext "List" exp
                valList  <- mapM newMVal $ vCast val'
                let indexes = (map vCast $ vCast indexVal :: [VInt])
                    list = concatMap vCast $ case listVal of
                        VUndef  -> [] -- autovivification
                        _       -> vCast listVal
                    assignTo curr (index, val)
                        | index < 0
                        , index' <- index + genericLength curr
                        , index' >= 0
                        = assignTo curr (index', val)
                        | index < 0
                        = retError "Modification of non-creatable array value attempted" (Val $ VInt index)
                        | otherwise = do
                        pre <- preM
                        return $ pre ++ [val] ++ genericDrop (index + 1) curr
                        where
                        preM    = do   
                            let currM = map return curr ++ repeat (newMVal VUndef)
                            sequence $ genericTake index currM
                newList  <- foldM assignTo list $ indexes `zip` valList
                writeMVal listMVal $ VList newList
                retVal val'
            [Syn "{}" [Var name, indexExp], exp] -> do
                hashMVal  <- evalVar name
                hashVal   <- readMVal hashMVal
                indexMVal <- evalExp indexExp
                indexVal  <- readMVal indexMVal
                val'      <- enterEvalContext "Scalar" exp
                valScalar <- newMVal val'
                let hash = addToFM fm indexVal valScalar
                    fm = case hashVal of
                            VUndef  -> emptyFM -- autovivification
                            _       -> vCast hashVal
                writeMVal hashMVal $ VHash $ MkHash hash
                retVal val'
            _ -> do
                -- XXX LValue ??::
                retError "Cannot modify constant item" (head exps)
    ":=" -> do
        let [Var name, exp] = exps
        val     <- enterEvalContext (cxtOfSigil $ head name) exp
        retVal val
    "::=" -> do -- XXX wrong
        let [Var _, exp] = exps
        evalExp exp
        retVal VUndef -- XXX wrong
    "=>" -> do
        let [keyExp, valExp] = exps
        key     <- enterEvalContext "Scalar" keyExp
        val     <- evalExp valExp
        retVal $ VPair (key, val)
    "*" -> do -- first stab at an implementation
        vals <- case exps of
            [Val v] | valType v == "Array" -> do
                val <- enterEvalContext "List" $ head exps
                fromVal val
            _ -> mapM (enterEvalContext "List") exps         
        cls  <- asks envClasses
        if isaType cls "Scalar" cxt          
            then do
                let slice = case (doSlice [] vals $ 0:[]) of {
                    (Just (val, _)) -> val ;
                    Nothing         -> vCast VUndef
                }
                retVal $ vCast slice
            else retVal $ VList $ concatMap vCast vals
    "," -> do
        vals    <- mapM (enterEvalContext "List") exps
        retVal $ VList $ concatMap vCast vals
    "cxt" -> do
        let [cxtExp, exp] = exps
        cxt     <- enterEvalContext "Str" cxtExp
        val     <- enterEvalContext (vCast cxt) exp
        enterEvalContext (vCast cxt) (Val val) -- force casting
    "[]" -> do
        let (listExp:rangeExp:errs) = exps
        range   <- enterEvalContext "List" rangeExp
        listVal <- enterEvalContext "List" listExp
        list    <- readMVal listVal
        let slice = unfoldr (doSlice errs $ vCast list) (map vCast $ vCast range)
        ifContextIsa "Scalar"
            (retVal $ last (VUndef:slice))
            (retVal $ VList slice)
    "{}" -> do
        let [listExp, rangeExp] = exps
        range   <- enterEvalContext "List" rangeExp
        hashVal  <- enterEvalContext "Hash" listExp
        hash    <- readMVal hashVal
        cls     <- asks envClasses
        let slice = map (lookupWithDefaultFM (vCast hash) VUndef) ((map vCast $ vCast range) :: [Val])
        if isaType cls "Scalar" cxt
            then retVal $ last (VUndef:slice)
            else retVal $ VList slice
    "()" -> do
        let [subExp, Syn "invs" invs, Syn "args" args] = exps
        vsub <- enterEvalContext "Code" subExp
        sub <- fromVal vsub
        apply sub invs args
    "try" -> do
        val <- resetT $ evalExp (head exps)
        retEvalResult False val
    "gather" -> do
        val     <- enterEvalContext "List" exp
        -- ignore val
        retVal val
    syn | last syn == '=' -> do
        let [lhs, exp] = exps
            op = "&infix:" ++ init syn
        evalExp $ Syn "=" [lhs, App op [lhs, exp] []]
    _ -> retError "Unknown syntactic construct" exp
    where
    doSlice :: [Exp] -> [Val] -> [VInt] -> Maybe (Val, [VInt])
    doSlice errs vs (n:ns)
        | n < 0
        , n' <- n + genericLength vs
        , n' >= 0
        = doSlice errs vs (n':ns)
        | n < 0
        = Nothing
        | (v:_)         <- n `genericDrop` vs
        = Just (v, ns)
        | ((Val err):_) <- errs
        = Just (err, ns)
        | otherwise
        = Nothing
    doSlice _ _ _ = Nothing
    doCond f = do
        let [cond, bodyIf, bodyElse] = exps
        vbool     <- enterEvalContext "Bool" cond
        if (f $ vCast vbool)
            then reduce env bodyIf
            else reduce env bodyElse
    -- XXX This treatment of while/until loops probably needs work
    doWhileUntil f = do
        let [cond, body] = exps
        let runBody = do
            vbool <- enterEvalContext "Bool" cond
            case f $ vCast vbool of
                True -> do
                    rv <- reduce env body
                    case rv of
                        VError _ _  -> retVal rv
                        _           -> runBody
                _ -> retVal vbool
        enterLoop runBody

reduce env (App name [Syn "," invs] args) = reduce env (App name invs args)
reduce env (App name invs [Syn "," args]) = reduce env (App name invs args)

-- XXX absolutely evil bloody hack for "goto"
reduce _ (App "&goto" (subExp:invs) args) = do
    vsub <- enterEvalContext "Code" subExp
    sub <- fromVal vsub
    local callerEnv $ do
        val <- apply sub invs args
        shiftT $ \_ -> retVal val
    where
    callerEnv env = let caller = maybe env id (envCaller env) in
        env{ envCaller = envCaller caller
           , envContext = envContext caller
           , envLValue = envLValue caller
           }


reduce Env{ envClasses = cls, envContext = cxt, envLexical = lex, envGlobal = glob } exp@(App name invs args) = do
    syms    <- liftIO $ readIORef glob
    subSyms <- mapM evalSym
        [ sym | sym <- lex ++ syms
        , let n = symName sym
        , (n ==) `any` [name, toGlobal name]
        ]
    lens <- mapM argSlurpLen (invs ++ args)
    sub <- findSub (sum lens) subSyms
    case sub of
        Just sub    -> applySub subSyms sub invs args
        Nothing     -> retError ("No compatible subroutine found: " ++ name) exp
    where
    argSlurpLen (Val listMVal) = do
        listVal  <- readMVal listMVal
        return $ length (vCast listVal :: [Val])
    argSlurpLen (Var name) = do
        listMVal <- evalVar name
        listVal  <- readMVal listMVal
        return $ length (vCast listVal :: [Val])
    argSlurpLen (Syn "," list) =  return $ length list
    argSlurpLen _ = return 1 -- XXX
    applySub subSyms sub invs args
        -- list-associativity
        | Sub{ subAssoc = "list" }      <- sub
        , (App name' invs' []):rest  <- invs
        , name == name'
        = applySub subSyms sub (invs' ++ rest)  []
        -- fix subParams to agree with number of actual arguments
        | Sub{ subAssoc = "list", subParams = (p:_) }   <- sub
        , null args
        = apply sub{ subParams = (length invs) `replicate` p } invs []
        -- chain-associativity
        | Sub{ subAssoc = "chain" }   <- sub
        , (App _ _ []):_              <- invs
        , null args
        = mungeChainSub sub invs
        | Sub{ subAssoc = "chain", subParams = (p:_) }   <- sub
        = apply sub{ subParams = (length invs) `replicate` p } invs []
        -- normal application
        | otherwise
        = apply sub invs args
    mungeChainSub sub invs = do
        let Sub{ subAssoc = "chain", subParams = (p:_) } = sub
            (App name' invs' args'):rest = invs
        syms    <- liftIO $ readIORef glob
        subSyms' <- mapM evalSym

            [ sym | sym <- lex ++ syms
            , let n = symName sym
            , (n ==) `any` [name', toGlobal name']
            ]
        lens'    <- mapM argSlurpLen (invs' ++ args')
        theSub <- findSub (sum lens') subSyms'
        case theSub of
            Just sub'    -> applyChainSub subSyms' sub invs sub' invs' args' rest
            Nothing      -> apply sub{ subParams = (length invs) `replicate` p } invs [] -- XXX Wrong
            -- retError ("No compatible subroutine found: " ++ name') exp
    applyChainSub subSyms sub invs sub' invs' args' rest
        | Sub{ subAssoc = "chain", subFun = fun, subParams = prm }   <- sub
        , Sub{ subAssoc = "chain", subFun = fun', subParams = prm' } <- sub'
        , null args'
        = applySub subSyms sub{ subParams = prm ++ tail prm', subFun = Prim $ chainFun prm' fun' prm fun } (invs' ++ rest) []
        | Sub{ subAssoc = "chain", subParams = (p:_) }   <- sub
        = apply sub{ subParams = (length invs) `replicate` p } invs [] -- XXX Wrong
        | otherwise
        = internalError "applyChainsub did not match a chain subroutine"
    findSub slurpLen subSyms = do
        subs' <- subs slurpLen subSyms
        return $ case sort subs' of
            ((_, sub):_)    -> Just sub
            _               -> Nothing
    subs slurpLen subSyms = (liftM catMaybes) $ (`mapM` subSyms) $ \(n, val) -> do
        sub@(Sub{ subType = subT, subReturns = ret, subParams = prms }) <- fromVal val
        let isGlobal = '*' `elem` n
        let fun = arityMatch sub (length (invs ++ args)) slurpLen
        if not (isJust fun) then return Nothing else do
        if not (deltaFromCxt ret /= 0) then return Nothing else do
        let invocants = filter isInvocant prms
        let prms' = if null invocants then prms else invocants
        let distance = (deltaFromCxt ret : map (deltaFromScalar . paramContext) prms')
        let bound = either (const False) (const True) $ bindParams prms invs args
        return $ Just
            ( (isGlobal, subT, isMulti sub, bound, distance)
            , fromJust fun
            )
    deltaFromCxt            = deltaType cls cxt
    deltaFromScalar ('*':x) = deltaFromScalar x
    deltaFromScalar x       = deltaType cls x "Scalar"

reduce env (Parens exp) = reduce env exp

reduce _ exp = retError "Invalid expression" exp

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
chainFun _ _ _ _ _ = internalError "chainFun: Not enough parameters in Val list"

applyExp :: [ApplyArg] -> Exp -> Eval Val
applyExp bound (Prim f) =
    f [ argValue arg | arg <- bound, (argName arg !! 1) /= '_' ]
applyExp bound body = do
    -- XXX - resetT here -- XXX - Wrong -- XXX - FIXME
    enterLex formal $ evalExp body
    where
    formal = filter (not . null . symName) $ map argNameValue bound
    argNameValue (ApplyArg name val _) = SymVal SMy name val

apply :: VSub -> [Exp] -> [Exp] -> Eval Val
apply sub invs args = do
    env <- ask
    doApply env sub invs args

-- XXX - faking application of lexical contexts
-- XXX - what about defaulting that depends on a junction?
doApply :: Env -> VSub -> [Exp] -> [Exp] -> Eval Val
doApply Env{ envClasses = cls } sub@Sub{ subParams = prms, subFun = fun, subType = typ } invs args =
    case bindParams prms invs args of
        Left errMsg     -> retError errMsg (Val VUndef)
        Right bindings  -> do
            enterScope $ do
                bound <- doBind bindings
                -- trace (show bound) $ return ()
                val <- local fixEnv $ do
                    (`juncApply` bound) $ \realBound -> do
                        enterSub sub $ do
                            applyExp realBound fun
                retVal val
    where
    enterScope :: Eval Val -> Eval Val
    enterScope
        | typ >= SubBlock = id
        | otherwise      = resetT
    fixEnv env
        | typ >= SubBlock = env
        | otherwise      = env{ envCaller = Just env }
    doBind :: [(Param, Exp)] -> Eval [ApplyArg]
    doBind [] = return []
    doBind ((prm, exp):rest) = do
        -- trace ("<== " ++ (show (prm, exp))) $ return ()
        (val, coll) <- case exp of
            Parens exp  -> local fixEnv $ expToVal prm exp -- default
            _           -> expToVal prm exp
        -- trace ("==> " ++ (show val)) $ return ()
        let name = paramName prm
            arg = ApplyArg name val coll
        val' <- enterEvalContext (cxtOfSigil $ head name) (Val val)
        restArgs <- enterLex [SymVal SMy name val'] $ do
            doBind rest
        return (arg:restArgs)
    expToVal Param{ isThunk = thunk, isLValue = lv, isSlurpy = slurpy, paramContext = cxt } exp = do
        env <- ask -- freeze environment at this point for thunks
        let eval = local (const env{ envLValue = lv }) $ enterEvalContext cxt exp
        val <- if thunk then return (VThunk $ MkThunk eval) else eval
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


arityMatch sub@Sub{ subAssoc = assoc, subParams = prms } argLen argSlurpLen
    | assoc == "list" || assoc == "chain"
    = Just sub
    | isNothing $ find (not . isSlurpy) prms -- XXX - what about empty ones?
    , assoc == "pre"
    , slurpLen <- length $ filter (\p -> isSlurpy p && head (paramName p) == '$') prms
    , hasArray <- isJust $ find (\p -> isSlurpy p && head (paramName p) == '@') prms
    , if hasArray then slurpLen <= argSlurpLen else slurpLen == argSlurpLen
    = Just sub
    | reqLen <- length $ filter (\p -> not (isOptional p || isSlurpy p)) prms
    , optLen <- length $ filter (\p -> isOptional p) prms
    , hasArray <- isJust $ find (\p -> isSlurpy p && head (paramName p) == '@') prms
    , argLen >= reqLen && (hasArray || argLen <= (reqLen + optLen))
    = Just sub
    | otherwise
    = Nothing
