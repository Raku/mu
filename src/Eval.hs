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
    cxt     <- asks envContext
    if lvalue
        then return v
        else do
            rv <- liftIO (readIORef mv)
            evaluate (Val rv)
evaluate (Val val) = do
    -- context casting, go!
    cxt <- asks envContext
    return $ case cxt of
        "List"  -> VList (vCast val)
        "Array" -> VArray (vCast val)
        "Hash"  -> VHash (vCast val)
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
    return val

evalExp :: Exp -> Eval Val
evalExp exp = do
    evl <- asks envEval
    evl exp

evalSym :: Symbol -> Eval (String, Val)
evalSym (Symbol _ name vexp) = do
    val <- evalExp vexp
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

retError :: VStr -> Exp -> Eval a
retError str exp = do
    shiftT $ \_ -> return $ VError str exp

newMVal val@(MVal r) = newMVal =<< liftIO (readIORef r)
newMVal val = do
    mval <- liftIO $ newIORef val
    return $ MVal mval

writeMVal l (MVal r)    = writeMVal l =<< liftIO (readIORef r)
writeMVal (MVal l) r    = liftIO $ writeIORef l r
writeMVal l@(VError s e) _ = retError s e
writeMVal _ l@(VError s e) = retError s e
writeMVal x y           = retError "Can't write a constant item" (Val x)

-- readMVal (MVal mv) =  liftIO $ readIORef mv

addGlobalSym sym = do
    glob <- asks envGlobal
    liftIO $ do
        syms <- readIORef glob
        writeIORef glob (sym:syms)

reduceStatements :: ([(Exp, SourcePos)], Exp) -> Eval Val
reduceStatements ([], exp) = reduceExp exp
reduceStatements (((exp, pos):rest), lastVal)
    | Syn "sym" (Sym sym@(Symbol _ _ vexp@(Syn "sub" [sub])):other) <- exp = do
        (VSub sub) <- enterEvalContext "Code" vexp
        lex <- asks envLexical
        reduceStatements (((Syn "sym" (other ++ [Sym sym{ symExp = Val $ VSub sub{ subPad = lex } }]), pos):rest), lastVal)
    | Syn "sym" (Sym sym@(Symbol _ name (Syn "mval" [_, vexp])):other) <- exp = do
        val <- enterEvalContext (cxtOfSigil $ head name) vexp
        mval <- newMVal val
        reduceStatements (((Syn "sym" (other ++ [Sym sym{ symExp = Val mval }]), pos):rest), Val mval)
    | Syn "sym" [Sym sym@(Symbol SGlobal _ vexp)] <- exp = do
        addGlobalSym sym
        reduceStatements (rest, vexp)
    | Syn "sym" [Sym sym@(Symbol SOur _ vexp)] <- exp = do
        addGlobalSym sym -- XXX Wrong
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
                addGlobalSym $ Symbol SGlobal name vexp
                reduceStatements (rest, vexp)
    | Val (VSub sub) <- exp
    , subType sub >= SubBlock = do
        -- bare Block in statement level; run it!
        let app = Syn "()" [exp, Syn "invs" [], Syn "args" []]
        reduceStatements ((app, pos):rest, lastVal)
    | null rest = do
        cxt <- asks envContext
        val <- enterLex (posSyms pos) $ reduceExp exp
        retVal val
    | otherwise = do
        val <- enterContext "Void" $ do
            enterLex (posSyms pos) $ do
                reduceExp exp
        processVal val $ do
            reduceStatements (rest, Val val)
    where 
    processVal val action = case val of
        VError str exp  -> retError str exp
        _               -> action

posSyms pos = [ Symbol SMy n (Val v) | (n, v) <- syms ]
    where
    file = sourceName pos
    line = show $ sourceLine pos
    col  = show $ sourceColumn pos
    syms =
        [ ("$?FILE", castV file)
        , ("$?LINE", castV line)
        , ("$?POSITION", castV $ file ++ " at line " ++ line ++ ", column " ++ col)
        ]

evalVar name = do
    env <- ask
    val <- local (\e -> e{ envLValue = True }) $ do
        rv <- findVar name
        enterEvalContext (cxtOfSigil $ head name) $ case rv of
            Nothing -> (Val $ VError ("Undefined variable " ++ name) (Val VUndef))
            Just (Val val)  -> (Val val)
            Just exp        -> exp -- XXX Wrong
    return val

breakOnGlue _ [] = ([],[])
breakOnGlue glue rest@(x:xs)
    | glue `isPrefixOf` rest = ([], rest)
    | otherwise = (x:piece, rest') where (piece, rest') = breakOnGlue glue xs

findVar name
    | (package, name') <- breakOnGlue "::" name
    , (sig, "CALLER") <- breakOnGlue "CALLER" package = do
        rv <- asks envCaller
        case rv of
            Just caller -> findVar' caller (sig ++ (drop 2 name'))
            Nothing -> retError "cannot access CALLER:: in top level" (Var name)
    | otherwise = do
        env <- ask
        findVar' env name
    where
    findVar' env name = do
        let lexSym = findSym name $ envLexical env
        -- XXX rewrite using Maybe monad
        if isJust lexSym then return lexSym else do
            glob <- liftIO . readIORef $ envGlobal env
            let globSym = findSym name glob
            if isJust globSym
                then return globSym
                else
                    let globSym = findSym (toGlobal name) glob in
                    if isJust globSym
                        then return globSym
                        else return Nothing
    
reduce :: Env -> Exp -> Eval Val

-- Reduction for mutables
reduce env exp@(Val val@(MVal mv)) = do
    lvalue  <- asks envLValue
    if lvalue
        then retVal val
        else do
            rv <- readMVal val
            retVal rv

-- Reduction for constants
reduce env exp@(Val v) = do
    return v

-- Reduction for variables
reduce env exp@(Var name) = do
    rv <- findVar name
    case rv of
        (Just vexp) -> do
            enterContext (cxtOfSigil $ head name) $ reduceExp vexp
        _ -> retError ("Undefined variable " ++ name) exp

reduce env (Statements stmts) = do
    let (global, local) = partition isGlobalExp stmts
    reduceStatements (global ++ local, Val VUndef)
    where
    isGlobalExp (Syn name _, _) = name `elem` (words "::=")
    isGlobalExp _ = False
    
-- Reduction for syntactic constructs
reduce env@Env{ envContext = cxt } exp@(Syn name exps) = case name of
    "sub" -> do
        let [exp] = exps
        (VSub sub) <- enterEvalContext "Code" exp
        retVal $ VSub sub{ subPad = envLexical env }
    "sym" -> do
        mapM_ evalExp [ exp | Sym (Symbol _ _ exp) <- exps ]
        retVal VUndef
    "mval" -> do
        let [Var name, exp] = exps
        val     <- enterEvalContext (cxtOfSigil $ head name) exp
        retVal =<< newMVal val
    "if" -> doCond id 
    "unless" -> doCond not
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
    "while" -> doWhileUntil id
    "until" -> doWhileUntil not
    "=" -> do
        case exps of
            [Var name, exp] -> do
                val     <- evalVar name
                val'    <- enterEvalContext (cxtOfSigil $ head name) exp
                writeMVal val val'
                retVal val'
            [Syn "[]" [Var name, indexExp], exp] -> do
                listMVal <- evalVar name
                listVal  <- readMVal listMVal
                indexVal <- evalExp indexExp
                val' <- enterEvalContext "List" exp
                let index   = (vCast indexVal :: Integer) -- XXX slicing
                    list    = concatMap vCast (vCast listVal)
                    pre     = genericTake index (list ++ repeat VUndef)
                    post    = genericDrop (index + 1) list
                writeMVal listMVal $ VList (pre ++ [val'] ++ post)
                retVal val'
            [Syn "{}" [Var name, indexExp], exp] -> do
                hashMVal  <- evalVar name
                hashVal   <- readMVal hashMVal
                indexMVal <- evalExp indexExp
                indexVal  <- readMVal indexMVal
                val' <- enterEvalContext "Scalar" exp
                let hash    = addToFM (vCast hashVal) indexVal val'
                writeMVal hashMVal $ VHash $ MkHash hash
                retVal val'
            _ -> do
                retError "Cannot modify constant item" (head exps)
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
        retVal $ VPair (key, val)
    "," -> do
        vals    <- mapM (enterEvalContext "List") exps
        retVal $ VList $ concatMap vCast vals
    "cxt" -> do
        let [cxtExp, exp] = exps
        cxt     <- enterEvalContext "Str" cxtExp
        val     <- enterEvalContext (vCast cxt) exp
        retVal val
    "[]" -> do
        let (listExp:rangeExp:errs) = exps
        list    <- enterEvalContext "List" listExp
        range   <- enterEvalContext "List" rangeExp
        let slice = unfoldr (doSlice errs $ vCast list) (map vCast $ vCast range)
        cls     <- asks envClasses
        if isaType cls "Scalar" cxt
            then retVal $ last (VUndef:slice)
            else retVal $ VList slice
    "{}" -> do
        let [listExp, rangeExp] = exps
        hash     <- enterEvalContext "Hash" listExp
        range   <- enterEvalContext "List" rangeExp
        cls     <- asks envClasses
        let slice = map (lookupWithDefaultFM (vCast hash) VUndef) ((map vCast $ vCast range) :: [Val])
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
    doCond f = do
        let [cond, bodyIf, bodyElse] = exps
        vbool     <- enterEvalContext "Bool" cond
        if (f $ vCast vbool)
            then reduce env bodyIf
            else reduce env bodyElse
    -- XXX This treatment of while/until loops probably needs work
    doWhileUntil f = do
        let [ cond, body] = exps
        let runBody = do
            vbool <- enterEvalContext "Bool" cond
            case f $ vCast vbool of
                True -> do
                    reduce env body
                    runBody
                _ -> shiftT $ \_ -> return $ vbool
        val <- resetT runBody
        retVal val

reduce env@Env{ envClasses = cls, envContext = cxt, envLexical = lex, envGlobal = glob } exp@(App name invs args) = do
    syms    <- liftIO $ readIORef glob
    subSyms <- mapM evalSym
        [ sym | sym <- lex ++ syms
        , let n = symName sym
        , (n ==) `any` [name, toGlobal name]
        ]
    lens    <- mapM argSlurpLen (invs ++ args)
    case findSub (sum lens) subSyms name of
        Just sub    -> applySub subSyms sub invs args
        otherwise   -> retError ("No compatible subroutine found: " ++ name) exp
    where
    argSlurpLen (Val listMVal) = do
        listVal  <- readMVal listMVal
        return $ length (vCast listVal :: [Val])
    argSlurpLen (Var name) = do
        listMVal <- evalVar name
        listVal  <- readMVal listMVal
        return $ length (vCast listVal :: [Val])
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
        , (App name' args' []):rest                 <- invs
        , Just sub'                                 <- findSub 2 subSyms name'
        , Sub{ subAssoc = "chain", subFun = fun', subParams = prm' } <- sub'
        = applySub subSyms sub{ subParams = prm ++ tail prm', subFun = Prim $ chainFun prm' fun' prm fun } (args' ++ rest) []
        -- fix subParams to agree with number of actual arguments
        | Sub{ subAssoc = "chain", subParams = (p:_) }   <- sub
        , null args
        = apply sub{ subParams = (length invs) `replicate` p } invs [] -- XXX Wrong
        -- normal application
        | otherwise
        = apply sub invs args
    findSub slurpLen subSyms name = case sort (subs slurpLen subSyms name) of
        ((_, sub):_)    -> Just sub
        _               -> Nothing
    subs slurpLen subSyms name = [
        ( (isGlobal, subT, isMulti sub, bound, distance)
        , fromJust fun
        )
        | (n, val) <- subSyms
        , let sub@(Sub{ subType = subT, subReturns = ret, subParams = prms }) = vCast val
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

applyExp :: [ApplyArg] -> Exp -> Eval Val
applyExp bound (Prim f)
    = f [ argValue arg | arg <- bound, (argName arg !! 1) /= '_' ]
applyExp bound body = do
    -- XXX - resetT here -- XXX - Wrong -- XXX - FIXME
    enterLex formal $ evalExp body
    where
    formal = filter (not . null . symName) $ map argNameValue bound
    argNameValue (ApplyArg name val _) = Symbol SMy name (Val val)

apply :: VSub -> [Exp] -> [Exp] -> Eval Val
apply sub invs args = do
    env <- ask
    doApply env sub invs args

-- XXX - faking application of lexical contexts
-- XXX - what about defaulting that depends on a junction?
doApply :: Env -> VSub -> [Exp] -> [Exp] -> Eval Val
doApply env@Env{ envClasses = cls } sub@Sub{ subParams = prms, subFun = fun, subType = typ } invs args =
    case bindParams prms invs args of
        Left errMsg     -> retError errMsg (Val VUndef)
        Right bindings  -> do
            local fixEnv $ enterScope $ do
                bound <- doBind bindings
                -- trace (show bound) $ return ()
                val <- (`juncApply` bound) $ \realBound -> do
                    enterSub sub $ do
                        applyExp realBound fun
                retVal val
    where
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
        (val, coll) <- expToVal prm exp
        -- trace ("==> " ++ (show val)) $ return ()
        let name = paramName prm
            arg = ApplyArg name val coll
        val' <- enterEvalContext (cxtOfSigil $ head name) (Val val)
        restArgs <- enterLex [Symbol SMy name (Val val')] $ do
            doBind rest
        return (arg:restArgs)
    expToVal Param{ isLValue = lv, isSlurpy = slurpy, paramContext = cxt } exp = do
        val <- local (\e -> e{ envLValue = lv }) $ enterEvalContext cxt exp
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


findSym :: String -> Pad -> Maybe Exp
findSym name pad
    | Just s <- find ((== name) . symName) pad
    = Just $ symExp s
    | otherwise
    = Nothing

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
    --, error $ show (prms, reqLen, optLen, argLen)
    , argLen >= reqLen && argLen <= (reqLen + optLen)
    = Just sub
    | otherwise
    = Nothing
