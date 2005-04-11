{-# OPTIONS_GHC -fglasgow-exts #-}

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
import qualified Data.Map as Map

import AST
import Junc
import Bind
import Prim
import Context
import Monads
import Pretty
import Types
import qualified Types.Hash as Hash
import qualified Types.Array as Array

emptyEnv :: (MonadIO m) => Pad -> m Env
emptyEnv pad = do
    ref  <- liftIO $ newIORef Map.empty
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
            let val = fun $ Map.findWithDefault "" key fm
            when (length val > 100) $ do
                hPutStrLn stderr "*** Warning: deep recursion"
            writeIORef ref (Map.insert key val fm)
            putStrLn ("***" ++ val ++ str ++ ": " ++ pretty a)

evaluateMain :: Exp -> Eval Val
evaluateMain exp = do
    val     <- resetT $ evaluate exp
    endAV   <- evalVar "@*END"
    subs    <- fromVal endAV
    enterContext "Void" $ do
        mapM_ evalExp [ Syn "()" [Val sub, Syn "invs" [], Syn "args" []]
                      | sub <- vCast subs
                      ]
    return val

evaluate :: Exp -> Eval Val
evaluate (Val v@(VRef var)) = do
    lvalue  <- asks envLValue
    _       <- asks envContext
    if lvalue
        then return v
        else do
            rv <- readRef var -- liftIO (readIORef mv)
            evaluate (Val rv)
evaluate (Val (VThunk (MkThunk t))) = t
evaluate (Val val) = evalVal val
evaluate exp = do
    debug "indent" (' ':) "Evl" exp
    val <- local (\e -> e{ envBody = exp }) $ do
        reduceExp exp
    debug "indent" (tail) " Ret" val
    trapVal val (return val)

evalSym :: Symbol a -> Eval (String, Val)
evalSym (SymVar _ name var) = do
    val <- readRef var
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
        ref <- newObject (cxtOfSigil $ head name) val
        let doRest = reduceStatements ((Sym other, pos):rest) v
            sym = SymVar scope name ref
        case scope of
            SMy -> enterLex [ sym ] doRest
            _   -> do
                addGlobalSym sym
                doRest
    | Syn syn [Var name, vexp] <- exp
    , (syn == ":=" || syn == "::=") = const $ do
        env <- ask
        let lex = envLexical env
            val = VThunk . MkThunk $ do
            local (const env{ envLValue = True }) $ do
                enterEvalContext (cxtOfSigil $ head name) vexp
        case findSym name lex of
            Just _  -> do
                let sym = (SymVar SMy name $ scalarRef val)
                enterLex [sym] $ do
                    reduceStatements rest (Val val)
            Nothing -> do
                addGlobalSym $ SymVar SGlobal name (scalarRef val)
                reduceStatements rest (Val val)
    | Syn "sub" [Val (VCode sub)] <- exp
    , subType sub >= SubBlock = do
        -- bare Block in statement level; run it!
        let app = Syn "()" [exp, Syn "invs" [], Syn "args" []]
        reduceStatements $ (app, pos):rest
    | Syn "dump" [] <- exp
    , null rest = \e -> do
        Env{ envGlobal = globals, envLexical = lexicals } <- ask
        liftIO $ modifyIORef globals (lexicals ++)
        reduceStatements rest e
    | null rest = const $ do
        _   <- asks envContext
        val <- enterLex (posSyms pos) $ reduceExp exp
        retVal val
    | otherwise = const $ do
        val <- enterContext "Void" $ do
            enterLex (posSyms pos) $ do
                reduceExp exp
        trapVal val $ do
            reduceStatements rest $ Val val

trapVal :: Val -> Eval a -> Eval a
trapVal val action = case val of
    VError str exp  -> retError str exp
    VControl c      -> retControl c
    _               -> action

posSyms pos = [ SymVar SMy n v | (n, v) <- syms ]
    where
    file = sourceName pos
    line = show $ sourceLine pos
    syms =
        [ ("$?FILE", scalarRef $ castV file)
        , ("$?LINE", scalarRef $ castV line)
        , ("$?POSITION", scalarRef $ castV $ pretty pos)
        ]

evalVar :: Ident -> Eval Val
evalVar name = do
    env <- ask
    v <- findVar env name
    case v of
        Just var -> readRef var
        Nothing  -> retError ("Undeclared variable " ++ name) (Val VUndef)

enterLValue = local (\e -> e{ envLValue = True })

findVar :: Env -> Ident -> Eval (Maybe VRef)
findVar env name
    | Just (package, name') <- breakOnGlue "::" name
    , Just (sig, "") <- breakOnGlue "CALLER" package =
        case (envCaller env) of
            Just caller -> findVar caller (sig ++ name')
            Nothing -> retError "cannot access CALLER:: in top level" (Var name)
    | otherwise = 
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
reduce env (Val v@(VRef var)) = do
    if envLValue env
        then retVal v
        else do
            rv <- readRef var
            retVal rv

-- Reduction for constants
reduce _ (Val v) = do
    retVal v

-- Reduction for variables
reduce env exp@(Var name) = do
    v <- findVar env name
    case v of
        Just var -> do
            if envLValue env then retVal (castV var) else do
                val <- readRef var
                reduce env (Val val)
        _ -> retError ("Undeclared variable " ++ name) exp

reduce _ (Statements stmts) = do
    let (global, local) = partition isGlobalExp stmts
    reduceStatements (global ++ local) $ Val VUndef
    where
    isGlobalExp (Syn name _, _) = name `elem` (words "::=")
    isGlobalExp _ = False
    
-- Reduction for syntactic constructs
reduce env@Env{ envContext = cxt } exp@(Syn name exps) = case name of
    "block" -> do
        let [body] = exps
        enterBlock $ reduce env body
    "sub" -> do
        let [exp] = exps
        (VCode sub) <- enterEvalContext "Code" exp
        retVal $ VCode sub{ subPad = envLexical env }
    "if" -> doCond id 
    "unless" -> doCond not
    "for" -> do
        let [list, body] = exps
        vlist <- enterEvalContext "List" list
        vsub  <- enterEvalContext "Code" body
        vals  <- fromVal vlist
        VCode sub <- fromVal vsub
        let arity = length (subParams sub)
            runBody [] = retVal VUndef
            runBody vs = do
                let (these, rest) = arity `splitAt` vs
                doApply env sub [] $ map Val these
                runBody rest
        enterLoop $ runBody vals
    "loop" -> do
        let [pre, cond, post, body] = case exps of { [_] -> exps'; _ -> exps }
            exps' = [Syn "noop" [], Val (VBool True), Syn "noop" []] ++ exps
        evalExp pre
        enterLoop . fix $ \runBody -> do
            valBody <- evalExp body
            valPost <- evalExp post
            vBool   <- enterEvalContext "Bool" cond
            vb      <- fromVal vBool
            trapVal valBody $ do
                trapVal valPost $ do
                    if vb
                        then runBody
                        else retVal valBody
    "given" -> do
        let [topic, body] = exps
        vtopic <- fromVal =<< enterLValue (enterEvalContext "Scalar" topic)
        enterGiven vtopic $ enterEvalContext "Code" body
    "when" -> do
        let [match, body] = exps
        break  <- evalVar "&?BLOCK_EXIT"
        vbreak <- fromVal break
        match  <- reduce env match
        topic  <- evalVar "$_"
        result <- op2Match topic match
        rb     <- fromVal result
        let runBody = do
            enterEvalContext "Code" body
            doApply env vbreak [] []
        if rb
            then enterWhen (codeRef vbreak) runBody
            else retVal VUndef
    "default" -> do
        let [body] = exps
        break  <- evalVar "&?BLOCK_EXIT"
        vbreak <- fromVal break
        let runBody = do
            enterEvalContext "Code" body
            doApply env vbreak [] []
        enterWhen (codeRef vbreak) runBody
    "while" -> doWhileUntil id
    "until" -> doWhileUntil not
    "=" -> do
        let [lhs, rhs] = exps
        refVal  <- enterLValue $ evalExp lhs
        ref     <- fromVal refVal
        val     <- enterEvalContext (refType ref) rhs
        writeRef ref val
        retVal refVal
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
        vals    <- mapM (enterEvalContext "Any") exps
        -- now do some basic flattening
        vlists  <- (`mapM` vals) $ \v -> case v of
            VList _   -> fromVal v
--          VArray _  -> fromVal v
            _         -> return [v]
        retVal $ VList $ concat vlists
    "val" -> do
        let [exp] = exps
        local (\e -> e{ envLValue = False }) $ evalExp exp
    "cxt" -> do
        let [cxtExp, exp] = exps
        cxt     <- enterEvalContext "Str" cxtExp
        val     <- enterEvalContext (vCast cxt) exp
        enterEvalContext (vCast cxt) (Val val) -- force casting
    "\\[]" -> do
        let [exp] = exps
        v   <- enterEvalContext "List" exp
        av  <- newObject "Array" v
        sv  <- newObject "Scalar" (VRef av)
        retVal $ VRef sv
    -- XXX evil hack for infinite slices
    "[]" | [lhs, App "&postfix:..." invs args] <- exps
         , [idx] <- invs ++ args
         , not (envLValue env) -> reduce env (Syn "[...]" [lhs, idx])
    "[]" | [lhs, App "&infix:.." invs args] <- exps
         , [idx, Val (VNum n)] <- invs ++ args
         , n == 1/0
         , not (envLValue env) -> reduce env (Syn "[...]" [lhs, idx])
    "[]" -> do
        let [listExp, indexExp] = exps
        idxVal  <- enterEvalContext (cxtOfExp indexExp) indexExp
        varVal  <- enterLValue $ enterEvalContext "Array" listExp
        doFetch (mkFetch $ doArray varVal Array.fetchElem)
                (mkFetch $ doArray varVal Array.fetchVal)
                (fromVal idxVal)
                (envLValue env)
                (isaType (envClasses env) "Scalar" (cxtOfExp indexExp))
    "[...]" -> do
        let [listExp, indexExp] = exps
        idxVal  <- evalExp indexExp
        idx     <- fromVal idxVal
        listVal <- enterLValue $ enterEvalContext "Array" listExp
        list    <- fromVal listVal
        elms    <- mapM fromVal list -- flatten
        retVal $ VList (drop idx $ concat elms)
    "{}" -> do
        let [listExp, indexExp] = exps
        idxVal  <- enterEvalContext (cxtOfExp indexExp) indexExp
        varVal  <- enterLValue $ enterEvalContext "Hash" listExp
        doFetch (mkFetch $ doHash varVal Hash.fetchElem)
                (mkFetch $ doHash varVal Hash.fetchVal)
                (fromVal idxVal)
                (envLValue env)
                (isaType (envClasses env) "Scalar" (cxtOfExp indexExp))
    "()" -> do
        let [subExp, Syn "invs" invs, Syn "args" args] = exps
        vsub <- enterEvalContext "Code" subExp
        sub  <- fromVal vsub
        apply sub invs args
    "try" -> do
        val <- resetT $ evalExp (head exps)
        retEvalResult False val
    "gather" -> do
        val     <- enterEvalContext "List" exp
        -- ignore val
        retVal val
    "rx" -> do
        let [exp, Val (VBool g)] = exps
        val     <- enterEvalContext "Str" exp
        str     <- fromVal val
        retVal $ VRule $ MkRule
            { rxRegex  = mkRegex $ encodeUTF8 str
            , rxGlobal = g
            }
    "subst" -> do
        let [exp, g, subst] = exps
        (VRule rx)  <- reduce env (Syn "rx" [exp, g])
        retVal $ VSubst (rx, subst)
    "inline" -> retEmpty
    "module" -> retEmpty
    "noop" -> retEmpty
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
        vb        <- fromVal vbool
        if (f vb)
            then reduce env bodyIf
            else reduce env bodyElse
    -- XXX This treatment of while/until loops probably needs work
    doWhileUntil f = do
        let [cond, body] = exps
        enterLoop . fix $ \runBody -> do
            vbool <- enterEvalContext "Bool" cond
            vb    <- fromVal vbool
            case f vb of
                True -> do
                    rv <- reduce env body
                    case rv of
                        VError _ _  -> retVal rv
                        _           -> runBody
                _ -> retVal vbool

reduce env (App name [Syn "," invs] args) = reduce env (App name invs args)
reduce env (App name invs [Syn "," args]) = reduce env (App name invs args)

-- XXX absolutely evil bloody hack for "goto"
reduce _ (App "&goto" (subExp:invs) args) = do
    vsub <- enterEvalContext "Code" subExp
    sub <- fromVal vsub
    local callerEnv $ do
        val <- apply sub invs args
        shiftT $ const (retVal val)
    where
    callerEnv env = let caller = maybe env id (envCaller env) in
        env{ envCaller = envCaller caller
           , envContext = envContext caller
           , envLValue = envLValue caller
           }

-- XXX absolutely evil bloody hack for "assuming"
reduce _ (App "&assuming" (subExp:invs) args) = do
    vsub <- enterEvalContext "Code" subExp
    sub <- fromVal vsub
    case bindSomeParams sub invs args of
        Left errMsg      -> retError errMsg (Val VUndef)
        Right curriedSub -> retVal $ castV $ curriedSub
        

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
        let bound = either (const False) (const True) $ bindParams sub invs args
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
    enterLex formal $ evalExp body
    where
    formal = filter (not . null . symName) $ map argNameValue bound
    argNameValue (ApplyArg name val _) = SymVar SMy name (scalarRef val)

apply :: VCode -> [Exp] -> [Exp] -> Eval Val
apply sub invs args = do
    env <- ask
    doApply env sub invs args

-- XXX - faking application of lexical contexts
-- XXX - what about defaulting that depends on a junction?
doApply :: Env -> VCode -> [Exp] -> [Exp] -> Eval Val
doApply Env{ envClasses = cls } sub@Sub{ subFun = fun, subType = typ } invs args =
    case bindParams sub invs args of
        Left errMsg     -> retError errMsg (Val VUndef)
        Right sub  -> do
            enterScope $ do
                (pad, bound) <- doBind [] (subBindings sub)
                -- trace (show bound) $ return ()
                val <- local fixEnv $ enterLex pad $ do
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
    doBind :: Pad -> [(Param, Exp)] -> Eval (Pad, [ApplyArg])
    doBind pad [] = return (pad, [])
    doBind pad ((prm, exp):rest) = do
        -- trace ("<== " ++ (show (prm, exp))) $ return ()
        let name = paramName prm
            cxt = cxtOfSigil $ head name
        (val, coll) <- enterContext cxt $ case exp of
            Parens exp  -> local fixEnv $ enterLex pad $ expToVal prm exp
            _           -> expToVal prm exp
        -- trace ("==> " ++ (show val)) $ return ()
        let sym = SymVar SMy name (scalarRef val)
        (pad', restArgs) <- doBind (sym:pad) rest
        return (pad', ApplyArg name val coll:restArgs)
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

doFetch :: (Val -> Eval (IVar VScalar))
            -> (Val -> Eval Val)
            -> (forall v. (Value v) => Eval v)
            -> Bool -> Bool
            -> Eval Val
doFetch fetchElem fetchVal fetchIdx isLV isSV = case (isLV, isSV) of
    (True, True) -> do
        -- LValue, Scalar context
        idx <- fetchIdx
        elm <- fetchElem idx
        retIVar elm
    (True, False) -> do
        -- LValue, List context
        idxList <- fetchIdx
        elms    <- mapM fetchElem idxList
        retIVar $ IArray elms
    (False, True) -> do
        -- RValue, Scalar context
        idx <- fetchIdx
        fetchVal idx
    (False, False) -> do
        -- RValue, List context
        idxList <- fetchIdx
        return . VList =<< mapM fetchVal idxList

mkFetch :: (Value n) => Eval (n -> Eval t) -> Val -> Eval t
mkFetch f v = do
    f' <- f
    v' <- fromVal v
    f' v'

