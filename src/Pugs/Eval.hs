{-# OPTIONS_GHC -fglasgow-exts #-}
{-# OPTIONS_GHC -#include "UnicodeC.h" #-}

{-
    Evaluation and reduction engine.

    Home is behind, the world ahead,
    And there are many paths to tread
    Through shadows to the edge of night,
    Until the stars are all alight.
    Then world behind and home ahead,
    We'll wander back to home and bed...
-}

module Pugs.Eval where
import Pugs.Internals
import Prelude hiding ( exp )
import qualified Data.Map as Map

import Pugs.AST
import Pugs.Junc
import Pugs.Bind
import Pugs.Prim
import Pugs.Context
import Pugs.Monads
import Pugs.Pretty
import Pugs.Types
import qualified Pugs.Types.Hash as Hash
import qualified Pugs.Types.Array as Array

emptyEnv :: (MonadIO m) => [IO (Pad -> Pad)] -> m Env
emptyEnv genPad = do
    pad  <- liftIO $ sequence genPad
    ref  <- liftIO $ newIORef Map.empty
    uniq <- liftIO $ newUnique
    syms <- liftIO $ initSyms
    glob <- liftIO $ newIORef (combine (pad ++ syms) $ mkPad [])
    return $ Env
        { envContext = CxtVoid
        , envLexical = mkPad []
        , envLValue  = False
        , envGlobal  = glob
        , envClasses = initTree
        , envEval    = evaluate
        , envCaller  = Nothing
        , envDepth   = 0
        , envID      = uniq
        , envBody    = Val undef
        , envDebug   = Just ref -- Set to "Nothing" to disable debugging
        , envStash   = ""
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
            putStrLn ("***" ++ val ++ str ++ pretty a)

evaluateMain :: Exp -> Eval Val
evaluateMain exp = do
    val     <- resetT $ evaluate exp
    endAV   <- evalVar "@*END"
    subs    <- fromVals endAV
    enterContext CxtVoid $ do
        mapM_ evalExp [ Syn "()" [Val sub, Syn "invs" [], Syn "args" []]
                      | sub <- subs
                      ]
    return val

evaluate :: Exp -> Eval Val
evaluate (Val val) = evalVal val
evaluate exp = do
    want <- asks envWant
    debug "indent" (' ':) ("Evl [" ++ want ++ "]:\n") exp
    val <- local (\e -> e{ envBody = exp }) $ do
        reduceExp exp
    debug "indent" (tail) " Ret: " val
    trapVal val (return val)


findSyms :: Var -> Eval [(String, Val)]
findSyms name = do
    lex  <- asks envLexical
    glob <- askGlobal
    let names = [name, toGlobal name]
    syms <- forM [lex, glob] $ \pad -> do
        forM names $ \name' -> do
            case lookupPad name' pad of
                Just ioRefs -> do
                    refs  <- liftIO $ mapM readIORef ioRefs
                    forM refs $ \ref -> do
                        val <- readRef ref
                        return (name', val)
                Nothing -> return []
    return $ concat (concat syms)

enterEvalContext :: Cxt -> Exp -> Eval Val
enterEvalContext cxt = enterContext cxt . evalExp

-- Reduction ---------------------------------------------------------------

reduceExp :: Exp -> Eval Val
reduceExp exp = do
    env <- ask
    reduce env exp

retVal :: Val -> Eval Val
retVal val = evaluate (Val val)  -- casting

addGlobalSym newSym = do
    glob <- asks envGlobal
    liftIO $ do
        syms <- readIORef glob
        writeIORef glob (newSym syms)

-- XXX This is a mess. my() and our() etc should not be statement level!
reduceStatements :: [(Exp, SourcePos)] -> Exp -> Eval Val
reduceStatements [] = reduceExp
reduceStatements ((exp, pos):rest) = case exp of
    Noop -> reduceStatements rest

    Syn ";" exps -> do
        reduceStatements $ (exps `zip` repeat pos) ++ rest

    Sym scope name -> const $ do
        ref <- newObject (typeOfSigil $ head name)
        let doRest = reduceStatements rest (Var name)
        sym <- genMultiSym name ref
        case scope of
            SMy -> enterLex [ sym ] doRest
            _   -> do { addGlobalSym sym; doRest }

    Syn "sub" [Val (VCode sub)] | subType sub >= SubBlock -> do
        -- bare Block in statement level; run it!
        let app = Syn "()" [exp, Syn "invs" [], Syn "args" []]
        reduceStatements $ (app, pos):rest
    Pad _ lex' -> \e -> do
        let doRest = reduceStatements rest e
        lex <- asks envLexical
        local (\e -> e{ envLexical = lex' `unionPads` lex }) doRest
    Syn "env" [] | null rest -> \e -> do
        env <- ask
        val <- case e of
            Val v   -> return v
            _       -> evalExp e
        writeVar "$*_" val
        return . VControl $ ControlEnv env
    Syn "dump" [] | null rest -> \e -> do
        Env{ envGlobal = globals, envLexical = lexicals } <- ask
        liftIO $ modifyIORef globals (unionPads lexicals)
        reduceStatements rest e
    _ | null rest -> const $ do
        _   <- asks envContext
        pad <- sequence $ posSyms pos
        val <- enterLex pad $ reduceExp exp
        retVal val
    _ -> const $ do
        val <- enterContext cxtVoid $ do
            pad <- sequence $ posSyms pos
            enterLex pad $ do
                reduceExp exp
        trapVal val $ do
            reduceStatements rest $ Val val

trapVal :: Val -> Eval a -> Eval a
trapVal val action = case val of
    VError str exp  -> retError str exp
    VControl c      -> retControl c
    _               -> action

posSyms pos = [ genSym n v | (n, v) <- syms ]
    where
    file = sourceName pos
    line = show $ sourceLine pos
    col  = show $ sourceColumn pos
    syms =
        [ ("$?FILE", scalarRef $ castV file)
        , ("$?LINE", scalarRef $ castV line)
        , ("$?COLUMN", scalarRef $ castV col)
        , ("$?POSITION", scalarRef $ castV $ pretty pos)
        ]

evalVar :: Ident -> Eval Val
evalVar name = do
    env <- ask
    v <- findVar env name
    case v of
        Just var -> readRef var
        Nothing  -> retError ("Undeclared variable " ++ name) (Val undef)

enterLValue = local (\e -> e{ envLValue = True })
enterRValue = local (\e -> e{ envLValue = False })

findVar :: Env -> Ident -> Eval (Maybe VRef)
findVar env name = do
    rv <- findVarRef env name
    case rv of
        Nothing  -> return Nothing
        Just ref -> fmap Just $ liftIO (readIORef ref)

findVarRef :: Env -> Ident -> Eval (Maybe (IORef VRef))
findVarRef env name
    | Just (package, name') <- breakOnGlue "::" name
    , Just (sig, "") <- breakOnGlue "CALLER" package =
        case (envCaller env) of
            Just caller -> findVarRef caller (sig ++ name')
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
    if isNothing v then retError ("Undeclared variable " ++ name) exp else do
    let ref = fromJust v
    if refType ref == (mkType "Thunk") then forceRef ref else do
    val <- callCC $ \esc -> do
        let cxt = envContext env
            typ = typeOfCxt cxt
            isCollectionRef = (refType ref /= mkType "Scalar")
        -- If RValue, read from the reference
        unless (envLValue env) $ do
            when (isCollectionRef && isItemCxt cxt) $ do
                -- auto-enreference
                esc $ VRef ref
            esc =<< readRef ref
        -- LValue here
        when isCollectionRef $ esc (castV ref)
        val <- readRef ref
        let isAutovivify = and $
                [ not (defined val)
                , isItemCxt cxt
                , (typ ==) `any` [mkType "Array", mkType "Hash"]
                ]
        when isAutovivify $ do
            ref' <- newObject typ
            writeRef ref (VRef ref')
        return $ castV ref
    retVal val

reduce _ (Stmts stmts) = do
    let (global, local) = partition isGlobalExp stmts
    reduceStatements (global ++ local) $ Val undef
    where
    isGlobalExp (Syn name _, _) = name `elem` (words "::=")
    isGlobalExp _ = False

-- Context forcing
reduce _ (Cxt cxt exp) = do
    val <- enterEvalContext cxt exp
    enterEvalContext cxt (Val val) -- force casting

-- Reduction for no-operations
reduce _ Noop = retEmpty

-- Reduction for syntactic constructs
reduce env exp@(Syn name exps) = case name of
    "block" -> do
        let [body] = exps
        enterBlock $ reduce env body
    "sub" -> do
        let [exp] = exps
        (VCode sub) <- enterEvalContext (cxtItem "Code") exp
        retVal $ VCode sub{ subPad = envLexical env }
    "if" -> doCond id 
    "unless" -> doCond not
    "for" -> do
        let [list, body] = exps
        vlist <- enterEvalContext cxtSlurpyAny list
        vsub  <- enterEvalContext (cxtItem "Code") body
        vals  <- fromVal vlist
        VCode sub <- fromVal vsub
        let arity = length (subParams sub)
            runBody [] = retVal undef
            runBody vs = do
                let (these, rest) = arity `splitAt` vs
                doApply env sub [] $ map Val these
                runBody rest
        enterLoop $ runBody vals
    "loop" -> do
        let [pre, cond, post, body] = case exps of { [_] -> exps'; _ -> exps }
            exps' = [emptyExp, Val (VBool True), emptyExp] ++ exps
        evalExp pre
        enterLoop . fix $ \runBody -> do
            valBody <- evalExp body
            valPost <- evalExp post
            vBool   <- enterEvalContext (cxtItem "Bool") cond
            vb      <- fromVal vBool
            trapVal valBody $ do
                trapVal valPost $ do
                    if vb
                        then runBody
                        else retVal valBody
    "given" -> do
        let [topic, body] = exps
        vtopic <- fromVal =<< enterLValue (enterEvalContext cxtItemAny topic)
        enterGiven vtopic $ enterEvalContext (cxtItem "Code") body
    "when" -> do
        let [match, body] = exps
        break  <- evalVar "&?BLOCK_EXIT"
        vbreak <- fromVal break
        match  <- reduce env match
        topic  <- evalVar "$_"
        result <- op2Match topic match
        rb     <- fromVal result
        if rb
            then enterWhen (subBody vbreak) $ do
                doApply env vbreak [body] []
            else retVal undef
    "default" -> do
        let [body] = exps
        break  <- evalVar "&?BLOCK_EXIT"
        vbreak <- fromVal break
        enterWhen (subBody vbreak) $ do
            doApply env vbreak [body] []
    "while" -> doWhileUntil id
    "until" -> doWhileUntil not
    "=" -> do
        let [lhs, rhs] = exps
        refVal  <- enterLValue $ evalExp lhs
        ref     <- fromVal refVal
        cls     <- asks envClasses
        let typ = refType ref
            cxt | isaType cls "List" typ = cxtSlurpyAny
                | otherwise = cxtItem $ takeWhile (/= ':') . show $ refType ref
        val <- enterRValue $ enterEvalContext cxt rhs
        writeRef ref val
        if envLValue env 
            then retVal refVal
            else retVal val
    "::=" -> reduce env (Syn ":=" exps)
    ":=" | [Syn "," vars, Syn "," vexps] <- exps -> do
        when (length vars > length vexps) $ do
            (`retError` exp) $ "Wrong number of binding parameters: "
                ++ (show $ length vexps) ++ " actual, "
                ++ (show $ length vars) ++ " expected"
        -- env' <- cloneEnv env -- FULL THUNKING
        names <- forM vars $ \var -> case var of
            Var name -> return name
            _        -> retError "Cannot bind this as lhs" var
        bindings <- forM (names `zip` vexps) $ \(name, vexp) -> do
            {- FULL THUNKING
            let ref = thunkRef . MkThunk $ do
                    local (const env'{ envLValue = True }) $ do
                        enterEvalContext (cxtOfSigil $ head name) vexp
            -}
            val  <- enterLValue $ enterEvalContext (cxtOfSigil $ head name) vexp
            ref  <- fromVal val
            rv   <- findVarRef env name
            case rv of
                Just ioRef -> return (ioRef, ref)
                Nothing -> do
                    retError ("Undeclared variable " ++ name) (Val undef)
        forM_ bindings $ \(ioRef, ref) -> do
            liftIO $ writeIORef ioRef ref
        return $ case map (VRef . snd) bindings of
            [v] -> v
            vs  -> VList vs
    ":=" -> do
        let [var, vexp] = exps
            expand e@(Syn "," _) = e
            expand e = Syn "," [e]
        reduce env (Syn ":=" [expand var, expand vexp])
    "=>" -> do
        let [keyExp, valExp] = exps
        key     <- enterEvalContext cxtItemAny keyExp
        val     <- evalExp valExp
        retVal $ castV (key, val)
    "*" | [Syn syn [exp]] <- exps -- * cancels out [] and {}
        , syn == "\\{}" || syn == "\\[]"
        -> enterEvalContext cxtSlurpyAny exp
    "*" -> do -- first stab at an implementation
        let [exp] = exps
        val     <- enterRValue $ enterEvalContext cxtSlurpyAny exp
        vals    <- fromVals val
        retVal $ VList $ concat vals
    "," -> do
        vals <- mapM evalExp exps
        -- retVal . VList . concat $ map vCast vals
        -- in slurpy context, flatten each arguments
        ifListContext
            (retVal . VList . concat $ map vCast vals)
            (retVal $ VList vals)
    "val" -> do
        let [exp] = exps
        enterRValue $ evalExp exp
    "\\{}" -> do
        let [exp] = exps
        v   <- enterRValue $ enterEvalContext cxtItemAny exp
        hv  <- newObject (MkType "Hash")
        writeRef hv v
        retVal $ VRef hv
    "\\[]" -> do
        let [exp] = exps
        v   <- enterRValue $ enterEvalContext cxtSlurpyAny exp
        av  <- newObject (MkType "Array")
        writeRef av v
        retVal $ VRef av
    -- XXX evil hack for infinite slices
    "[]" | [lhs, App "&postfix:..." invs args] <- exps
         , [idx] <- invs ++ args
         , not (envLValue env)
         -> reduce env (Syn "[...]" [lhs, idx])
    "[]" | [lhs, App "&infix:.." invs args] <- exps
         , [idx, Val (VNum n)] <- invs ++ args
         , n == 1/0
         , not (envLValue env)
         -> reduce env (Syn "[...]" [lhs, idx])
    "[]" -> do
        let [listExp, indexExp] = exps
        idxCxt  <- if envLValue env
            then cxtOfExp indexExp else return (envContext env)
        idxVal  <- enterRValue $ enterEvalContext idxCxt indexExp
        varVal  <- enterLValue $ enterEvalContext (cxtItem "Array") listExp
        doFetch (mkFetch $ doArray varVal Array.fetchElem)
                (mkFetch $ doArray varVal Array.fetchVal)
                (fromVal idxVal)
                (envLValue env)
                (not (isSlurpyCxt idxCxt))
    "[...]" -> do
        let [listExp, indexExp] = exps
        idxVal  <- enterRValue $ enterEvalContext (cxtItem "Int") indexExp
        idx     <- fromVal idxVal
        listVal <- enterLValue $ enterEvalContext (cxtItem "Array") listExp
        list    <- fromVal listVal
        elms    <- mapM fromVal list -- flatten
        retVal $ VList (drop idx $ concat elms)
    "{}" -> do
        let [listExp, indexExp] = exps
        idxCxt  <- if envLValue env
            then cxtOfExp indexExp else return (envContext env)
        idxVal  <- enterRValue $ enterEvalContext idxCxt indexExp
        varVal  <- enterLValue $ enterEvalContext (cxtItem "Hash") listExp
        doFetch (mkFetch $ doHash varVal Hash.fetchElem)
                (mkFetch $ doHash varVal Hash.fetchVal)
                (fromVal idxVal)
                (envLValue env)
                (not (isSlurpyCxt idxCxt))
    "()" -> do
        let [subExp, Syn "invs" invs, Syn "args" args] = exps
        vsub <- enterEvalContext (cxtItem "Code") subExp
        (`juncApply` [ApplyArg "" vsub False]) $ \[arg] -> do
            sub  <- fromVal $ argValue arg
            apply sub invs args
    "try" -> do
        val <- resetT $ evalExp (head exps)
        retEvalResult False val
    "gather" -> do
        val     <- enterEvalContext cxtSlurpyAny exp
        -- ignore val
        retVal val
    "rx" -> do
        let [exp, adverbs] = exps
        hv      <- fromVal =<< evalExp adverbs
        val     <- enterEvalContext (cxtItem "Str") exp
        str     <- fromVal val
        p5      <- fromAdverb hv ["P5", "Perl5", "perl5"]
        p5flags <- fromAdverb hv ["P5", "Perl5", "perl5"]
        flag_g  <- fromAdverb hv ["g", "global"]
        flag_i  <- fromAdverb hv ["i", "ignorecase"]
        when (not p5) $ do
            retError "Perl 6 rules is not implemented yet, use :P5" (Val val)
        retVal $ VRule $ MkRule
            { rxRegex  = mkRegexWithPCRE (encodeUTF8 str) $
                [ pcreUtf8
                , ('i' `elem` p5flags || flag_i) `implies` pcreCaseless
                , ('m' `elem` p5flags) `implies` pcreMultiline
                , ('s' `elem` p5flags) `implies` pcreDotall
                , ('x' `elem` p5flags) `implies` pcreExtended
                ] 
            , rxGlobal = ('g' `elem` p5flags || flag_g)
            }
        where
        implies True  = id
        implies False = const 0
        fromAdverb _ [] = fromVal undef
        fromAdverb hv (k:ks) = case lookup k hv of
            Just v  -> fromVal v
            Nothing -> fromAdverb hv ks
    "subst" -> do
        let [exp, subst, adverbs] = exps
        (VRule rx)  <- reduce env (Syn "rx" [exp, adverbs])
        retVal $ VSubst (rx, subst)
    "is" -> do
        retEmpty
    "module" -> do
        let [exp] = exps
        val <- evalExp exp
        writeVar "$?MODULE" val
        retEmpty
    "inline" -> do
        let [langExp, _] = exps
        langVal <- evalExp langExp
        lang    <- fromVal langVal
        when (lang /= "Haskell") $
            retError "Inline: Unknown language" (Val langVal)
        modVal  <- readVar "$?MODULE"
        mod     <- fromVal modVal
        let file = (`concatMap` mod) $ \v -> case v of
            { '-' -> "__"; _ | isAlphaNum v -> [v] ; _ -> "_" }
        op1 "require_haskell" (VStr $ file ++ ".o")
        retEmpty
    syn | last syn == '=' -> do
        let [lhs, exp] = exps
            op = "&infix:" ++ init syn
        evalExp $ Syn "=" [lhs, App op [lhs, exp] []]
    _ -> retError "Unknown syntactic construct" exp
    where
    doCond f = do
        let [cond, bodyIf, bodyElse] = exps
        vbool     <- enterEvalContext (cxtItem "Bool") cond
        vb        <- fromVal vbool
        if (f vb)
            then reduce env bodyIf
            else reduce env bodyElse
    -- XXX This treatment of while/until loops probably needs work
    doWhileUntil f = do
        let [cond, body] = exps
        enterLoop . fix $ \runBody -> do
            vbool <- enterEvalContext (cxtItem "Bool") cond
            vb    <- fromVal vbool
            case f vb of
                True -> do
                    rv <- reduce env body
                    case rv of
                        VError _ _  -> retVal rv
                        _           -> runBody
                _ -> retVal vbool

{-
--- XXX ALL WRONG
reduce env (App name invs args)
    | isPrefix
    , not . null $ [ undefined | (Syn "," _) <- invs ]
    = reduce env $ App name (concatMap flatten invs) args
    | isPrefix
    , not . null $ [ undefined | (Syn "," _) <- args ]
    = reduce env $ App name invs (concatMap flatten args)
    where
    flatten (Syn "," exps) = exps
    flatten exp = [exp]
    fixity = takeWhile (/= ':') name
    isPrefix
        | fixity == name        = True
        | fixity == "&infix"    = False
        | fixity == "&postfix"  = False
        | otherwise             = True
-}

-- XXX absolutely evil bloody hack for context hinters
reduce _ (App "&hash" invs args) =
    enterEvalContext cxtItemAny $ Syn "\\{}" [Syn "," $ invs ++ args]

reduce _ (App "&list" invs args) =
    enterEvalContext cxtSlurpyAny $ case invs ++ args of
        []    -> Val (VList [])
        [exp] -> exp
        exps  -> Syn "," exps

reduce _ (App "&scalar" invs args)
    | [exp] <- invs ++ args = enterEvalContext cxtItemAny exp
    | otherwise = enterEvalContext cxtItemAny $ Syn "," (invs ++ args)

-- XXX absolutely evil bloody hack for "zip"
reduce _ (App "&zip" invs args) = do
    vals <- mapM (enterRValue . enterEvalContext (cxtItem "Array")) (invs ++ args)
    val  <- op0 "Y" vals
    retVal val

-- XXX absolutely evil bloody hack for "goto"
reduce _ (App "&goto" (subExp:invs) args) = do
    vsub <- enterEvalContext (cxtItem "Code") subExp
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
    vsub <- enterEvalContext (cxtItem "Code") subExp
    sub <- fromVal vsub
    case bindSomeParams sub invs args of
        Left errMsg      -> retError errMsg (Val undef)
        Right curriedSub -> retVal $ castV $ curriedSub

reduce _ (App "&infix:=>" [keyExp, valExp] []) = do
    key <- enterEvalContext cxtItemAny keyExp
    val <- enterEvalContext cxtItemAny valExp
    retVal $ castV (key, val)

reduce Env{ envClasses = cls, envContext = cxt } exp@(App name invs args) = do
    subSyms <- findSyms name
    lens <- mapM argSlurpLen (invs ++ args)
    sub <- findSub (sum lens) subSyms
    case sub of
        Just sub    -> applySub subSyms sub invs args
        Nothing     -> retError ("No compatible subroutine found: " ++ name) exp
    where
    argSlurpLen (Val listMVal) = do
        listVal  <- fromVal listMVal
        return $ length (vCast listVal :: [Val])
    argSlurpLen (Var name) = do
        listMVal <- evalVar name
        listVal  <- fromVal listMVal
        return $ length (vCast listVal :: [Val])
    argSlurpLen (Syn "," list) =  return $ length list
    argSlurpLen _ = return 1 -- XXX
    applySub subSyms sub invs args
        -- list-associativity
        | MkCode{ subAssoc = "list" }      <- sub
        , (App name' invs' []):rest  <- invs
        , name == name'
        = applySub subSyms sub (invs' ++ rest)  []
        -- fix subParams to agree with number of actual arguments
        | MkCode{ subAssoc = "list", subParams = (p:_) }   <- sub
        , null args
        = apply sub{ subParams = (length invs) `replicate` p } invs []
        -- chain-associativity
        | MkCode{ subAssoc = "chain" }   <- sub
        , (App _ _ []):_              <- invs
        , null args
        = mungeChainSub sub invs
        | MkCode{ subAssoc = "chain", subParams = (p:_) }   <- sub
        = apply sub{ subParams = (length invs) `replicate` p } invs []
        -- normal application
        | otherwise
        = apply sub invs args
    mungeChainSub sub invs = do
        let MkCode{ subAssoc = "chain", subParams = (p:_) } = sub
            (App name' invs' args'):rest = invs
        subSyms' <- findSyms name'
        lens'    <- mapM argSlurpLen (invs' ++ args')
        theSub <- findSub (sum lens') subSyms'
        case theSub of
            Just sub'    -> applyChainSub subSyms' sub invs sub' invs' args' rest
            Nothing      -> apply sub{ subParams = (length invs) `replicate` p } invs [] -- XXX Wrong
            -- retError ("No compatible subroutine found: " ++ name') exp
    applyChainSub subSyms sub invs sub' invs' args' rest
        | MkCode{ subAssoc = "chain", subBody = fun, subParams = prm }   <- sub
        , MkCode{ subAssoc = "chain", subBody = fun', subParams = prm' } <- sub'
        , null args'
        = applySub subSyms sub{ subParams = prm ++ tail prm', subBody = Prim $ chainFun prm' fun' prm fun } (invs' ++ rest) []
        | MkCode{ subAssoc = "chain", subParams = (p:_) }   <- sub
        = apply sub{ subParams = (length invs) `replicate` p } invs [] -- XXX Wrong
        | otherwise
        = internalError "applyChainsub did not match a chain subroutine"
    findSub slurpLen subSyms = do
        subs' <- subs slurpLen subSyms
        -- let foo (x, sub) = show x ++ show (map paramContext $ subParams sub)
        -- trace (unlines $ map foo $ sort subs') return ()
        return $ case sort subs' of
            ((_, sub):_)    -> Just sub
            _               -> Nothing
    subs slurpLen subSyms = (liftM catMaybes) $ (`mapM` subSyms) $ \(n, val) -> do
        sub@(MkCode{ subType = subT, subReturns = ret, subParams = prms }) <- fromVal val
        let isGlobal = '*' `elem` n
        let fun = arityMatch sub (length (invs ++ args)) slurpLen
        if isNothing fun then return Nothing else do
        -- if deltaFromCxt ret == 0 then return Nothing else do
        let invocants = filter isInvocant prms
        let prms' = if null invocants then prms else invocants
        let distance = (deltaFromCxt ret : map (deltaFromScalar . typeOfCxt . paramContext) prms')
        let bound = either (const False) (const True) $ bindParams sub invs args
        return $ Just
            ( (isGlobal, subT, isMulti sub, bound, distance)
            , fromJust fun
            )
    deltaFromCxt x          = deltaType cls (typeOfCxt cxt) x
    deltaFromScalar x       = deltaType cls x (mkType "Scalar")

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
    pad <- formal
    enterLex pad $ evalExp body
    where
    formal = mapM argNameValue $ filter (not . null . argName) bound
    argNameValue (ApplyArg name val _) = genSym name (vCast val)

apply :: VCode -> [Exp] -> [Exp] -> Eval Val
apply sub invs args = do
    env <- ask
    doApply env sub invs args

-- XXX - faking application of lexical contexts
-- XXX - what about defaulting that depends on a junction?
doApply :: Env -> VCode -> [Exp] -> [Exp] -> Eval Val
doApply Env{ envClasses = cls } sub@MkCode{ subBody = fun, subType = typ } invs args =
    case bindParams sub invs args of
        Left errMsg -> retError errMsg (Val undef)
        Right sub   -> do
            forM_ (subSlurpLimit sub) $ \limit@(n, _) -> do
                extra <- checkSlurpyLimit limit
                when (not $ null extra) $ do
                    (`retError` (Val undef)) $
                        "Too many slurpy arguments for " ++ subName sub ++ ": "
                        ++ show ((genericLength (take 1000 extra)) + n) ++ " actual, "
                        ++ show n ++ " expected"
            enterScope $ do
                (syms, bound) <- doBind [] (subBindings sub)
                -- trace (show bound) $ return ()
                val <- local fixEnv $ enterLex syms $ do
                    (`juncApply` bound) $ \realBound -> do
                        enterSub sub $ do
                            applyExp realBound fun
                retVal val
    where
    enterScope :: Eval Val -> Eval Val
    enterScope
        | typ >= SubBlock = id
        | otherwise       = resetT
    fixEnv env
        | typ >= SubBlock = env
        | otherwise       = env{ envCaller = Just env }
    doBind :: [Pad -> Pad] -> [(Param, Exp)] -> Eval ([Pad -> Pad], [ApplyArg])
    doBind syms [] = return (syms, [])
    doBind syms ((prm, exp):rest) = do
        -- trace ("<== " ++ (show (prm, exp))) $ return ()
        let name = paramName prm
            cxt = cxtOfSigil $ head name
        (val, coll) <- enterContext cxt $ case exp of
            Parens exp  -> local fixEnv $ enterLex syms $ expToVal prm exp
            _           -> expToVal prm exp
        -- trace ("==> " ++ (show val)) $ return ()
        boundRef <- fromVal val
        newSym   <- genSym name boundRef
        (syms', restArgs) <- doBind (newSym:syms) rest
        return (syms', ApplyArg name val coll:restArgs)
    expToVal MkParam{ isThunk = thunk, isLValue = lv, paramContext = cxt, paramName = name, isWritable = rw } exp = do
        env <- ask -- freeze environment at this point for thunks
        let eval = local (const env{ envLValue = lv }) $ do
            enterEvalContext (cxtOfSigil $ head name) exp
        val <- if thunk then return (VRef . thunkRef $ MkThunk eval) else do
            v   <- eval
            typ <- evalValType v
            if isaType cls "Junction" typ then return v else do
            case (lv, rw) of
                (True, True)    -> return v
                (True, False)   -> return (VRef $ scalarRef v) 
                (False, False)  -> return v -- XXX reduce to val?
                (False, True)   -> do
                    -- make a copy
                    ref <- newObject (typeOfSigil $ head name)
                    writeRef ref v
                    return (VRef ref)
        return (val, (isSlurpyCxt cxt || isCollapsed (typeOfCxt cxt)))
    checkSlurpyLimit (n, exp) = do
        listVal <- enterLValue $ enterEvalContext (cxtItem "Array") exp
        list    <- fromVal listVal
        elms    <- mapM fromVal list -- flatten
        return $ genericDrop n (concat elms :: [Val])
    isCollapsed typ
        | isaType cls "Bool" typ        = True
        | isaType cls "Junction" typ    = True
        | otherwise                     = False

toGlobal name
    | (sigil, identifier) <- break (\x -> isAlpha x || x == '_') name
    , last sigil /= '*'
    = sigil ++ ('*':identifier)
    | otherwise = name


arityMatch sub@MkCode{ subAssoc = assoc, subParams = prms } argLen argSlurpLen
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
        fmap VList $ mapM fetchVal idxList

mkFetch :: (Value n) => Eval (n -> Eval t) -> Val -> Eval t
mkFetch f v = do
    f' <- f
    v' <- fromVal v
    f' v'

