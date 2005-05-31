{-# OPTIONS_GHC -fglasgow-exts -cpp #-}
{-# OPTIONS_GHC -#include "UnicodeC.h" #-}

{-|
    Evaluation and reduction engine.

>   Home is behind, the world ahead,
>   And there are many paths to tread
>   Through shadows to the edge of night,
>   Until the stars are all alight.
>   Then world behind and home ahead,
>   We'll wander back to home and bed...

This module takes an Abstract Syntaxt Tree and recursively evaluates it,
thereby evaluating the program.

The AST is represented as a hierarchy of nested 'Exp's (see "Pugs.AST").
Some understanding of 'Exp' and "Pugs.AST" in general will be necessary
before this module can be properly understood.

Functions of notable interest:

* 'evaluate' and 'reduce' (the guts of the whole evaluation\/reduction engine)

* 'apply' and 'doApply' (the guts of function application)
-}

module Pugs.Eval (
    evaluate,
    emptyEnv, evaluateMain,
    enterLValue, enterRValue,
) where
import Pugs.Config
import Pugs.Internals
import Prelude hiding ( exp )
import qualified Data.Map as Map

import Pugs.AST
import Pugs.Junc
import Pugs.Bind
import Pugs.Prim
import Pugs.Prim.List (op0Zip)
import Pugs.Context
import Pugs.Monads
import Pugs.Pretty
import Pugs.Types
import Pugs.Prim.List (op2Fold)
import Pugs.External
import Pugs.Embed.Perl5

{-|
Construct a new, initially empty 'Env' (evaluation environment).

Used in 'Main.doParse', 'Main.doParseWith' and 'Pugs.Run.prepareEnv'.
Of these, only 'Pugs.Run.prepareEnv' seems to make use of the second
argument.  See 'Pugs.Prims.initSyms'
-}
emptyEnv :: (MonadIO m, MonadSTM m) 
         => String             -- ^ Name associated with the environment
         -> [STM (Pad -> Pad)] -- ^ List of 'Pad'-mutating transactions used
                               --     to declare an initial set of global vars
         -> m Env
emptyEnv name genPad = do
    uniq <- liftIO newUnique
    liftSTM $ do
        pad  <- sequence genPad
        ref  <- newTVar Map.empty
        syms <- initSyms
        glob <- newTVar (combine (pad ++ syms) $ mkPad [])
        return $ MkEnv
            { envContext = CxtVoid
            , envLexical = mkPad []
            , envLValue  = False
            , envGlobal  = glob
            , envPackage = "main"
            , envClasses = initTree
            , envEval    = evaluate
            , envCaller  = Nothing
            , envOuter   = Nothing
            , envDepth   = 0
            , envID      = uniq
            , envBody    = Val undef
            , envDebug   = Just ref -- Set to "Nothing" to disable debugging
            , envStash   = ""
            , envPos     = MkPos name 1 1 1 1
            }

-- Evaluation ---------------------------------------------------------------

debug :: Pretty a => String -> (String -> String) -> String -> a -> Eval ()
debug key fun str a = do
    rv <- asks envDebug
    case rv of
        Nothing -> return ()
        Just ref -> do
            val <- liftSTM $ do
                fm <- readTVar ref
                let val = fun $ Map.findWithDefault "" key fm
                writeTVar ref (Map.insert key val fm)
                return val
            when (length val > 100) $ do
                trace "*** Warning: deep recursion" return ()
            trace ("***" ++ val ++ str ++ pretty a) return ()

evaluateMain :: Exp -> Eval Val
evaluateMain exp = do
    -- S04: INIT {...}*      at run time, ASAP
    initAV   <- evalVar "@?INIT"
    initSubs <- fromVals initAV
    enterContext CxtVoid $ do
        mapM_ evalExp [ App (Val sub) Nothing [] | sub <- initSubs ]
    -- The main runtime
    val      <- resetT $ evaluate exp
    -- S04: END {...}       at run time, ALAP
    endAV    <- evalVar "@*END"
    endSubs  <- fromVals endAV
    enterContext CxtVoid $ do
        mapM_ evalExp [ App (Val sub) Nothing [] | sub <- endSubs ]
    liftIO $ performGC
    return val

-- | Evaluate an expression. This function mostly just delegates to 'reduce'.
evaluate :: Exp -- ^ The expression to evaluate
         -> Eval Val
evaluate (Val val) = evalVal val
evaluate exp = do
    want <- asks envWant
    debug "indent" ('-':) (" Evl [" ++ want ++ "]:\n") exp
    val <- local (\e -> e{ envBody = exp }) $ reduce exp
    debug "indent" (tail) "- Ret: " val
    trapVal val (return val)

findSyms :: Var -> Eval [(String, Val)]
findSyms name = do
    lex  <- asks envLexical
    glob <- askGlobal
    pkg  <- asks envPackage
    let names = nub [name, toPackage pkg name, toGlobal name]
    syms <- forM [lex, glob] $ \pad -> do
        forM names $ \name' -> do
            case lookupPad name' pad of
                Just tvar -> do
                    refs  <- liftSTM $ mapM readTVar tvar
                    forM refs $ \ref -> do
                        val <- readRef ref
                        return (name', val)
                Nothing -> return []
    return $ concat (concat syms)

-- Reduction ---------------------------------------------------------------

retVal :: Val -> Eval Val
retVal val = evaluate (Val val)

retItem :: Val -> Eval Val
retItem val = do
    ifListContext
        (retVal $ VList [val])
        (retVal $ val)

addGlobalSym :: (Pad -> Pad) -> Eval ()
addGlobalSym newSym = do
    glob <- asks envGlobal
    liftSTM $ do
        syms <- readTVar glob
        writeTVar glob (newSym syms)

trapVal :: Val -> Eval a -> Eval a
trapVal val action = case val of
    VError str posList -> do
        pos <- asks envPos
        shiftT . const . return $ VError str (pos:posList)
    VControl c      -> retControl c
    _               -> action

evalVar :: Var -> Eval Val
evalVar name = do
    v <- findVar name
    case v of
        Just var -> readRef var
        _ | (':':rest) <- name -> return $ VType (mkType rest)
        _ -> retError "Undeclared variable" name

enterLValue :: Eval a -> Eval a
enterLValue = local (\e -> e{ envLValue = True })
enterRValue :: Eval a -> Eval a
enterRValue = local (\e -> e{ envLValue = False })

findVar :: Var -> Eval (Maybe VRef)
findVar name = do
    rv <- findVarRef name
    case rv of
        Nothing  -> case name of
            ('&':_) -> maybeM (findSub name Nothing []) $ \sub -> do
                return $ codeRef sub
            _ -> return Nothing
        Just ref -> fmap Just $ liftSTM (readTVar ref)

findVarRef :: Var -> Eval (Maybe (TVar VRef))
findVarRef name
    | Just (package, name') <- breakOnGlue "::" name
    , Just (sig, "") <- breakOnGlue "CALLER" package = do
        maybeCaller <- asks envCaller
        case maybeCaller of
            Just env -> local (const env) $ do
                findVarRef (sig ++ name')
            Nothing -> retError "cannot access CALLER:: in top level" name
    | Just (package, name') <- breakOnGlue "::" name
    , Just (sig, "") <- breakOnGlue "OUTER" package = do
        maybeOuter <- asks envOuter
        case maybeOuter of
            Just env -> local (const env) $ do
                findVarRef (sig ++ name')
            Nothing -> retError "cannot access OUTER:: in top level" name
    | ('$':'?':_) <- name = do
        rv  <- getMagical name
        case rv of
            Nothing  -> doFindVarRef name
            Just val -> do
                tvar <- liftSTM $ newTVar (MkRef . constScalar $ val)
                return $ Just tvar
    | otherwise = doFindVarRef name
    where
    doFindVarRef :: Var -> Eval (Maybe (TVar VRef))
    doFindVarRef name = do
        callCC $ \foundIt -> do
            lexSym <- fmap (findSym name . envLexical) ask
            when (isJust lexSym) $ foundIt lexSym
            glob   <- liftSTM . readTVar . envGlobal =<< ask
            let globSym = findSym name glob
            when (isJust globSym) $ foundIt globSym
            let globSym = findSym (toGlobal name) glob
            when (isJust globSym) $ foundIt globSym
            return Nothing

posSym :: Value a => (Pos -> a) -> Eval (Maybe Val)
posSym f = fmap (Just . castV . f) $ asks envPos
constSym :: String -> Eval (Maybe Val)
constSym = return . Just . castV

{-|
Evaluate the \'magical\' variable associated with a given name. Returns 
@Nothing@ if the name does not match a known magical.
-}
getMagical :: String -- ^ Name of the magical var to evaluate
           -> Eval (Maybe Val)
getMagical "$?FILE"     = posSym posName
getMagical "$?LINE"     = posSym posBeginLine
getMagical "$?COLUMN"   = posSym posBeginColumn
getMagical "$?POSITION" = posSym pretty
getMagical "$?MODULE"   = constSym "main"
getMagical "$?OS"       = constSym $ getConfig "osname"
getMagical "$?CLASS"    = fmap (Just . VType . mkType) (asks envPackage)
getMagical "$?PACKAGE"  = fmap (Just . VType . mkType) (asks envPackage)
getMagical "$?ROLE"     = fmap (Just . VType . mkType) (asks envPackage)
getMagical _            = return Nothing

evalRef :: VRef -> Eval Val
evalRef ref = do
    if refType ref == (mkType "Thunk") then forceRef ref else do
    val <- callCC $ \esc -> do
        cxt <- asks envContext
        lv  <- asks envLValue
        let typ = typeOfCxt cxt
            isCollectionRef = (refType ref /= mkType "Scalar")
        -- If RValue, read from the reference
        unless lv $ do
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

-- | Reduce an expression into its value. This is the workhorse of "Pugs.Eval".
reduce :: Exp -- ^ The expression to reduce
       -> Eval Val

-- Reduction for mutables
reduce (Val v@(VRef var)) = do
    lv <- asks envLValue
    if lv then retVal v else do
    rv <- readRef var
    retVal rv

-- Reduction for constants
reduce (Val v) = retVal v

-- Reduction for variables
reduce (Var name) = do
    v <- findVar name
    case v of
        Just var -> evalRef var
        _ | (':':rest) <- name -> return $ VType (mkType rest)
        _ -> retError "Undeclared variable" name

reduce (Stmts this rest) | Noop <- unwrap rest = reduce this
reduce (Stmts this rest) | Noop <- unwrap this = reduce rest

reduce (Stmts this rest) = do
    val <- enterContext cxtVoid $ reduce this
    trapVal val $ case unwrap rest of
        (Syn "env" []) -> do
            env <- ask
            writeVar "$*_" val
            return . VControl $ ControlEnv env
        _ -> reduce rest

reduce (Syn "env" []) = do
    env <- ask
    -- writeVar "$*_" val
    return . VControl $ ControlEnv env

reduce (Pos pos exp) = do
    local (\e -> e{ envPos = pos }) $ do
        evalExp exp

reduce (Pad SMy lex exp) = do
    -- heuristics: if we are repeating ourselves, generate a new TVar.
    lex' <- fmap mkPad $ liftSTM $ forM (padToList lex) $ \(name, tvars) -> do
        tvars' <- forM tvars $ \orig@(fresh, _) -> do
            isFresh <- readTVar fresh
            if isFresh then do { writeTVar fresh False; return orig } else do
            -- regen TVar -- we have re-entered this scope
            ref     <- newObject (typeOfSigil $ head name)
            tvar'   <- newTVar ref
            return (fresh, tvar')
        return (name, tvars')
    local (\e -> e{ envLexical = lex' `unionPads` envLexical e }) $ do
        evalExp exp

reduce (Pad _ lex exp) = do
    local (\e -> e{ envLexical = lex `unionPads` envLexical e }) $ do
        evalExp exp

-- Special case: my (undef) is no-op
reduce (Sym _ "" exp) = evalExp exp

reduce (Sym scope name exp) = do
    ref <- newObject (typeOfSigil $ head name)
    sym <- case name of
        ('&':_) -> genMultiSym name ref
        _       -> genSym name ref
    case scope of
        SMy     -> enterLex [ sym ] $ evalExp exp
        SState  -> enterLex [ sym ] $ evalExp exp
        _       -> do { addGlobalSym sym; evalExp exp }

-- Context forcing
reduce (Cxt cxt exp) = do
    val <- enterEvalContext cxt exp
    enterEvalContext cxt (Val val) -- force casting

-- Reduction for no-operations
reduce Noop = retEmpty

-- Reduction for syntactic constructs
reduce exp@(Syn name exps) = case name of
    "block" -> do
        let [body] = exps
        enterBlock $ reduce body
    "sub" -> do
        let [exp] = exps
        (VCode sub) <- enterEvalContext (cxtItem "Code") exp
        env     <- ask
        cont    <- if subType sub /= SubCoroutine then return Nothing else liftSTM $ do
            tvar <- newTVar undefined
            let thunk = MkThunk . fix $ \redo -> do
                evalExp $ subBody sub
                liftSTM $ writeTVar tvar thunk
                redo
            writeTVar tvar thunk
            return $ Just tvar
        retVal $ VCode sub
            { subEnv  = Just (env{ envStash = "" })
            , subCont = cont
            }
    "if" -> doCond id 
    "unless" -> doCond not
    "for" -> do
        let [list, body] = exps
        av    <- enterLValue $ enterEvalContext cxtSlurpyAny list
        vsub  <- enterRValue $ enterEvalContext (cxtItem "Code") body
        -- XXX this is wrong -- should use Array.next
        sz    <- join $ doArray av array_fetchSize
        fetch <- doArray av array_fetchElem
        elms  <- mapM fetch [0..sz-1]
        VCode sub <- fromVal vsub
        -- XXX: need clarification -- this makes
        --      for @x { ... } into for @x -> $_ {...}
        let arity = max 1 $ length (subParams sub)
            runBody [] _ = retVal undef
            runBody vs sub' = do
                let (these, rest) = arity `splitAt` vs
                genSymCC "&next" $ \symNext -> do
                    genSymPrim "&redo" (const $ runBody vs sub') $ \symRedo -> do
                        apply (updateSubPad sub' (symRedo . symNext)) Nothing $
                            map (Val . VRef . MkRef) these
                runBody rest sub'
        genSymCC "&last" $ \symLast -> do
            let munge sub | subParams sub == [defaultArrayParam] =
                    munge sub{ subParams = [defaultScalarParam] }
                munge sub = updateSubPad sub symLast
            runBody elms $ munge sub
    "gather" -> do
        let [exp] = exps
        sub     <- fromVal =<< evalExp exp
        av      <- newArray []
        symTake <- genSym "@?TAKE" (MkRef av)
        apply (updateSubPad sub symTake) Nothing []
        fmap VList $ readIVar av
    "loop" -> do
        let [pre, cond, post, body] = case exps of { [_] -> exps'; _ -> exps }
            exps' = [emptyExp, Val (VBool True), emptyExp] ++ exps
            evalCond | unwrap cond == Noop = return True
                     | otherwise = fromVal =<< enterEvalContext (cxtItem "Bool") cond
        evalExp pre
        vb  <- evalCond
        if not vb then retEmpty else do
        genSymCC "&last" $ \symLast -> enterLex [symLast] $ fix $ \runBody -> do
            genSymPrim "&redo" (const $ runBody) $ \symRedo -> do
            let runNext = do
                valPost <- evalExp post
                vb      <- evalCond
                trapVal valPost $ if vb then runBody else retEmpty
            genSymPrim "&next" (const $ runNext) $ \symNext -> do
            valBody <- enterLex [symRedo, symNext] $ evalExp body
            trapVal valBody $ runNext
    "given" -> do
        let [topic, body] = exps
        vtopic <- fromVal =<< enterLValue (enterEvalContext cxtItemAny topic)
        enterGiven vtopic $ enterEvalContext (cxtItem "Code") body
    "when" -> do
        let [match, body] = exps
        break  <- evalVar "&?BLOCK_EXIT"
        vbreak <- fromVal break
        result <- reduce $ case unwrap match of
            App _ (Just (Var "$_")) _ -> match
            _ -> App (Var "&infix:~~") Nothing [(Var "$_"), match]
        rb     <- fromVal result
        if rb
            then enterWhen (subBody vbreak) $ apply vbreak Nothing [body]
            else retVal undef
    "default" -> do
        let [body] = exps
        break  <- evalVar "&?BLOCK_EXIT"
        vbreak <- fromVal break
        enterWhen (subBody vbreak) $ apply vbreak Nothing [body]
    "while" -> doWhileUntil id False
    "until" -> doWhileUntil not False
    "postwhile" -> doWhileUntil id True
    "postuntil" -> doWhileUntil not True
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
        lv  <- asks envLValue
        retVal $ if lv then refVal else val
    "::=" -> reduce (Syn ":=" exps)
    ":=" | [Syn "," vars, Syn "," vexps] <- unwrap exps -> do
        when (length vars > length vexps) $ do
            fail $ "Wrong number of binding parameters: "
                ++ (show $ length vexps) ++ " actual, "
                ++ (show $ length vars) ++ " expected"
        -- env' <- cloneEnv env -- FULL THUNKING
        names <- forM vars $ \var -> case unwrap var of
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
            rv   <- findVarRef name
            case rv of
                Just ioRef -> return (ioRef, ref)
                Nothing -> do
                    retError "Undeclared variable" name
        forM_ bindings $ \(ioRef, ref) -> do
            liftSTM $ writeTVar ioRef ref
        return $ case map (VRef . snd) bindings of
            [v] -> v
            vs  -> VList vs
    ":=" -> do
        let [var, vexp] = exps
            expand e | e'@(Syn "," _) <- unwrap e = e'
            expand e = Syn "," [e]
        reduce (Syn ":=" [expand var, expand vexp])
    "=>" -> do
        let [keyExp, valExp] = exps
        key <- enterEvalContext cxtItemAny keyExp
        val <- enterEvalContext cxtItemAny valExp
        retItem $ castV (key, val)
    "*" | [Syn syn [exp]] <- unwrap exps --  * cancels out [] and {}
        , syn == "\\{}" || syn == "\\[]"
        -> enterEvalContext cxtSlurpyAny exp
    "*" -> do -- first stab at an implementation
        let [exp] = exps
        val     <- enterRValue $ enterEvalContext cxtSlurpyAny exp
        vals    <- fromVals val
        retVal $ VList $ concat vals
    "," -> do
        vals <- mapM (enterEvalContext cxtSlurpyAny) exps
        retVal . VList . concat $ map vCast vals
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
        retItem $ VRef av
    -- XXX evil hack for infinite slices
    "[]" | [lhs, App (Var "&postfix:...") invs args] <- unwrap exps
         , [idx] <- maybeToList invs ++ args
--       , not (envLValue env)
         -> reduce (Syn "[...]" [lhs, idx])
    "[]" | [lhs, App (Var "&infix:..") invs args] <- unwrap exps
         , [idx, Val (VNum n)] <- maybeToList invs ++ args
         , n == 1/0
--       , not (envLValue env)
         -> reduce (Syn "[...]" [lhs, idx])
    "[]" -> do
        let [listExp, indexExp] = exps
        idxCxt  <- cxtOfExp indexExp 
        {- if envLValue env
            then cxtOfExp indexExp else return (envContext env)
        -}
        idxVal  <- enterRValue $ enterEvalContext idxCxt indexExp
        varVal  <- enterLValue $ enterEvalContext (cxtItem "Array") listExp
        lv      <- asks envLValue
        doFetch (mkFetch $ doArray varVal array_fetchElem)
                (mkFetch $ doArray varVal array_fetchVal)
                (fromVal idxVal) lv (not (isSlurpyCxt idxCxt))
    "[...]" -> do
        let [listExp, indexExp] = exps
        idxVal  <- enterRValue $ enterEvalContext (cxtItem "Int") indexExp
        idx     <- fromVal idxVal
        listVal <- enterRValue $ enterEvalContext (cxtItem "Array") listExp
        list    <- fromVal listVal
        -- error $ show list
        -- elms    <- mapM fromVal list -- flatten
        retVal $ VList (drop idx $ list)
    "@{}" -> do
        let [exp] = exps
        val     <- enterEvalContext (cxtItem "Hash") exp
        ivar    <- doArray val IArray
        evalRef (MkRef ivar)
    "%{}" -> do
        let [exp] = exps
        val     <- enterEvalContext (cxtItem "Hash") exp
        ivar    <- doHash val IHash
        evalRef (MkRef ivar)
    "&{}" -> do
        let [exp] = exps
        val     <- enterEvalContext (cxtItem "Hash") exp
        sub     <- fromVal val
        return $ VCode sub
    "${}" -> do
        let [exp] = exps
        val     <- enterEvalContext (cxtItem "Hash") exp
        ref     <- fromVal val
        evalRef ref
    sigil:"::()" -> do
        -- These are all parts of the name
        parts   <- mapM fromVal =<< mapM evalExp exps
        -- Now we only have to add the sigil in front of the string and join
        -- the parts with "::".
        let varname = sigil:(concat . (intersperse "::") $ parts)
        -- Finally, eval the varname.
        evalExp . Var $ varname
    "{}" -> do
        let [listExp, indexExp] = exps
        idxCxt  <- cxtOfExp indexExp 
        {- if envLValue env
            then cxtOfExp indexExp else return (envContext env)
        -}
        idxVal  <- enterRValue $ enterEvalContext idxCxt indexExp
        varVal  <- enterLValue $ enterEvalContext (cxtItem "Hash") listExp
        lv      <- asks envLValue
        doFetch (mkFetch $ doHash varVal hash_fetchElem)
                (mkFetch $ doHash varVal hash_fetchVal)
                (fromVal idxVal) lv (not (isSlurpyCxt idxCxt))
    "rx" -> do
        let [exp, adverbs] = exps
        hv      <- fromVal =<< evalExp adverbs
        val     <- enterEvalContext (cxtItem "Str") exp
        str     <- fromVal val
        p5      <- fromAdverb hv ["P5", "Perl5", "perl5"]
        p5flags <- fromAdverb hv ["P5", "Perl5", "perl5"]
        flag_g  <- fromAdverb hv ["g", "global"]
        flag_i  <- fromAdverb hv ["i", "ignorecase"]
        flag_s  <- fromAdverb hv ["stringify"] -- XXX hack
        let rx | p5 = MkRulePCRE p5re g flag_s 
               | otherwise = MkRulePGE str g flag_s
            g = ('g' `elem` p5flags || flag_g)
            p5re = mkRegexWithPCRE (encodeUTF8 str) $
                        [ pcreUtf8
                        , ('i' `elem` p5flags || flag_i) `implies` pcreCaseless
                        , ('m' `elem` p5flags) `implies` pcreMultiline
                        , ('s' `elem` p5flags) `implies` pcreDotall
                        , ('x' `elem` p5flags) `implies` pcreExtended
                        ]
        retVal $ VRule rx
        where
        implies True  = id
        implies False = const 0
        fromAdverb _ [] = fromVal undef
        fromAdverb hv (k:ks) = case lookup k hv of
            Just v  -> fromVal v
            Nothing -> fromAdverb hv ks
    "//" -> reduce (Syn "rx" exps)
    "match" -> reduce (Syn "rx" exps) -- XXX - this is wrong
    "subst" -> do
        let [exp, subst, adverbs] = exps
        (VRule rx)  <- reduce (Syn "rx" [exp, adverbs])
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
            retError "Inline: Unknown language" langVal
        modVal  <- readVar "$?MODULE"
        mod     <- fromVal modVal
#ifndef HADDOCK
        let file = (`concatMap` mod) $ \v -> case v of
            { '-' -> "__"; _ | isAlphaNum v -> [v] ; _ -> "_" }
#endif
        externRequire "Haskell" (file ++ ".o")
        retEmpty
    syn | last syn == '=' -> do
        let [lhs, exp] = exps
            op = "&infix:" ++ init syn
        evalExp $ Syn "=" [lhs, App (Var op) Nothing [lhs, exp]]
    _ -> retError "Unknown syntactic construct" exp
    where
    doCond :: (Bool -> Bool) -> Eval Val
    doCond f = do
        let [cond, bodyIf, bodyElse] = exps
        vbool     <- enterEvalContext (cxtItem "Bool") cond
        vb        <- fromVal vbool
        if (f vb)
            then reduce bodyIf
            else reduce bodyElse
    -- XXX This treatment of while/until loops probably needs work
    doWhileUntil :: (Bool -> Bool) -> Bool -> Eval Val
    doWhileUntil f postloop = do
        let [cond, body] = exps
        -- XXX redo for initial run
        if postloop
            then reduce body
            else retEmpty
        enterWhile . fix $ \runLoop -> do
            vbool <- enterEvalContext (cxtItem "Bool") cond
            vb    <- fromVal vbool
            case f vb of
                True -> fix $ \runBody -> do
                    genSymPrim "&redo" (const $ runBody) $ \symRedo -> do
                    rv <- enterLex [symRedo] $ reduce body
                    case rv of
                        VError _ _  -> retVal rv
                        _           -> runLoop
                _ -> retVal vbool

-- XXX absolutely evil bloody hack for context hinters
reduce (App (Var "&hash") invs args) =
    enterEvalContext cxtItemAny $ Syn "\\{}" [Syn "," $ maybeToList invs ++ args]

reduce (App (Var "&list") invs args) =
    enterEvalContext cxtSlurpyAny $ case maybeToList invs ++ args of
        []    -> Val (VList [])
        [exp] -> exp
        exps  -> Syn "," exps

reduce (App (Var "&scalar") invs args)
    | [exp] <- maybeToList invs ++ args = enterEvalContext cxtItemAny exp
    | otherwise = enterEvalContext cxtItemAny $ Syn "," (maybeToList invs ++ args)

-- XXX absolutely evil bloody hack for "zip"
reduce (App (Var "&zip") invs args) = do
    vals <- mapM (enterRValue . enterEvalContext (cxtItem "Array")) (maybeToList invs ++ args)
    val  <- op0Zip vals
    retVal val

-- XXX absolutely evil bloody hack for "not"
reduce (App (Var "&not") Nothing []) = retEmpty

reduce (App (Var "&not") invs args) = do
    bool <- fromVal =<< evalExp (last $ maybeToList invs ++ args)
    retVal $ VBool (not bool)

-- XXX absolutely evil bloody hack for "goto"
reduce (App (Var "&goto") (Just subExp) args) = do
    vsub <- enterEvalContext (cxtItem "Code") subExp
    sub <- fromVal vsub
    local callerEnv $ do
        val <- apply sub Nothing args
        shiftT $ const (retVal val)
    where
    callerEnv :: Env -> Env
    callerEnv env = let caller = maybe env id (envCaller env) in
        env{ envCaller  = envCaller caller
           , envContext = envContext caller
           , envLValue  = envLValue caller
           , envDepth   = envDepth caller
           }

-- XXX absolutely evil bloody hack for "assuming"
reduce (App (Var "&assuming") (Just subExp) args) = do
    vsub <- enterEvalContext (cxtItem "Code") subExp
    sub <- fromVal vsub
    case bindSomeParams sub Nothing args of
        Left errMsg      -> fail errMsg
        Right curriedSub -> retVal $ castV $ curriedSub

reduce (App (Var "&infix:=>") invs args) = reduce (Syn "=>" (maybeToList invs ++ args))

reduce (App (Var name@('&':_)) invs args) = do
    sub     <- findSub name invs args
    case sub of
        Just sub    -> applySub sub invs args
        _ | [Syn "," args'] <- unwrap args -> do
            sub <- findSub name invs args'
            if isNothing sub then err else do
            fail $ "Extra space found after " ++ name ++ " (...) -- did you mean " ++ name ++ "(...) instead?"
        _ -> err
    where
    err = retError "No compatible subroutine found" name
    applySub :: VCode -> (Maybe Exp) -> [Exp] -> Eval Val
    applySub sub invs args
        -- list-associativity
        | MkCode{ subAssoc = "list" }      <- sub
        , (App (Var name') Nothing args'):rest  <- args
        , name == name'
        = applySub sub invs (args' ++ rest)
        -- fix subParams to agree with number of actual arguments
        | MkCode{ subAssoc = "list", subParams = (p:_) }   <- sub
        = apply sub{ subParams = (length args) `replicate` p } invs args
        -- chain-associativity
        | MkCode{ subAssoc = "chain" }  <- sub
        , (App _ _ []):_                <- args
        = mungeChainSub sub args
        | MkCode{ subAssoc = "chain", subParams = (p:_) }   <- sub
        = apply sub{ subParams = (length args) `replicate` p } invs args
        -- normal application
        | otherwise
        = apply sub invs args
    mungeChainSub :: VCode -> [Exp] -> Eval Val
    mungeChainSub sub args = do
        let MkCode{ subAssoc = "chain", subParams = (p:_) } = sub
            (App (Var name') invs' args'):rest = args
        theSub   <- findSub name' invs' args'
        case theSub of
            Just sub'    -> applyChainSub sub args sub' args' rest
            Nothing      -> apply sub{ subParams = (length args) `replicate` p } Nothing args -- XXX Wrong
    applyChainSub :: VCode -> [Exp] -> VCode -> [Exp] -> [Exp] -> Eval Val
    applyChainSub sub args sub' args' rest
        | MkCode{ subAssoc = "chain", subBody = fun, subParams = prm }   <- sub
        , MkCode{ subAssoc = "chain", subBody = fun', subParams = prm' } <- sub'
        = applySub sub{ subParams = prm ++ tail prm', subBody = Prim $ chainFun prm' fun' prm fun } Nothing (args' ++ rest)
        | MkCode{ subAssoc = "chain", subParams = (p:_) }   <- sub
        = apply sub{ subParams = (length args) `replicate` p } Nothing args -- XXX Wrong
        | otherwise
        = internalError "applyChainsub did not match a chain subroutine"

reduce (App subExp invs args) = do
    vsub <- enterEvalContext (cxtItem "Code") subExp
    (`juncApply` [ApplyArg "" vsub False]) $ \[arg] -> do
        sub  <- fromVal $ argValue arg
        apply sub invs args

reduce exp = retError "Invalid expression" exp

{-|
Return the context that an expression bestows upon a hash or array
subscript. See 'reduce' for \{\} and \[\]. (Is this correct?)
-}
cxtOfExp :: Exp -- ^ Expression to find the context of
         -> Eval Cxt
cxtOfExp (Pos _ exp)            = cxtOfExp exp
cxtOfExp (Cxt cxt _)            = return cxt
cxtOfExp (Syn "," _)            = return cxtSlurpyAny
cxtOfExp (Syn "[]" [_, exp])    = cxtOfExp exp
cxtOfExp (Syn "{}" [_, exp])    = cxtOfExp exp
cxtOfExp (Syn (sigil:"{}") _) = return $ cxtOfSigil sigil
cxtOfExp (Val (VList _))        = return cxtSlurpyAny
cxtOfExp (Val (VRef ref))       = do
    cls <- asks envClasses
    let typ = refType ref
    return $ if isaType cls "List" typ
        then cxtSlurpyAny
        else CxtItem typ
cxtOfExp (Val _)                = return cxtItemAny
cxtOfExp (Var (sigil:_))            = return $ cxtOfSigil sigil
cxtOfExp (App (Var "&list") _ _)   = return cxtSlurpyAny
cxtOfExp (App (Var "&scalar") _ _) = return cxtSlurpyAny
cxtOfExp (App (Var name) invs args)   = do
    -- inspect the return type of the function here
    env <- ask
    sub <- findSub name invs args
    return $ case sub of
        Just sub
            | isaType (envClasses env) "Scalar" (subReturns sub)
            -> CxtItem (subReturns sub)
        _ -> cxtSlurpyAny
cxtOfExp _                      = return cxtSlurpyAny

findSub :: String -> Maybe Exp -> [Exp] -> Eval (Maybe VCode)
findSub name' invs args = do
    let name = possiblyFixOperatorName name'
    case invs of
        Just _ | Just (package, name') <- breakOnGlue "::" name
                 , Just (sig, "") <- breakOnGlue "SUPER" package -> do
            typ <- asks envPackage
            findSuperSub (mkType typ) (sig ++ name')
        Just exp | not (':' `elem` drop 2 name) -> do
            typ     <- evalInvType $ unwrap exp
            if typ == mkType "Scalar::Perl5" then runPerl5Sub name else do
            findTypedSub typ name
        _ | [exp] <- args -> do
            typ     <- evalInvType $ unwrap exp
            findTypedSub typ name
        _ -> findBuiltinSub name
    where
    findSuperSub :: Type -> String -> Eval (Maybe VCode)
    findSuperSub typ name = do
        let pkg = showType typ
            qualified = (head name:pkg) ++ "::" ++ tail name
        subs    <- findWithSuper pkg name
        subs'   <- if isJust subs then return subs else findBuiltinSub name
        case subs' of
            Just sub | subName sub == qualified -> return Nothing
            _   -> return subs'
    findTypedSub :: Type -> String -> Eval (Maybe VCode)
    findTypedSub typ name = do
        subs    <- findWithPkg (showType typ) name
        if isJust subs then return subs else findBuiltinSub name
    findBuiltinSub :: String -> Eval (Maybe VCode)
    findBuiltinSub name = do
        sub <- findSub' name
        if isNothing sub then possiblyBuildMetaopVCode name else return sub
    evalInvType :: Exp -> Eval Type
    evalInvType x@(Var (':':typ)) = do
        typ' <- evalExpType x
        return $ if typ' == mkType "Scalar::Perl5" then typ' else mkType typ
    evalInvType (App (Var "&new") (Just inv) _) = do
        evalInvType $ unwrap inv
    evalInvType x@(App (Var _) (Just inv) _) = do
        typ <- evalInvType $ unwrap inv
        if typ == mkType "Scalar::Perl5" then return typ else evalExpType x
    evalInvType x = evalExpType $ unwrap x
    runPerl5Sub :: String -> Eval (Maybe VCode)
    runPerl5Sub name = do
        metaSub <- possiblyBuildMetaopVCode name
        if isJust metaSub then return metaSub else do
        return . Just $ mkPrim
            { subName     = name
            , subParams   = makeParams ["Object", "List", "Named"]
            , subReturns  = mkType "Scalar::Perl5"
            , subBody     = Prim $ \(inv:named:pos:_) -> do
                sv      <- fromVal inv
                posSVs  <- fromVals pos
                namSVs  <- fmap concat (fromVals named)
                let svs = posSVs ++ namSVs
                found   <- liftIO $ canPerl5 sv (tail name)
                found'  <- liftIO $ if found
                    then return found
                    else canPerl5 sv "AUTOLOAD"
                if not found' then evalExp (App (Var name) Nothing (map (Val . PerlSV) (sv:svs))) else do
                env     <- ask
                rv      <- liftIO $ do
                    envSV   <- mkVal (VControl $ ControlEnv env)
                    subSV   <- vstrToSV $ tail name
                    callPerl5 subSV sv svs envSV (enumCxt $ envContext env)
                return $ case rv of
                    [sv]    -> PerlSV sv
                    _       -> VList (map PerlSV rv)
            }
    possiblyBuildMetaopVCode :: String -> Eval (Maybe VCode)
    possiblyBuildMetaopVCode op' | "&prefix:[" `isPrefixOf` op', "]" `isSuffixOf` op' = do 
        -- Strip the trailing "]" from op
        let op = drop 9 (init op')
        -- We try to find the userdefined sub.
        -- We use the first two elements of invs as invocants, as these are the
        -- types of the op.
            rv = findSub ("&infix:" ++ op) Nothing (take 2 $ args ++ [Val undef, Val undef])
        maybeM rv $ \code -> return $ mkPrim
            { subName     = "&prefix:[" ++ op ++ "]"
            , subType     = SubPrim
            , subAssoc    = "spre"
            , subParams   = makeParams ["List"]
            , subReturns  = mkType "Str"
            , subBody     = Prim $ \[vs] -> do
                list_of_args <- fromVal vs
                op2Fold (list_of_args) (VCode code)
            }
        -- Now we construct the sub. Is there a more simple way to do it?
    possiblyBuildMetaopVCode op' | "&prefix:" `isPrefixOf` op', "\171" `isSuffixOf` op' = do 
        let op = drop 8 (init op')
        possiblyBuildMetaopVCode ("&prefix:" ++ op ++ "<<")
    possiblyBuildMetaopVCode op' | "&prefix:" `isPrefixOf` op', "<<" `isSuffixOf` op' = do 
        let op = drop 8 (init (init op'))
            rv = findSub ("&prefix:" ++ op) Nothing [head $ args ++ [Val undef]]
        maybeM rv $ \code -> return $ mkPrim
            { subName     = "&prefix:" ++ op ++ "<<"
            , subType     = SubPrim
            , subAssoc    = subAssoc code
            , subParams   = subParams code
            , subReturns  = mkType "List"
            , subBody     = Prim
                (\x -> op1HyperPrefix code (listArg x))
            }
    possiblyBuildMetaopVCode op' | "&postfix:\187" `isPrefixOf` op' = do
        let op = drop 10 op'
        possiblyBuildMetaopVCode ("&postfix:>>" ++ op)
    possiblyBuildMetaopVCode op' | "&postfix:>>" `isPrefixOf` op' = do
        let op = drop 11 op'
            rv = findSub ("&postfix:" ++ op) Nothing [head $ args ++ [Val undef]]
        maybeM rv $ \code -> return $ mkPrim
            { subName     = "&postfix:>>" ++ op
            , subType     = SubPrim
            , subAssoc    = subAssoc code
            , subParams   = subParams code
            , subReturns  = mkType "List"
            , subBody     = Prim
                (\x -> op1HyperPostfix code (listArg x))
            }
    possiblyBuildMetaopVCode op' | "&infix:\187" `isPrefixOf` op', "\171" `isSuffixOf` op' = do 
        let op = drop 8 (init op')
        possiblyBuildMetaopVCode ("&infix:>>" ++ op ++ "<<")
    possiblyBuildMetaopVCode op' | "&infix:>>" `isPrefixOf` op', "<<" `isSuffixOf` op' = do 
        let op = drop 9 (init (init op'))
            rv = findSub ("&infix:" ++ op) Nothing (take 2 (args ++ [Val undef, Val undef]))
        maybeM rv $ \code -> return $ mkPrim
            { subName     = "&infix:>>" ++ op ++ "<<"
            , subType     = SubPrim
            , subAssoc    = subAssoc code
            , subParams   = makeParams ["Any", "Any"]
            , subReturns  = mkType "List"
            , subBody     = Prim (\[x, y] -> op2Hyper code x y)
            }
        -- Taken from Pugs.Prim. Probably this should be refactored. (?)
    possiblyBuildMetaopVCode _ = return Nothing
    listArg [x] = x
    listArg xs = VList xs
    makeParams = map (\p -> p{ isWritable = isLValue p }) . foldr foldParam [] . map takeWord
    takeWord = takeWhile isWord . dropWhile (not . isWord)
    isWord   = not . (`elem` "(),:")
    findAttrs pkg = do
        maybeM (findVar (':':'*':pkg)) $ \ref -> do
            meta    <- readRef ref
            fetch   <- doHash meta hash_fetchVal
            fromVal =<< fetch "traits"
    findWithPkg :: String -> String -> Eval (Maybe VCode)
    findWithPkg pkg name = do
        subs <- findSub' (('&':pkg) ++ "::" ++ tail name)
        if isJust subs then return subs else do
        findWithSuper pkg name
    findWithSuper :: String -> String -> Eval (Maybe VCode)
    findWithSuper pkg name = do
        -- get superclasses
        attrs <- fmap (fmap (filter (/= pkg) . nub)) $ findAttrs pkg
        if isNothing attrs || null (fromJust attrs) then findSub' name else do
        (`fix` (fromJust attrs)) $ \run pkgs -> do
            if null pkgs then return Nothing else do
            subs <- findWithPkg (head pkgs) name
            if isJust subs then return subs else run (tail pkgs)
    findSub' :: String -> Eval (Maybe VCode)
    findSub' name = do
        subSyms     <- findSyms name
        lens        <- mapM argSlurpLen (unwrap $ maybeToList invs ++ args)
        doFindSub (sum lens) subSyms
    argSlurpLen :: Exp -> Eval Int
    argSlurpLen (Val listMVal) = do
        listVal  <- fromVal listMVal
        return $ length (vCast listVal :: [Val])
    argSlurpLen (Var name) = do
        listMVal <- evalVar name
        listVal  <- fromVal listMVal
        return $ length (vCast listVal :: [Val])
    argSlurpLen (Syn "," list) =  return $ length list
    argSlurpLen _ = return 1 -- XXX
    doFindSub :: Int -> [(String, Val)] -> Eval (Maybe VCode)
    doFindSub slurpLen subSyms = do
        subs' <- subs slurpLen subSyms
        -- let foo (x, sub) = show x ++ show (map paramContext $ subParams sub)
        -- trace (unlines $ map foo $ sort subs') return ()
        return $ case sort subs' of
            ((_, sub):_)    -> Just sub
            _               -> Nothing
    subs :: Int -> [(String, Val)] -> Eval [((Bool, Bool, Int, Int), VCode)]
    subs slurpLen subSyms = (liftM catMaybes) $ (`mapM` subSyms) $ \(_, val) -> do
        sub@(MkCode{ subReturns = ret, subParams = prms }) <- fromVal val
        let rv = return $ arityMatch sub (length (maybeToList invs ++ args)) slurpLen
        maybeM rv $ \fun -> do
            -- if deltaFromCxt ret == 0 then return Nothing else do
            let pairs = map (typeOfCxt . paramContext) prms
                            `zip` (map unwrap $ maybeToList invs ++ args)
            deltaCxt    <- deltaFromCxt ret
            deltaArgs   <- mapM deltaFromPair pairs
            let bound = either (const False) (const True) $ bindParams sub invs args
            return ((isMulti sub, bound, sum deltaArgs, deltaCxt), fun)
    deltaFromCxt :: Type -> Eval Int
    deltaFromCxt x  = do
        cls <- asks envClasses
        cxt <- asks envContext
        return $ deltaType cls (typeOfCxt cxt) x
    deltaFromPair (x, y) = do
        cls <- asks envClasses
        typ <- evalExpType y
        return $ deltaType cls x typ

evalExpType :: Exp -> Eval Type
evalExpType (Var var) = do
    rv  <- findVar var
    case rv of
        Nothing  -> return $ typeOfSigil (head var)
        Just ref -> evalValType (VRef ref)
evalExpType (Val val) = evalValType val
evalExpType (App (Val val) _ _) = do
    sub <- fromVal val
    return $ subReturns sub
evalExpType (App (Var "&new") (Just (Var (':':name))) _) = return $ mkType name
evalExpType (App (Var name) invs args) = do
    sub <- findSub name invs args
    case sub of
        Just sub    -> return $ subReturns sub
        Nothing     -> return $ mkType "Any"
evalExpType exp@(Syn syn _) | (syn ==) `any` words "{} []" = do
    val <- evalExp exp
    evalValType val
evalExpType (Cxt cxt _) | typeOfCxt cxt /= (mkType "Any") = return $ typeOfCxt cxt
evalExpType (Cxt _ exp) = evalExpType exp
evalExpType (Pos _ exp) = evalExpType exp
evalExpType (Pad _ _ exp) = evalExpType exp
evalExpType (Sym _ _ exp) = evalExpType exp
evalExpType (Stmts _ exp) = evalExpType exp
evalExpType (Syn "sub" [exp]) = evalExpType exp
evalExpType _ = return $ mkType "Any"

chainFun :: Params -> Exp -> Params -> Exp -> [Val] -> Eval Val
chainFun p1 f1 p2 f2 (v1:v2:vs) = do
    val <- applyExp SubPrim (chainArgs p1 [v1, v2]) f1
    case val of
        VBool False -> return val
        _           -> applyExp SubPrim (chainArgs p2 (v2:vs)) f2
    where
    chainArgs prms vals = map chainArg (prms `zip` vals)
    chainArg (p, v) = ApplyArg (paramName p) v False
chainFun _ _ _ _ _ = internalError "chainFun: Not enough parameters in Val list"

applyExp :: SubType -> [ApplyArg] -> Exp -> Eval Val
applyExp _ bound (Prim f) =
    f [ argValue arg | arg <- bound, (argName arg) /= "%_" ]
applyExp styp bound body = applyThunk styp bound (MkThunk $ evalExp body)

applyThunk :: SubType -> [ApplyArg] -> VThunk -> Eval Val
applyThunk _ [] thunk = thunk_force thunk
applyThunk styp bound@(arg:_) thunk = do
    -- introduce $?SELF and $_ as the first invocant.
    inv <- if styp <= SubMethod then invocant else return []
    pad <- formal
    enterLex (inv ++ pad) $ thunk_force thunk
    where
    formal = mapM argNameValue $ filter (not . null . argName) bound
    invocant = mapM (`genSym` (vCast $ argValue arg)) $ words "$?SELF $_"
    argNameValue (ApplyArg name val _) = genSym name (vCast val)

{-|
Apply a sub (or other code) to lists of invocants and arguments.

Mostly delegates to 'doApply' after explicitly retrieving the local 'Env'.
-}
apply :: VCode       -- ^ The sub to apply
      -> (Maybe Exp) -- ^ invocant
      -> [Exp]       -- ^ List of arguments (non-invocant)
      -> Eval Val
apply sub invs args = do
    env <- ask
    doApply env sub invs args

-- XXX - faking application of lexical contexts
-- XXX - what about defaulting that depends on a junction?
{-|
Apply a sub (or other code) to lists of invocants and arguments, in the 
specified context.
-}
doApply :: Env   -- ^ Environment to evaluate in
        -> VCode -- ^ Code to apply
        -> (Maybe Exp) -- ^ Invocants (arguments before the colon)
        -> [Exp] -- ^ Arguments (not including invocants)
        -> Eval Val
doApply env sub@MkCode{ subCont = cont, subBody = fun, subType = typ } invs args =
    case bindParams sub invs args of
        Left errMsg -> fail errMsg
        Right sub   -> do
            forM_ (subSlurpLimit sub) $ \limit@(n, _) -> do
                extra <- checkSlurpyLimit limit
                when (not $ null extra) $ do
                    fail $
                        "Too many slurpy arguments for " ++ subName sub ++ ": "
                        ++ show ((genericLength (take 1000 extra)) + n) ++ " actual, "
                        ++ show n ++ " expected"
            enterScope $ do
                (syms, bound) <- doBind [] (subBindings sub)
                -- trace (show bound) $ return ()
                val <- local fixEnv $ enterLex syms $ do
                    (`juncApply` bound) $ \realBound -> do
                        enterSub sub $ case cont of
                            Just tvar   -> do
                                thunk <- liftSTM $ readTVar tvar
                                applyThunk (subType sub) realBound thunk
                            Nothing     -> applyExp (subType sub) realBound fun
                retVal val
    where
    enterScope :: Eval Val -> Eval Val
    enterScope
        | typ >= SubBlock = id
        | otherwise       = resetT
    fixEnv :: Env -> Env
    fixEnv env
        | typ >= SubBlock = env
        | otherwise       = env
            { envCaller = Just env
            , envDepth = envDepth env + 1 }
    doBind :: [Pad -> Pad] -> [(Param, Exp)] -> Eval ([Pad -> Pad], [ApplyArg])
    doBind syms [] = return (syms, [])
    doBind syms ((prm, exp):rest) = do
        -- trace ("<== " ++ (show (prm, exp))) $ return ()
        let name = paramName prm
            cxt = cxtOfSigil $ head name
        (val, coll) <- enterContext cxt $ case exp of
            Syn "default" [exp] -> local fixEnv $ enterLex syms $ expToVal prm exp
            _                   -> expToVal prm exp
        -- trace ("==> " ++ (show val)) $ return ()
        boundRef <- fromVal val
        newSym   <- genSym name boundRef
        (syms', restArgs) <- doBind (newSym:syms) rest
        return (syms', ApplyArg name val coll:restArgs)
    expToVal :: Param -> Exp -> Eval (Val, Bool)
    expToVal MkParam{ isLazy = thunk, isLValue = lv, paramContext = cxt, paramName = name, isWritable = rw } exp = do
        env <- ask -- freeze environment at this point for thunks
        let eval = local (const env{ envLValue = lv }) $ do
            enterEvalContext (cxtOfSigil $ head name) exp
        val <- if thunk then return (VRef . thunkRef $ MkThunk eval) else do
            v   <- eval
            typ <- evalValType v
            let cls = envClasses env
            if isaType cls "Junction" typ then return v else do
            case (lv, rw) of
                (True, True)    -> return v
                (True, False)   -> do
                    --- not scalarRef!
                    case showType (typeOfSigil $ head name) of
                        "Hash"  -> fmap (VRef . hashRef) (fromVal v :: Eval VHash)
                        "Array" -> fmap (VRef . arrayRef) (fromVal v :: Eval VArray)
                        _       -> return (VRef $ scalarRef v) 
                (False, False)  -> return v -- XXX reduce to val?
                (False, True)   -> do
                    -- make a copy
                    ref <- newObject (typeOfSigil $ head name)
                    writeRef ref v
                    return (VRef ref)
        return (val, (isSlurpyCxt cxt || isCollapsed (typeOfCxt cxt)))
    checkSlurpyLimit :: (VInt, Exp) -> Eval [Val]
    checkSlurpyLimit (n, exp) = do
        listVal <- enterLValue $ enterEvalContext (cxtItem "Array") exp
        list    <- fromVal listVal
        elms    <- mapM fromVal list -- flatten
        return $ genericDrop n (concat elms :: [Val])
    isCollapsed :: Type -> Bool
    isCollapsed typ
        | isaType (envClasses env) "Bool" typ        = True
        | isaType (envClasses env) "Junction" typ    = True
        | otherwise                     = False

toPackage :: String -> String -> String
toPackage pkg name
    | (sigil, identifier) <- break (\x -> isAlpha x || x == '_') name
    , last sigil /= '*'
    = concat [sigil, pkg, "::", identifier]
    | otherwise = name

toGlobal :: String -> String
toGlobal name
    | (sigil, identifier) <- break (\x -> isAlpha x || x == '_') name
    , last sigil /= '*'
    = sigil ++ ('*':identifier)
    | otherwise = name


arityMatch :: VCode -> Int -> Int -> Maybe VCode
arityMatch sub@MkCode{ subAssoc = assoc, subParams = prms } argLen argSlurpLen
    | assoc == "list" || assoc == "chain"
    = Just sub
    | isNothing $ find (not . isSlurpy) prms -- XXX - what about empty ones?
    , assoc == "pre"
    , slurpLen <- length $ filter (\p -> isSlurpy p && head (paramName p) == '$') prms
    , hasArray <- isJust $ find (\p -> isSlurpy p && head (paramName p) /= '$') prms
    , if hasArray then slurpLen <= argSlurpLen else slurpLen == argSlurpLen
    = Just sub
    | reqLen <- length $ filter (\p -> not (isOptional p || isSlurpy p)) prms
    , optLen <- length $ filter (\p -> isOptional p) prms
    , hasArray <- isJust $ find (\p -> isSlurpy p && head (paramName p) /= '$') prms
    , argLen >= reqLen && (hasArray || argLen <= (reqLen + optLen))
    = Just sub
    | otherwise
    = Nothing

doFetch :: (Val -> Eval (IVar VScalar))
        -> (Val -> Eval Val)
        -> (forall v. (Value v) => Eval v)
        -> Bool
        -> Bool
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

