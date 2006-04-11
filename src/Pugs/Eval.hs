{-# OPTIONS_GHC -fglasgow-exts -cpp #-}
{-# OPTIONS_GHC -#include "../UnicodeC.h" #-}

{-|
    Evaluation and reduction engine.

>   Home is behind, the world ahead,
>   And there are many paths to tread
>   Through shadows to the edge of night,
>   Until the stars are all alight.
>   Then world behind and home ahead,
>   We'll wander back to home and bed...

This module takes an /abstract syntax tree/ and recursively evaluates it,
thereby evaluating the program.

The AST is represented as a hierarchy of nested 'Exp' expressions
(see "Pugs.AST").  Some understanding of 'Exp' and "Pugs.AST" in will help in
understanding this module.
-}

module Pugs.Eval (
    evaluate,
    emptyEnv, evaluateMain,
    enterLValue, enterRValue,
    runWarn
) where
import Pugs.Internals
import Prelude hiding ( exp )
import qualified Data.Map as Map

import Pugs.AST
import Pugs.Junc
import Pugs.Bind
import Pugs.Prim
import Pugs.Prim.List (op0Zip)
import Pugs.Monads
import Pugs.Pretty
import Pugs.Types
import Pugs.External
import Pugs.Eval.Var
import DrIFT.YAML ()

import RRegex.PCRE as PCRE

{-|
Construct a new, \'empty\' 'Env' (evaluation environment).

See the source of 'Pugs.Prims.initSyms' for a list of symbols that are
initially present in the global 'Pad'.
-}
emptyEnv :: (MonadIO m, MonadSTM m) 
         => String             -- ^ Name associated with the environment
         -> [STM PadMutator]   -- ^ List of 'Pad'-mutating transactions used
                               --     to declare an initial set of global
                               --     variables
         -> m Env
emptyEnv name genPad = liftSTM $ do
    pad  <- sequence genPad
    ref  <- newTVar Map.empty
    syms <- initSyms
    glob <- newTVar (combine (pad ++ syms) $ mkPad [])
    init <- newTVar $ MkInitDat { initPragmas=[] }
    return $ MkEnv
        { envContext = CxtVoid
        , envLexical = mkPad []
        , envImplicit= Map.empty
        , envLValue  = False
        , envGlobal  = glob
        , envPackage = "main"
        , envClasses = initTree
        , envEval    = evaluate
        , envCaller  = Nothing
        , envOuter   = Nothing
        , envDepth   = 0
        -- XXX see AST/Internals.hs
        --, envID      = uniq
        , envBody    = Val undef
        , envDebug   = Just ref -- Set to "Nothing" to disable debugging
        , envPos     = MkPos name 1 1 1 1
        , envPragmas = []
        , envInitDat = init
        }

-- Evaluation ---------------------------------------------------------------

{-|
Emits a runtime warning.
-}
-- XXX: This should cache so that you don't warn in the same place twice
--   (even though Perl 5 doesn't do this).
--   It should also respond to lexical warnings pragmata.
runWarn :: String -> Eval ()
runWarn msg = do 
    enterEvalContext CxtVoid $
        App (Var "&warn") Nothing [Val (VStr msg)]
    return ()

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
    initAV   <- reduceVar "@*INIT"
    initSubs <- fromVals initAV
    enterContext CxtVoid $ do
        mapM_ evalExp [ App (Val sub) Nothing [] | sub <- initSubs ]
    -- The main runtime
    val      <- resetT $ evaluate exp
    -- S04: END {...}       at run time, ALAP
    endAV    <- reduceVar "@*END"
    endSubs  <- fromVals endAV
    enterContext CxtVoid $ do
        mapM_ evalExp [ App (Val sub) Nothing [] | sub <- endSubs ]
    liftIO $ performGC
    return val

{-|
Evaluate an expression.

This function mostly just delegates to 'reduce'.
-}
evaluate :: Exp -- ^ The expression to evaluate
         -> Eval Val
evaluate (Val val) = evalVal val
evaluate exp = do
    want <- asks envWant
    debug "indent" ('-':) (" Evl [" ++ want ++ "]:\n") exp
    val <- local (\e -> e{ envBody = exp }) $ reduce exp
    debug "indent" (tail) "- Ret: " val
    trapVal val (return val)

-- Reduction ---------------------------------------------------------------

retVal :: Val -> Eval Val
retVal val = evaluate (Val val)

retItem :: Val -> Eval Val
retItem val = do
    ifListContext
        (retVal $ VList [val])
        (retVal $ val)

{-|
Add a symbol to the global 'Pad'.

Used by 'reduceSym'.
-}
addGlobalSym :: PadMutator -- ^ 'Pad'-transformer that will insert the new
                           --     symbol
             -> Eval ()
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

evalRef :: VRef -> Eval Val
evalRef ref = do
    if refType ref == (mkType "Thunk") then forceRef ref else do
    val <- callCC $ \esc -> do
        MkEnv{ envContext = cxt, envLValue = lv, envClasses = cls } <- ask
        let typ = typeOfCxt cxt
            isCollectionRef = isaType cls "List" (refType ref)
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

{-|
Reduce an expression into its value.

This function dispatches to the relevant @reduce*@ function for the type of
expression given.
-}
reduce :: Exp -- ^ The expression to reduce
       -> Eval Val

reduce (Val v) = reduceVal v

reduce (Var name) = reduceVar name

reduce (Stmts this rest) = reduceStmts this rest

reduce (Ann (Prag prag) exp) = reducePrag prag exp

reduce (Ann (Pos pos) exp) = reducePos pos exp

reduce (Pad scope lexEnv exp) = reducePad scope lexEnv exp

reduce (Sym scope name exp) = reduceSym scope name exp

reduce (Ann (Cxt cxt) exp) = reduceCxt cxt exp

-- Reduction for no-operations
reduce Noop = retEmpty

reduce (Syn name subExps) = reduceSyn name subExps

reduce (App subExp inv args) = reduceApp subExp inv args

reduce exp = retError "Invalid expression" exp

reduceVal :: Val -> Eval Val
-- Reduction for mutables
reduceVal v@(VRef var) = do
    lv <- asks envLValue
    if lv then retVal v else do
    rv <- readRef var
    retVal rv

-- Reduction for constants
reduceVal v = retVal v

-- Reduction for variables
reduceVar :: Var -> Eval Val
reduceVar (sigil:'.':name) = enterContext (cxtOfSigil sigil) $
    reduceSyn "{}" [Var "$?SELF", Val (VStr name)]
reduceVar (sigil:'!':name@(_:_)) = enterContext (cxtOfSigil sigil) $
    reduceSyn "{}" [Var "$?SELF", Val (VStr name)]
reduceVar name = do
    let cxt = case name of
            ('$':_) -> enterContext (CxtItem $ MkType "Scalar")
            _       -> id
    v <- findVar name
    case v of
        Just var -> cxt $ evalRef var
        _ -> case name of
            (':':rest)  -> return $ VType (mkType rest)
            (_:'*':_)   -> evalExp (Sym SGlobal name (Var name))
            _           -> case isQualified name of
                Just _  -> evalExp (Sym SGlobal name (Var name))
                _       -> retError "Undeclared variable" name

reduceStmts :: Exp -> Exp -> Eval Val
reduceStmts this rest
    | Noop <- unwrap rest = reduce this
    | Noop <- unwrap this = reduce rest

reduceStmts this rest = do
    val <- enterContext cxtVoid $ reduce this
    trapVal val $ case unwrap rest of
        (Syn "env" []) -> do
            env <- ask
            writeVar "$*_" val
            return . VControl $ ControlEnv env
        _ -> reduce rest

reducePrag :: [Pragma] -> Exp -> Eval Val
reducePrag prag exp = do
    local (\e -> e{ envPragmas = prag }) $ do
        evalExp exp

{-|
Reduce a 'Pos' expression by reducing its subexpression in a new 'Env', which
holds the 'Pos'\'s position.
-}
reducePos :: Pos -> Exp -> Eval Val
reducePos pos exp = do
    local (\e -> e{ envPos = pos }) $ do
        evalExp exp

reducePad :: Scope -> Pad -> Exp -> Eval Val
reducePad SEnv lex@(MkPad lex') exp = do
    local (\e -> e{ envImplicit = Map.map (const ()) lex' `Map.union` envImplicit e }) $
        reducePad SMy lex exp
reducePad SMy lex exp = do
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

reducePad STemp lex exp = do
    tmps <- mapM (\(sym, _) -> evalExp $ App (Var "&TEMP") (Just $ Var sym) []) $ padToList lex
    -- default to nonlocal exit
    isNonLocal  <- liftSTM $ newTVar True
    val <- resetT $ do
        val' <- evalExp exp
        -- exp evaluated without error; no need to shift out
        liftSTM $ writeTVar isNonLocal False
        return val'
    mapM_ (\tmp -> evalExp $ App (Val tmp) Nothing []) tmps
    isn <- liftSTM $ readTVar isNonLocal
    (if isn then (shiftT . const) else id) (return val)

reducePad _ lex exp = do
    local (\e -> e{ envLexical = lex `unionPads` envLexical e }) $ do
        evalExp exp
        
reduceSym :: Scope -> String -> Exp -> Eval Val
-- Special case: my (undef) is no-op
reduceSym _ "" exp = evalExp exp

reduceSym scope name exp | scope <= SMy = do
    ref <- newObject (typeOfSigil $ head name)
    let (gen, name') = case name of
            ('&':n@('&':_)) -> (genMultiSym, n)
            _               -> (genSym, name)
    sym <- gen name' ref
    enterLex [ sym ] $ evalExp exp

reduceSym _ name exp = do
    ref     <- newObject (typeOfSigil $ head name)
    let (gen, name') = case name of
            ('&':n@('&':_)) -> (genMultiSym, n)
            _               -> (genSym, name)
    qn      <- toQualified name'
    sym     <- gen qn ref
    addGlobalSym sym
    evalExp exp

-- Context forcing
reduceCxt :: Cxt -> Exp -> Eval Val
reduceCxt cxt exp = do
    val <- enterEvalContext cxt exp
    enterEvalContext cxt (Val val) -- force casting

{-|
Reduce a 'Syn' expression, i.e. a syntactic construct that cannot (yet) be
expressed using 'App' (regular sub application).

Theoretically, 'Syn' will one day be deprecated when 'App' becomes powerful
enough to make it redundant.
-}
reduceSyn :: String -> [Exp] -> Eval Val

reduceSyn "env" [] = do
    env <- ask
    -- writeVar "$*_" val
    return . VControl $ ControlEnv env

reduceSyn "block" [Ann _ (Syn "sub" [Val (VCode sub)])]
    | subType sub == SubBlock, isEmptyParams (subParams sub) =
    enterBlock $ reduce (subBody sub)

reduceSyn "block" [Syn "sub" [Val (VCode sub)]]
    | subType sub == SubBlock, isEmptyParams (subParams sub) =
    enterBlock $ reduce (subBody sub)

reduceSyn "block" [body] = do
    enterBlock $ reduce body
    
reduceSyn "sub" [exp] = do
    (VCode sub) <- enterEvalContext (cxtItem "Code") exp
    env  <- ask
    cont <- if subType sub /= SubCoroutine then return Nothing else liftSTM $ do
        tvar <- newTVar (error "empty sub")
        let thunk = (`MkThunk` anyType) . fix $ \redo -> do
            evalExp $ subBody sub
            liftSTM $ writeTVar tvar thunk
            redo
        writeTVar tvar thunk
        return $ Just tvar
    retVal $ VCode sub
        { subEnv  = Just env
        , subCont = cont
        }

reduceSyn "but" [obj, block] = do
    evalExp $ App (Var "&Pugs::Internals::but_block") Nothing [obj, block]

reduceSyn name [cond, bodyIf, bodyElse]
    | "if"     <- name = doCond id
    | "unless" <- name = doCond not
    where
    doCond :: (Bool -> Bool) -> Eval Val
    doCond f = do
        vbool     <- enterEvalContext (cxtItem "Bool") cond
        vb        <- fromVal vbool
        if (f vb)
            then reduce bodyIf
            else reduce bodyElse

reduceSyn "for" [list, body] = do
    av    <- enterLValue $ enterEvalContext cxtSlurpyAny list
    vsub  <- enterRValue $ enterEvalContext (cxtItem "Code") body
    -- XXX this is wrong -- should use Array.next
    sz    <- join $ doArray av array_fetchSize
    fetch <- doArray av array_fetchElem
    elms  <- mapM fetch [0..sz-1]
    sub' <- fromVal vsub
    sub  <- case sub' of
        VCode s -> return s
        VList [] -> fail $ "Invalid codeblock for 'for': did you mean {;}?"
        _ -> fail $ "Invalid codeblock for 'for'"
    -- XXX: need clarification -- this makes
    --      for @x { ... } into for @x -> $_ {...}
    let arity = max 1 $ length (subParams sub)
        runBody [] _ = retVal undef
        runBody vs sub' = do
            let (these, rest) = arity `splitAt` vs
            callCC $ \esc -> genSymPrim "&redo" (const $ (runBody vs sub') >>= esc) $  \symRedo -> do
                genSymCC "&next" $ \symNext -> do
                    apply (updateSubPad sub' (symRedo . symNext)) Nothing $
                        map (Val . VRef . MkRef) these
                runBody rest sub'
    genSymCC "&last" $ \symLast -> do
        let munge sub | subParams sub == [defaultArrayParam] =
                munge sub{ subParams = [defaultScalarParam] }
            munge sub = updateSubPad sub symLast
        runBody elms $ munge sub

reduceSyn "gather" [exp] = do
    sub     <- fromVal =<< evalExp exp
    av      <- newArray []
    symTake <- genSym "@?TAKE" (MkRef av)
    apply (updateSubPad sub symTake) Nothing []
    fmap VList $ readIVar av

reduceSyn "loop" exps = do
    let [pre, cond, post, body] = case exps of { [_] -> exps'; _ -> exps }
        exps' = [emptyExp, Val (VBool True), emptyExp] ++ exps
        evalCond | unwrap cond == Noop = return True
                 | otherwise = fromVal =<< enterEvalContext (cxtItem "Bool") cond
    evalExp pre
    vb  <- evalCond
    if not vb then retEmpty else do
    genSymCC "&last" $ \symLast -> enterLex [symLast] $ fix $ \runBody -> do
        -- genSymPrim "&redo" (const $ runBody) $ \symRedo -> do
        callCC $ \esc -> genSymPrim "&redo" (const $ runBody >>= esc) $ \symRedo -> do
        let runNext = do
            valPost <- evalExp post
            vb      <- evalCond
            trapVal valPost $ if vb then runBody else retEmpty
        callCC $ \esc -> genSymPrim "&next" (const $ runNext >>= esc) $ \symNext -> do
            valBody <- enterLex [symRedo, symNext] $ evalExp body
            trapVal valBody $ runNext

reduceSyn "given" [topic, body] = do
    vtopic <- fromVal =<< enterLValue (enterEvalContext cxtItemAny topic)
    enterGiven vtopic $ enterEvalContext (cxtItem "Code") body

reduceSyn "when" [match, body] = do
    break  <- reduceVar "&?BLOCK_EXIT"
    vbreak <- fromVal break
    result <- reduce $ case unwrap match of
        App _ (Just (Var "$_")) _ -> match
        _ -> App (Var "&*infix:~~") Nothing [(Var "$_"), match]
    rb     <- fromVal result
    if rb
        then enterWhen (subBody vbreak) $ apply vbreak Nothing [body]
        else retVal undef

reduceSyn "default" [body] = do
    break  <- reduceVar "&?BLOCK_EXIT"
    vbreak <- fromVal break
    enterWhen (subBody vbreak) $ apply vbreak Nothing [body]

reduceSyn name [cond, body]
    | "while" <- name = doWhileUntil id False
    | "until" <- name = doWhileUntil not False
    | "postwhile" <- name = doWhileUntil id True
    | "postuntil" <- name = doWhileUntil not True
    where
    -- XXX This treatment of while/until loops probably needs work
    doWhileUntil :: (Bool -> Bool) -> Bool -> Eval Val
    doWhileUntil f postloop = do
        -- XXX redo for initial run
        if postloop
            then reduce body
            else retEmpty
        enterWhile . fix $ \runLoop -> do
            vbool <- enterEvalContext (cxtItem "Bool") cond
            vb    <- fromVal vbool
            case f vb of
                True -> fix $ \runBody -> do
                    -- genSymPrim "&redo" (const $ runBody) $ \symRedo -> do
                    callCC $ \esc -> genSymPrim "&redo" (const $ runBody >>= esc) $  \symRedo -> do
                    rv <- enterLex [symRedo] $ reduce body
                    case rv of
                        VError _ _  -> retVal rv
                        _           -> runLoop
                _ -> retVal vbool

reduceSyn "=" [lhs, rhs] = do
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

reduceSyn "::=" exps = reduce (Syn ":=" exps)

reduceSyn ":=" exps
    | [Syn "," vars, Syn "," vexps] <- unwrap exps = do
        when (length vars > length vexps) $ do
            fail $ "Wrong number of binding parameters: "
                ++ (show $ length vexps) ++ " actual, "
                ++ (show $ length vars) ++ " expected"
        -- env' <- cloneEnv env -- FULL THUNKING
        names <- forM vars $ \var -> case unwrap var of
            Var name -> return name
            Syn [sigil,':',':','(',')'] [vexp]
                | Val (VStr name) <- unwrap vexp -> return (sigil:name)
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
                _ -> retError "Bind to undeclared variable" name
        forM_ bindings $ \(ioRef, ref) -> do
            liftSTM $ writeTVar ioRef ref
        return $ case map (VRef . snd) bindings of
            [v] -> v
            vs  -> VList vs

reduceSyn ":=" [var, vexp] = do
    let expand e | e'@(Syn "," _) <- unwrap e = e'
        expand e = Syn "," [e]
    reduce (Syn ":=" [expand var, expand vexp])

reduceSyn "*" exps = do
    let [exp] = exps
    val <- enterRValue $ enterEvalContext cxtSlurpyAny exp
    return . VList =<< fromVal val
    -- vals <- fromVals val
    -- return $ VList $ concat vals

reduceSyn "," exps = do
    vals <- mapM (enterEvalContext cxtSlurpyAny) exps
    retVal . VList . concat $ map castList vals
    where
    castList (VList vs) = vs
    castList v = [v]

reduceSyn "val" [exp] = do
    enterRValue $ evalExp exp

reduceSyn "\\{}" [exp] = do
    v   <- enterRValue $ enterEvalContext cxtSlurpyAny exp
    hv  <- newObject (MkType "Hash")
    writeRef hv v
    retVal $ VRef hv

reduceSyn "\\[]" [exp] = do
    v   <- enterRValue $ enterEvalContext cxtSlurpyAny exp
    av  <- newObject (MkType "Array")
    writeRef av v
    retItem $ VRef av

reduceSyn "[]" exps
    -- XXX evil hack for infinite slices
    | [lhs, App (Var "&postfix:...") invs args] <- unwrap exps
    , [idx] <- maybeToList invs ++ args
--  , not (envLValue env)
    = reduce (Syn "[...]" [lhs, idx])
    | [lhs, App (Var "&infix:..") invs args] <- unwrap exps
    , [idx, Val (VNum n)] <- maybeToList invs ++ args
    , n == 1/0
--  , not (envLValue env)
    = reduce (Syn "[...]" [lhs, idx])
    | otherwise = do
        let [listExp, indexExp] = exps
        varVal  <- enterLValue $ enterEvalContext (cxtItem "Array") listExp
        idxCxt  <- inferExpCxt indexExp 
        {- if envLValue env
            then inferExpCxt indexExp else return (envContext env)
        -}
        idxVal  <- enterRValue $ enterEvalContext idxCxt indexExp
        lv      <- asks envLValue
        doFetch (mkFetch $ doArray varVal array_fetchElem)
                (mkFetch $ doArray varVal array_fetchVal)
                (fromVal idxVal) lv (not (isSlurpyCxt idxCxt))

reduceSyn "[...]" [listExp, indexExp] = do
    idxVal  <- enterRValue $ enterEvalContext (cxtItem "Int") indexExp
    idx     <- fromVal idxVal
    listVal <- enterRValue $ enterEvalContext (cxtItem "Array") listExp
    list    <- fromVal listVal
    -- error $ show list
    -- elms    <- mapM fromVal list -- flatten
    retVal $ VList (drop idx $ list)

reduceSyn "@{}" [exp] = do
    val     <- enterEvalContext (cxtItem "Array") exp
    ivar    <- doArray val IArray
    evalRef (MkRef ivar)

reduceSyn "%{}" [exp] = do
    val     <- enterEvalContext (cxtItem "Hash") exp
    ivar    <- doHash val IHash
    evalRef (MkRef ivar)

reduceSyn "&{}" [exp] = do
    val     <- enterEvalContext (cxtItem "Code") exp
    sub     <- fromVal val
    return $ VCode sub

reduceSyn "${}" [exp] = do
    val     <- enterEvalContext (cxtItem "Scalar") exp
    ref     <- fromVal val
    evalRef ref

reduceSyn (sigil:"::()") exps = do
    -- These are all parts of the name
    parts   <- mapM fromVal =<< mapM evalExp exps
    -- Now we only have to add the sigil in front of the string and join
    -- the parts with "::".
    let varname = sigil:(concat . (intersperse "::") $ parts)
    -- Finally, eval the varname.
    evalExp . Var $ varname

reduceSyn "{}" [listExp, indexExp] = do
    varVal  <- enterLValue $ enterEvalContext (cxtItem "Hash") listExp
    idxCxt  <- inferExpCxt indexExp 
    {- if envLValue env
        then inferExpCxt indexExp else return (envContext env)
    -}
    idxVal  <- enterRValue $ enterEvalContext idxCxt indexExp
    lv      <- asks envLValue
    doFetch (mkFetch $ doHash varVal hash_fetchElem)
            (mkFetch $ doHash varVal hash_fetchVal)
            (fromVal idxVal) lv (not (isSlurpyCxt idxCxt))

reduceSyn "rx" [exp, adverbs] = do
    hv      <- fromVal =<< evalExp adverbs
    val     <- enterEvalContext (cxtItem "Str") exp
    str     <- fromVal val
    p5      <- fromAdverb hv ["P5", "Perl5", "perl5"]
    p5flags <- fromAdverb hv ["P5", "Perl5", "perl5"]
    flag_g  <- fromAdverb hv ["g", "global"]
    flag_i  <- fromAdverb hv ["i", "ignorecase"]
    flag_w  <- fromAdverb hv ["w", "words"]
    flag_s  <- fromAdverb hv ["stringify"] -- XXX hack
    adverbHash <- reduce adverbs
    -- XXX - this fix for :global PCRE rules awaits someone with
    --  the Haskell'Fu to write the ns line below.
    -- let rx | p5 = MkRulePCRE p5re g ns flag_s str adverbHash
    let rx | p5 = MkRulePCRE p5re g 1 flag_s str adverbHash
           | otherwise = MkRulePGE p6re g flag_s adverbHash
        g = ('g' `elem` p5flags || flag_g)
        p5re = mkRegexWithPCRE (encodeUTF8 str) $
                    [ pcreUtf8
                    , ('i' `elem` p5flags || flag_i) `implies` pcreCaseless
                    , ('m' `elem` p5flags) `implies` pcreMultiline
                    , ('s' `elem` p5flags) `implies` pcreDotall
                    , ('x' `elem` p5flags) `implies` pcreExtended
                    ]
        p6re = if not flag_w then str
               else case str of
                      ':':_ -> ":w"   ++ str
                      _     -> ":w::" ++ str
        -- ns <- liftIO $ PCRE.numSubs p5re
    retVal $ VRule rx
    where
    implies True  = id
    implies False = const 0
    fromAdverb _ [] = fromVal undef
    fromAdverb hv (k:ks) = case lookup k hv of
        Just v  -> fromVal v
        Nothing -> fromAdverb hv ks

reduceSyn "//" exps = reduce (Syn "rx" exps)

reduceSyn "match" exps = reduce (Syn "rx" exps) -- XXX - this is wrong

reduceSyn "subst" [exp, subst, adverbs] = do
    (VRule rx)  <- reduce (Syn "rx" [exp, adverbs])
    retVal $ VSubst (rx, subst)

reduceSyn "is" _ = do
    retEmpty

reduceSyn "package" [kind, exp] = reduceSyn "namespace" [kind, exp, emptyExp]

reduceSyn "namespace" [_kind, exp, body] = do
    val <- evalExp exp
    str <- fromVal val
    when (str `elem` words "MY OUR OUTER CALLER") $ do
        fail $ "Cannot use " ++ str ++ " as a namespace"
    enterPackage str $ evalExp body

reduceSyn "inline" [langExp, _] = do
    langVal <- evalExp langExp
    lang    <- fromVal langVal
    when (lang /= "Haskell") $
        retError "Inline: Unknown language" langVal
    pkg     <- asks envPackage -- full module name here
#ifndef HADDOCK
    let file = (`concatMap` pkg) $ \v -> case v of
        { '-' -> "__"; _ | isAlphaNum v -> [v] ; _ -> "_" }
#endif
    externRequire "Haskell" (file ++ ".o")
    retEmpty

reduceSyn "=>" [keyExp, valExp] = do
    key <- enterEvalContext cxtItemAny keyExp
    val <- enterEvalContext cxtItemAny valExp
    retItem $ castV (key, val)

reduceSyn syn [lhs, exp]
    | last syn == '=' = do
        let op = "&infix:" ++ init syn
        evalExp $ Syn "=" [lhs, App (Var op) Nothing [lhs, exp]]

reduceSyn "q:code" [ body ] = expToEvalVal body

reduceSyn name exps =
    retError "Unknown syntactic construct" (Syn name exps)

reduceApp :: Exp -> (Maybe Exp) -> [Exp] -> Eval Val
-- XXX absolutely evil bloody hack for context hinters
reduceApp (Var "&hash") invs args = do
    enterEvalContext cxtItemAny $ Syn "\\{}" [Syn "," $ maybeToList invs ++ args]

reduceApp (Var "&list") invs args =
    enterEvalContext cxtSlurpyAny $ case maybeToList invs ++ args of
        []    -> Val (VList [])
        [exp] -> exp
        exps  -> Syn "," exps

reduceApp (Var "&item") invs args
    | [exp] <- maybeToList invs ++ args = enterEvalContext cxtItemAny exp
    | otherwise = enterEvalContext cxtItemAny $ Syn "," (maybeToList invs ++ args)

-- XXX absolutely evil bloody hack for "zip"
reduceApp (Var "&zip") Nothing args = do
    vals <- mapM (enterRValue . enterEvalContext (cxtItem "Array")) args
    val  <- op0Zip vals
    retVal val

-- XXX absolutely evil bloody hack for "return"
reduceApp (Var "&return") Nothing [] = op1Return $ shiftT . const $ retEmpty
reduceApp (Var "&return") (Just inv) [] = op1Return $ shiftT . const $ evalExp inv
reduceApp (Var "&return") Nothing [arg] = op1Return $ shiftT . const $ evalExp arg
reduceApp (Var "&return") invs args = op1Return $ shiftT . const . evalExp $
    Syn "," (maybeToList invs ++ args)

-- XXX absolutely evil bloody hack for "not"
reduceApp (Var "&not") Nothing [] = retEmpty

reduceApp (Var "&not") invs args = do
    bool <- fromVal =<< evalExp (last $ maybeToList invs ++ args)
    retVal $ VBool (not bool)

-- XXX absolutely evil bloody hack for "goto"
reduceApp (Var "&goto") (Just subExp) args = do
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
           , envPos     = envPos caller
           }

-- XXX absolutely evil bloody hack for "assuming"
reduceApp (Var "&assuming") (Just subExp) args = do
    vsub <- enterEvalContext (cxtItem "Code") subExp
    sub  <- fromVal vsub
    case bindSomeParams sub Nothing args of
        Left errMsg      -> fail errMsg
        Right curriedSub -> retVal $ castV $ curriedSub

reduceApp (Var "&infix:=>") invs args = do
    reduceSyn "=>" $ maybeToList invs ++ args

reduceApp (Var name@('&':_)) invs args = do
    sub     <- findSub name invs args
    case sub of
        Right sub    -> applySub sub invs args
        _ | [Syn "," args'] <- unwrap args -> do
            sub <- findSub name invs args'
            either err (fail errSpcMessage) sub
        Left failure -> err failure
    where
    errSpcMessage = "Extra space found after " ++ name ++ " (...) -- did you mean " ++ name ++ "(...) instead?"
    err NoMatchingMulti    = retError "No compatible subrountine found" name
    err NoSuchSub          = retError "No such sub" name
    err (NoSuchMethod cls) = retError ("No such method in class " ++ cls) name
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
        , (App _ Nothing _):_                <- args
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
            Right sub'    -> applyChainSub sub args sub' args' rest
            Left _        -> apply sub{ subParams = (length args) `replicate` p } Nothing args -- XXX Wrong
    applyChainSub :: VCode -> [Exp] -> VCode -> [Exp] -> [Exp] -> Eval Val
    applyChainSub sub args sub' args' rest
        | MkCode{ subAssoc = "chain", subBody = fun, subParams = prm }   <- sub
        , MkCode{ subAssoc = "chain", subBody = fun', subParams = prm' } <- sub'
        = applySub sub{ subParams = prm ++ tail prm', subBody = Prim $ chainFun prm' fun' prm fun } Nothing (args' ++ rest)
        | MkCode{ subAssoc = "chain", subParams = (p:_) }   <- sub
        = apply sub{ subParams = (length args) `replicate` p } Nothing args -- XXX Wrong
        | otherwise
        = internalError "applyChainsub did not match a chain subroutine"

reduceApp subExp invs args = do
    vsub <- enterEvalContext (cxtItem "Code") subExp
    (`juncApply` [ApplyArg "" vsub False]) $ \[arg] -> do
        sub  <- fromVal $ argValue arg
        apply sub invs args


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
applyExp styp bound body = do
    let invocant         = head bound
    let (attrib, normal) = partition isAttrib bound
    sequence_ [ evalExp (Syn "=" [Syn "{}" [Val (argValue invocant), Val (VStr key)], Val val]) |
        ApplyArg{ argName = (_:_:key), argValue = val } <- attrib ]
    -- typ <- inferExpType body
    ret <- applyThunk styp normal $ MkThunk (evalExp body) anyType
    return ret

isAttrib :: ApplyArg -> Bool
isAttrib ApplyArg{ argName = (_:'.':_) } = True
isAttrib ApplyArg{ argName = (_:':':_) } = True
isAttrib _ = False

applyThunk :: SubType -> [ApplyArg] -> VThunk -> Eval Val
applyThunk _ [] thunk = thunk_force thunk
applyThunk styp bound@(arg:_) thunk = do
    -- introduce $?SELF and $_ as the first invocant.
    inv     <- case styp of
        SubPointy               -> aliased ["$_"]
        _ | styp <= SubMethod   -> aliased ["$?SELF", "$_"]
        _                       -> return []
    pad <- formal
    enterLex (inv ++ pad) $ thunk_force thunk
    where
    formal = mapM argNameValue $ filter (not . null . argName) bound
    aliased names = do
        argRef  <- fromVal (argValue arg)
        mapM (`genSym` argRef) names
    argNameValue (ApplyArg name val _) = genSym name =<< fromVal val

{-|
Apply a sub (or other code object) to an (optional) invocant, and
a list of arguments.

Mostly delegates to 'doApply' after explicitly retrieving the local 'Env'.
-}
apply :: VCode       -- ^ The sub to apply
      -> (Maybe Exp) -- ^ Explicit invocant
      -> [Exp]       -- ^ List of arguments (not including explicit invocant)
      -> Eval Val
apply sub invs args = do
    env <- ask
    doApply env sub invs args

-- XXX not entirely sure how this evaluation should proceed
reduceNamedArg :: Exp -> Eval Exp
reduceNamedArg (Syn "named" [keyExp, val]) = do
    key    <- fmap VStr $ fromVal =<< enterEvalContext cxtItemAny keyExp
    return $ Syn "named" [Val key, val]
reduceNamedArg other = return other

        

-- XXX - faking application of lexical contexts
-- XXX - what about defaulting that depends on a junction?
{-|
Apply a sub (or other code object) to an (optional) invocants, and a list of
arguments, in the specified environment.
-}
doApply :: Env         -- ^ Environment to evaluate in
        -> VCode       -- ^ The sub to apply
        -> (Maybe Exp) -- ^ Explicit invocant
        -> [Exp]       -- ^ List of arguments (not including explicit invocant)
        -> Eval Val
doApply env sub@MkCode{ subCont = cont, subBody = fun, subType = typ } invs args = do
    realInvs <- fmapM reduceNamedArg invs
    realArgs <-  mapM reduceNamedArg args  
    case bindParams sub realInvs realArgs of
        Left errMsg -> fail errMsg
        Right sub   -> do
            forM_ (subSlurpLimit sub) $ \limit@(n, _) -> do
                extra <- checkSlurpyLimit limit
                unless (null extra) $ do
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
    fixSub MkCode{ subType = SubPrim } env = env
    fixSub sub env = env
        { envLexical = subPad sub
        , envPackage = maybe (envPackage env) envPackage (subEnv sub)
        , envOuter   = maybe Nothing envOuter (subEnv sub)
        }
    fixEnv :: Env -> Env
    fixEnv | typ >= SubBlock = id
           | otherwise       = envEnterCaller
    doBind :: [PadMutator] -> [(Param, Exp)] -> Eval ([PadMutator], [ApplyArg])
    doBind syms [] = return (syms, [])
    doBind syms ((prm, exp):rest) = do
        -- trace ("<== " ++ (show (prm, exp))) $ return ()
        let name = paramName prm
            cxt = cxtOfSigil $ head name
        (val, coll) <- enterContext cxt $ case exp of
            Syn "param-default" [exp, Val (VCode sub)] -> do
                local (fixSub sub . fixEnv) $ enterLex syms $ expToVal prm exp
            _  -> expToVal prm exp
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
            thunkify = do
                -- typ <- inferExpType exp
                return . VRef . thunkRef $ MkThunk eval (anyType)
        val <- if thunk then thunkify else do
            v   <- eval
            typ <- evalValType v
            let cls = envClasses env
            if isaType cls "Junction" typ then return v else do
            case (lv, rw) of
                (True, True)    -> return v
                (True, False)   -> do
                    --- not scalarRef! -- use the new "transparent IType" thing!
                    case showType (typeOfSigil $ head name) of
                        "Hash"  -> fmap (VRef . hashRef) (fromVal v :: Eval VHash)
                        "Array" -> fmap (VRef . arrayRef) (fromVal v :: Eval VArray)
                        _       -> case v of
                            VRef (MkRef (IScalar _)) -> return (VRef $ scalarRef v) 
                            VRef _ -> return v -- XXX - preserving ref
                            _ -> return (VRef $ scalarRef v) 
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

