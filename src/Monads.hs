{-# OPTIONS_GHC -fglasgow-exts #-}

{-
    Monad structures.

    One Ring to rule them all,
    One Ring to find them,
    One Ring to bring them all
    and in the darkness bind them...
-}

module Monads where
import Internals
import AST
import Context

enterLex :: Pad -> Eval a -> Eval a
enterLex pad = local (\e -> e{ envLexical = (pad ++ envLexical e) })

enterContext :: Cxt -> Eval a -> Eval a
enterContext cxt = local (\e -> e{ envContext = cxt })

askDump str = do
    env <- asks envContext
    liftIO $ putStrLn $ "Current scope: " ++ str ++ " - Env: " ++ env


enterGiven topic action = enterLex [SymVar SMy "$_" topic] action

enterWhen break action = callCC $ \esc -> do
    env <- ask
    enterLex ((genSubs env "&continue" $ continueSub esc)
           ++ (genSubs env "&break" $ breakSub)) action
    where
    continueSub esc env = Sub
        { isMulti = True
        , subName = "continue"
        , subType = SubPrim
        , subPad = []
        , subAssoc = "pre"
        , subParams = makeParams env
        , subBindings = []
        , subReturns = envContext env
        , subFun = Prim (esc . head)
        }
    breakSub env = Sub
        { isMulti = True
        , subName = "break"
        , subType = SubPrim
        , subPad = []
        , subAssoc = "pre"
        , subParams = makeParams env
        , subBindings = []
        , subReturns = envContext env
        , subFun = break
        }

enterLoop action = callCC $ \esc -> do
    enterLex [SymVar SMy "&last" $ lastSub esc] action
    where
    lastSub esc = codeRef $ Sub
        { isMulti = False
        , subName = "last"
        , subType = SubPrim
        , subPad = []
        , subAssoc = "pre"
        , subParams = []
        , subBindings = []
        , subReturns = "Void"
        , subFun = Prim (const $ esc VUndef)
        }

enterBlock action = callCC $ \esc -> do
    env <- ask
    enterLex (genSubs env "&?BLOCK_EXIT" $ escSub esc) action
    where
    escSub esc env = Sub
        { isMulti = True
        , subName = "BLOCK_EXIT"
        , subType = SubPrim
        , subPad = []
        , subAssoc = "pre"
        , subParams = makeParams env
        , subBindings = []
        , subReturns = envContext env
        , subFun = Prim (esc . head)
        }
  
enterSub sub@Sub{ subType = typ } action
    | typ >= SubPrim = action -- primitives just happen
    | otherwise     = do
        env <- ask
        if typ >= SubBlock
            then local (fixEnv undefined env) action
            else resetT $ callCC $ \cc -> local (fixEnv cc env) action
    where
    doReturn [v] = shiftT $ const $ evalVal v
    doReturn _   = internalError "enterSub: doReturn list length /= 1"
    doCC cc [v] = cc =<< evalVal v
    doCC _  _   = internalError "enterSub: doCC list length /= 1"
    orig sub = sub { subBindings = [], subParams = (map fst (subBindings sub)) }
    subRec = [ SymVar SMy "&?SUB" (codeRef (orig sub))
             , SymVar SMy "$?SUBNAME" (scalarRef $ VStr $ subName sub)]
    blockRec = SymVar SMy "&?BLOCK" (codeRef (orig sub))
    fixEnv cc env@Env{ envLexical = pad } env'
        | typ >= SubBlock = env'{ envLexical = (blockRec:subPad sub) ++ pad }
        | otherwise      = env'{ envLexical = concat
            [ subRec
            , genSubs env "&return" retSub
            , genSubs env "&?CALLER_CONTINUATION" (ccSub cc)
            , subPad sub
            ] }
    retSub env = Sub
        { isMulti = True
        , subName = "return"
        , subType = SubPrim
        , subPad = []
        , subAssoc = "pre"
        , subParams = makeParams env
        , subBindings = []
        , subReturns = envContext env
        , subFun = Prim doReturn
        }
    ccSub cc env = Sub
        { isMulti = False
        , subName = "CALLER_CONTINUATION"
        , subType = SubPrim
        , subPad = []
        , subAssoc = "pre"
        , subParams = makeParams env
        , subBindings = []
        , subReturns = envContext env
        , subFun = Prim $ doCC cc
        }

genSubs env name gen =
    [ SymVar SMy name (codeRef $ gen env)
    , SymVar SMy name (codeRef $ gen env{ envContext = "Scalar" })
    , SymVar SMy name (codeRef $ gen env{ envContext = "List" })
    ]

makeParams Env{ envClasses = cls, envContext = cxt, envLValue = lv }
    = [ Param
        { isInvocant = False
        , isSlurpy = isList
        , isOptional = False
        , isNamed = False
        , isLValue = lv
        , isThunk = False
        , paramName = if isList then "@?0" else "$?0"
        , paramContext = cxt
        , paramDefault = Val VUndef
        } ]
    where
    isList = isaType cls "List" cxt
{-
enterSub sub = enterScope $ do
    local (\e -> e { envLexical = subPad sub }) $ do
        case subName sub of
            "inner" -> inner
            "sub3" -> sub3
-}

-- enter a lexical context

dumpLex :: String -> Eval ()
dumpLex label = do
    pad <- asks envLexical
    depth <- asks envDepth
    liftIO $ putStrLn ("("++(show depth)++")"++label ++ ": " ++ (show pad))
    return ()

callerCC :: Int -> Val -> Eval Val
callerCC n _ = do
    _ <- caller n
    -- (envCC env) v
    return undefined

caller :: Int -> Eval Env
caller n = do
    depth <- asks envDepth
    when (depth <= n) $
        fail "Cannot ask for deeper depth"
    asks $ foldl (.) id $ replicate n (fromJust . envCaller)

inner :: Eval Val
inner = do
    dumpLex ">inner"
    -- now try raising exceptions via multiple delimiting
    -- fail "foo"
    -- enterSub sub3Sub
    returnScope "aihsd"
    -- returnScope "foo"
    -- env <- caller 2
    -- throwErr $ ErrStr "test"
    -- (envShift env) $ \r -> return $ VStr "out1"

sub3 :: Eval Val
sub3 = do
    dumpLex ">sub3"
    -- now try raising exceptions via multiple delimiting
    -- fail "foo"
    callerReturn 1 (VStr "happy")
    -- returnScope "foo"
    -- env <- caller 2
    -- throwErr $ ErrStr "test"
    -- (envShift env) $ \r -> return $ VStr "out1"

{-
throwErr :: (MonadIO m) => VErr -> m a
throwErr = liftIO . throwIO . DynException . toDyn
-}


callerReturn :: Int -> Val -> Eval Val
callerReturn n v
    | n == 0 =  do
        shiftT $ \_ -> return v
    | otherwise = do
        env <- caller n
        shiftT $ \_ -> return $ VControl $ ControlLeave (return . (==) (envID env) . envID) v

returnScope = callerReturn 0 . VStr

evalVal x = do
    -- context casting, go!
    Env{ envLValue = isLValue, envClasses = cls, envContext = cxt } <- ask
    typ <- evalValType x
    val <- if isaType cls "Junction" typ then fromVal' x else return x
    let isCompatible = isaType cls cxt typ
        isListCxt    = isaType cls "List" cxt
        isRef        = case val of { VRef _ -> True; _ -> False }
    -- trace (show ((cxt, typ), isCompatible, isLValue, isListCxt, val)) return ()
    case (isCompatible, isLValue, isListCxt) of
        (True, True, _)         -> return val
        (True, False, False)    -> fromVal val
        (True, False, True)     -> return . VList =<< fromVal val
        (False, True, False)    -> return val -- auto scalar varify?
        (False, True, True)     -> return val -- auto list varify?
        (False, False, False)   -> if isRef then return val else fromVal' val
        (False, False, True)    -> return . VList =<< fromVal' val
