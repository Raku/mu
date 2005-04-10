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
import Types

enterLex :: Pad -> Eval a -> Eval a
enterLex pad = local (\e -> e{ envLexical = (pad ++ envLexical e) })

enterContext :: Cxt -> Eval a -> Eval a
enterContext cxt = local (\e -> e{ envContext = cxt })

askDump str = do
    env <- asks envContext
    liftIO $ putStrLn $ "Current scope: " ++ str ++ " - Env: " ++ env


{-
enterScope f = do
    uniq <- liftIO $ newUnique
    rv <- callCC $ \cc -> resetT $ do
        local (\e -> e{ envCaller = Just e, envDepth = 1 + envDepth e, envID = uniq } ) f
    liftIO $ print (rv)
    -- here we trigger error handler of various sorts
    case rv of
        VControl (ControlLeave f val) -> do
            env <- ask
            match <- f env
            if match
                then callerReturn 0 val
                else return rv
        _ -> return rv
    {-
    -- detect for abnormal return
    return rv
    -}
-}

enterGiven topic action = enterLex [SymVar SMy "$_" topic] action

enterWhen break action = callCC $ \esc -> do
    enterLex [SymVar SMy "&continue" $ continueSub esc,
              SymVar SMy "&break" break] action
    where
    continueSub esc = codeRef $ Sub
        { isMulti = False
        , subName = "continue"
        , subType = SubPrim
        , subPad = []
        , subAssoc = "pre"
        , subParams = []
        , subBindings = []
        , subReturns = "Void"
        , subFun = Prim (const $ esc VUndef)
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
    enterLex [SymVar SMy "&?BLOCK_EXIT" $ escSub esc] action
    where
    escSub esc = codeRef $ Sub
        { isMulti = False
        , subName = "&?BLOCK_EXIT"
        , subType = SubPrim
        , subPad = []
        , subAssoc = "pre"
        , subParams = []
        , subBindings = []
        , subReturns = "Void"
        , subFun = Prim (const $ esc VUndef)
        }
  
enterSub sub@Sub{ subType = typ } action
    | typ >= SubPrim = action -- primitives just happen
    | otherwise     = do
        cxt <- asks envContext
        pad <- asks envLexical
        if typ >= SubBlock
            then local (fixEnv undefined pad cxt) action
            else resetT $ callCC $ \cc -> local (fixEnv cc pad cxt) action
    where
    doReturn [v] = shiftT $ const (return v)
    doReturn _   = internalError "enterSub: doReturn list length /= 1"
    doCC cc [v] = cc v
    doCC _  _   = internalError "enterSub: doCC list length /= 1"
    orig sub = sub { subBindings = [], subParams = (map fst (subBindings sub)) }
    subRec = [ SymVar SMy "&?SUB" (codeRef (orig sub))
             , SymVar SMy "$?SUBNAME" (scalarRef $ VStr $ subName sub)]
    blockRec = SymVar SMy "&?BLOCK" (codeRef (orig sub))
    ret cxt = SymVar SMy "&return" (codeRef $ retSub cxt)
    callerCC cc cxt = SymVar SMy "&?CALLER_CONTINUATION" (codeRef $ ccSub cc cxt)
    fixEnv cc pad cxt env
        | typ >= SubBlock = env{ envLexical = (blockRec:subPad sub) ++ pad }
        | otherwise      = env{ envLexical = subRec ++ (ret cxt:callerCC cc cxt:subPad sub) }
    retSub cxt = Sub
        { isMulti = False
        , subName = "return"
        , subType = SubPrim
        , subPad = []
        , subAssoc = "pre"
        , subParams = [ Param
            { isInvocant = False
            , isSlurpy = True
            , isOptional = False
            , isNamed = False
            , isLValue = False
            , isThunk = False
            , paramName = "@?0"
            , paramContext = cxt
            , paramDefault = Val VUndef
            } ]
        , subBindings = []
        , subReturns = cxt
        , subFun = Prim doReturn
        }
    ccSub cc cxt = Sub
        { isMulti = False
        , subName = "CALLER_CONTINUATION"
        , subType = SubPrim
        , subPad = []
        , subAssoc = "pre"
        , subParams = [ Param
            { isInvocant = False
            , isSlurpy = True
            , isOptional = False
            , isNamed = False
            , isLValue = False
            , isThunk = False
            , paramName = "@?0"
            , paramContext = cxt
            , paramDefault = Val VUndef
            } ]
        , subBindings = []
        , subReturns = cxt
        , subFun = Prim $ doCC cc
        }

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

