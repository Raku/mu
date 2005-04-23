{-# OPTIONS_GHC -fglasgow-exts #-}

{-
    Monad structures.

    One Ring to rule them all,
    One Ring to find them,
    One Ring to bring them all
    and in the darkness bind them...
-}

module Pugs.Monads where
import Pugs.Internals
import Pugs.AST
import Pugs.Context
import Pugs.Types

enterLex :: Pad -> Eval a -> Eval a
enterLex pad = local (\e -> e{ envLexical = (pad ++ envLexical e) })

enterContext :: Cxt -> Eval a -> Eval a
enterContext cxt = local (\e -> e{ envContext = cxt })

enterGiven topic action = do
    sym <- genSym "$_" topic
    enterLex [sym] action

enterWhen break action = callCC $ \esc -> do
    env <- ask
    contRec  <- genSubs env "&continue" $ continueSub esc
    breakRec <- genSubs env "&break" $ breakSub
    enterLex (contRec ++ breakRec) action
    where
    continueSub esc env = mkPrim
        { subName = "continue"
        , subParams = makeParams env
        , subFun = Prim (esc . head)
        }
    breakSub env = mkPrim
        { subName = "break"
        , subParams = makeParams env
        , subFun = break
        }

enterLoop action = callCC $ \esc -> do
    sym <- genSym "&last" $ lastSub esc
    enterLex [sym] action
    where
    lastSub esc = codeRef $ mkPrim
        { subName = "last"
        , subFun = Prim (const $ esc VUndef)
        }

enterBlock action = callCC $ \esc -> do
    env <- ask
    exitRec <- genSubs env "&?BLOCK_EXIT" $ escSub esc
    enterLex exitRec action
    where
    escSub esc env = mkPrim
        { subName = "BLOCK_EXIT"
        , subParams = makeParams env
        , subFun = Prim (esc . head)
        }
  
enterSub sub action
    | typ >= SubPrim = action -- primitives just happen
    | otherwise     = do
        env <- ask
        if typ >= SubBlock
            then do
                doFix <- fixEnv undefined env
                local doFix action
            else resetT $ callCC $ \cc -> do
                doFix <- fixEnv cc env
                local doFix action
    where
    typ = subType sub
    doReturn [v] = shiftT $ const $ evalVal v
    doReturn _   = internalError "enterSub: doReturn list length /= 1"
    doCC cc [v] = cc =<< evalVal v
    doCC _  _   = internalError "enterSub: doCC list length /= 1"
    orig sub = sub { subBindings = [], subParams = (map fst (subBindings sub)) }
    fixEnv cc env@Env{ envLexical = pad }
        | typ >= SubBlock = do
            blockRec <- genSym "&?BLOCK" (codeRef (orig sub))
            return $ \e -> e{ envLexical = (blockRec:subPad sub) ++ pad }
        | otherwise = do
            subRec <- sequence
                [ genSym "&?SUB" (codeRef (orig sub))
                , genSym "$?SUBNAME" (scalarRef $ VStr $ subName sub)]
            retRec    <- genSubs env "&return" retSub
            callerRec <- genSubs env "&?CALLER_CONTINUATION" (ccSub cc)
            return $ \e -> e
                { envLexical = concat [subRec, retRec, callerRec, subPad sub] }
    retSub env = mkPrim
        { subName = "return"
        , subParams = makeParams env
        , subFun = Prim doReturn
        }
    ccSub cc env = mkPrim
        { subName = "CALLER_CONTINUATION"
        , subParams = makeParams env
        , subFun = Prim $ doCC cc
        }

genSubs env name gen = sequence
    [ genSym name (codeRef $ gen env{ envContext = cxtItemAny })
    , genSym name (codeRef $ gen env{ envContext = cxtSlurpyAny })
    ]

makeParams Env{ envContext = cxt, envLValue = lv }
    = [ MkParam
        { isInvocant = False
        , isOptional = False
        , isNamed    = False
        , isLValue   = lv
        , isWritable = lv
        , isThunk    = False
        , paramName = case cxt of
            CxtSlurpy _ -> "@?0"
            _           -> "$?0"
        , paramContext = cxt
        , paramDefault = Val VUndef
        } ]

-- enter a lexical context

dumpLex :: String -> Eval ()
dumpLex label = do
    pad <- asks envLexical
    depth <- asks envDepth
    liftIO $ putStrLn ("("++(show depth)++")"++label ++ ": " ++ (show pad))
    return ()

caller :: Int -> Eval Env
caller n = do
    depth <- asks envDepth
    when (depth <= n) $
        fail "Cannot ask for deeper depth"
    asks $ foldl (.) id $ replicate n (fromJust . envCaller)

evalVal x = do
    -- context casting, go!
    -- XXX this needs a rewrite!
    -- Env{ envLValue = isLValue, envClasses = cls, envContext = cxt } <- ask
    Env{ envLValue = lv, envClasses = cls } <- ask
    typ <- evalValType x
    val <- if not lv && isaType cls "Junction" typ
        then readRef =<< fromVal x
        else return x
    return val
    {-
    if isSlurpyCxt cxt
        then return . VList . concat =<< fromVals val
        else return val
    --- XXX isCompatible is all wrong!
    let isCompatible | isSlurpyCxt cxt = isaType cls "List" typ
                     | otherwise       = isaType cls "Scalar" typ
        isRef = case val of { VRef _ -> True; _ -> False }
    -- trace (show ((cxt, typ), isCompatible, isLValue, isListCxt, val)) return ()
    
    case (isCompatible, isLValue, isSlurpyCxt cxt) of
        (True, True, _)         -> return val
        (True, False, False)    -> fromVal val
        (True, False, True)     -> return . VList =<< fromVal val
        (False, True, False)    -> return val -- auto scalar varify?
        (False, True, True)     -> return val -- auto list varify?
        (False, False, False)   -> if isRef then return val else fromVal' val
        (False, False, True)    -> return . VList =<< fromVal' val
    -}
