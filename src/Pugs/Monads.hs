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

enterGiven topic action = enterLex [MkSym "$_" topic] action

enterWhen break action = callCC $ \esc -> do
    env <- ask
    enterLex ((genSubs env "&continue" $ continueSub esc)
           ++ (genSubs env "&break" $ breakSub)) action
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
    enterLex [MkSym "&last" $ lastSub esc] action
    where
    lastSub esc = codeRef $ mkPrim
        { subName = "last"
        , subFun = Prim (const $ esc VUndef)
        }

enterBlock action = callCC $ \esc -> do
    env <- ask
    enterLex (genSubs env "&?BLOCK_EXIT" $ escSub esc) action
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
            then local (fixEnv undefined env) action
            else resetT $ callCC $ \cc -> local (fixEnv cc env) action
    where
    typ = subType sub
    doReturn [v] = shiftT $ const $ evalVal v
    doReturn _   = internalError "enterSub: doReturn list length /= 1"
    doCC cc [v] = cc =<< evalVal v
    doCC _  _   = internalError "enterSub: doCC list length /= 1"
    orig sub = sub { subBindings = [], subParams = (map fst (subBindings sub)) }
    subRec = [ MkSym "&?SUB" (codeRef (orig sub))
             , MkSym "$?SUBNAME" (scalarRef $ VStr $ subName sub)]
    blockRec = MkSym "&?BLOCK" (codeRef (orig sub))
    fixEnv cc env@Env{ envLexical = pad } env'
        | typ >= SubBlock = env'{ envLexical = (blockRec:subPad sub) ++ pad }
        | otherwise      = env'{ envLexical = concat
            [ subRec
            , genSubs env "&return" retSub
            , genSubs env "&?CALLER_CONTINUATION" (ccSub cc)
            , subPad sub
            ] }
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

genSubs env name gen =
    [ MkSym name (codeRef $ gen env{ envContext = cxtItemAny })
    , MkSym name (codeRef $ gen env{ envContext = cxtSlurpyAny })
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
    Env{ envLValue = _, envClasses = cls, envContext = _ } <- ask
    typ <- evalValType x
    val <- if isaType cls "Junction" typ then fromVal' x else return x
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
