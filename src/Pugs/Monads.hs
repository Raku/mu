{-# OPTIONS_GHC -fglasgow-exts #-}

{-|
    Monad structures.
    
    Note that there aren't actually any monads defined here--try looking in
    "Pugs.AST.SIO" and "Pugs.AST.Internals".

>   One Ring to rule them all,
>   One Ring to find them,
>   One Ring to bring them all
>   and in the darkness bind them...
-}

module Pugs.Monads where
import Pugs.Internals
import Pugs.AST
import Pugs.Context
import Pugs.Types

headVal :: [Val] -> Eval Val
headVal []    = retEmpty
headVal (v:_) = return v

-- |Perform the specified evaluation in a lexical scope that has been
-- augmented by the given list of lexical 'Pad' transformers. Subsequent
-- chained 'Eval's do /not/ see this new scope.
enterLex :: [Pad -> Pad] -- ^ Transformations on current 'Pad' to produce the
                         --     new 'Pad'.
         -> Eval a       -- ^ Evaluation to be performed in the new scope
         -> Eval a       -- ^ Resulting evaluation (lexical scope enter & exit
                         --     are encapsulated)
enterLex newSyms = local (\e -> e{ envLexical = combine newSyms (envLexical e) })

-- |Perform the specified evaluation in the specified context ('Cxt').
-- Subsequent chained 'Eval's do /not/ see this new scope.
enterContext :: Cxt -> Eval a -> Eval a
enterContext cxt = local (\e -> e{ envContext = cxt })

-- |Bind @\$_@ to the given topic value in a new lexical scope, then perform
-- the given evaluation in that scope. Used by "Pugs.Eval"'s implementation
-- of 'Pugs.Eval.reduce' for @\"given\"@.
enterGiven :: VRef   -- ^ Reference to the value to topicalise
           -> Eval a -- ^ Action to perform within the new scope
           -> Eval a
enterGiven topic action = do
    sym <- genSym "$_" topic
    enterLex [sym] action

enterWhen :: Exp -> Eval Val -> Eval Val
enterWhen break action = callCC $ \esc -> do
    env <- ask
    contRec  <- genSubs env "&continue" $ continueSub esc
    breakRec <- genSubs env "&break" $ breakSub
    enterLex (contRec ++ breakRec) action
    where
    continueSub esc env = mkPrim
        { subName = "continue"
        , subParams = makeParams env
        , subBody = Prim ((esc =<<) . headVal)
        }
    breakSub env = mkPrim
        { subName = "break"
        , subParams = makeParams env
        , subBody = break
        }

enterLoop :: Eval Val -> Eval Val
enterLoop action = genSymCC "&last" $ \symLast -> do
    genSymPrim "&next" (const action) $ \symNext -> do
        enterLex [symLast, symNext] action

genSymPrim :: (MonadSTM m) 
           => String 
           -> ([Val] -> Eval Val)     
           -> ((Pad -> Pad) -> m t)
           -> m t
genSymPrim symName@('&':name) prim action = do
    newSym <- genSym symName . codeRef $ mkPrim
        { subName = name
        , subBody = Prim prim
        }
    action newSym
genSymPrim _ _ _ = error "need a &name"

genSymCC :: String
         -> ((Pad -> Pad) -> Eval Val)
         -> Eval Val
genSymCC symName action = callCC $ \esc -> do
    genSymPrim symName (const $ esc undef) action

{-|
Perform the specified evaluation in a new lexical scope in which
@&?BLOCK_EXIT@ is bound to a continuation that will break out of the block
when called. (Actually, @&?BLOCK_EXIT@ is bound to a 'Prim' 'VCode'
that is /implemented/ using the continuation.) Used by 'Pugs.Eval.reduce'
when evaluating @('Syn' \"block\" ... )@ expressions.
-}
enterBlock :: Eval Val -> Eval Val
enterBlock action = callCC $ \esc -> do
    env <- ask
    exitRec <- genSubs env "&?BLOCK_EXIT" $ escSub esc
    enterLex exitRec action
    where
    escSub esc env = mkPrim
        { subName = "BLOCK_EXIT"
        , subParams = makeParams env
        , subBody = Prim ((esc =<<) . headVal)
        }

enterSub :: VCode -> Eval Val -> Eval Val
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
    doCC cc [v] = cc =<< evalVal v
    doCC _  _   = internalError "enterSub: doCC list length /= 1"
    orig sub = sub { subBindings = [], subParams = (map fst (subBindings sub)) }
    fixEnv cc env
        | typ >= SubBlock = do
            blockRec <- genSym "&?BLOCK" (codeRef (orig sub))
            return $ \e -> e
                { envLexical = combine [blockRec]
                    (subPad sub `unionPads` envLexical env) }
        | otherwise = do
            subRec <- sequence
                [ genSym "&?SUB" (codeRef (orig sub))
                , genSym "$?SUBNAME" (scalarRef $ VStr $ subName sub)]
            -- retRec    <- genSubs env "&return" retSub
            callerRec <- genSubs env "&?CALLER_CONTINUATION" (ccSub cc)
            return $ \e -> e
                { envLexical = combine (concat [subRec, callerRec]) (subPad sub) }
    ccSub cc env = mkPrim
        { subName = "CALLER_CONTINUATION"
        , subParams = makeParams env
        , subBody = Prim $ doCC cc
        }

genSubs :: t -> Var -> (t -> VCode) -> Eval [Pad -> Pad]
genSubs env name gen = sequence
    [ genMultiSym name (codeRef $ gen env)
    , genMultiSym name (codeRef $ (gen env) { subParams = [] })
    ]

makeParams :: Env -> [Param]
makeParams MkEnv{ envContext = cxt, envLValue = lv }
    = [ MkParam
        { isInvocant = False
        , isOptional = False
        , isNamed    = False
        , isLValue   = lv
        , isWritable = lv
        , isLazy     = False
        , paramName = case cxt of
            CxtSlurpy _ -> "@?0"
            _           -> "$?0"
        , paramContext = cxt
        , paramDefault = Val VUndef
        } ]

caller :: Int -> Eval Env
caller n = do
    depth <- asks envDepth
    when (depth <= n) $
        fail "Cannot ask for deeper depth"
    asks $ foldl (.) id $ replicate n (fromJust . envCaller)

evalVal :: Val -> Eval Val
evalVal val = do
    lv  <- asks envLValue
    cls <- asks envClasses
    typ <- evalValType val
    if lv then return val else do
    case val of
        VRef ref | refType ref == mkType "Scalar::Const" -> do
            evalVal =<< readRef ref
        VRef ref | isaType cls "Junction" typ -> do
            evalVal =<< readRef ref
        _ -> do
            return val
