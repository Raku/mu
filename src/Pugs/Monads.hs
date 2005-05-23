{-# OPTIONS_GHC -fglasgow-exts #-}

{-|
    Common operations on Eval monad.
    
>   One Ring to rule them all,
>   One Ring to find them,
>   One Ring to bring them all
>   and in the darkness bind them...

    Note that the actual monads are defined elsewhere -- try looking at
    "Pugs.AST.SIO" and "Pugs.AST.Internals".
-}

module Pugs.Monads where
import Pugs.Internals
import Pugs.AST
import Pugs.Context
import Pugs.Types

{-|
Create a new lexical scope by applying the list of 'Pad'-transformers
(which install new bindings), then perform the specified evaluation in that
new scope.

(Subsequent chained 'Eval's do /not/ see this new scope.)
-}
enterLex :: [Pad -> Pad] -- ^ Transformations on current 'Pad' to produce the
                         --     new 'Pad'.
         -> Eval a       -- ^ Evaluation to be performed in the new scope
         -> Eval a       -- ^ Resulting evaluation (lexical scope enter & exit
                         --     are encapsulated)
enterLex newSyms = local (\e -> e{ envLexical = combine newSyms (envLexical e) })

{-|
Perform the specified evaluation in the specified (Perl6) context ('Cxt').

(Subsequent chained 'Eval's do /not/ see this new scope.)
-}
enterContext :: Cxt -> Eval a -> Eval a
enterContext cxt = local (\e -> e{ envContext = cxt })

{-|
Evaluate the specified wxpression in the specified (Perl6) context ('Cxt').

(Subsequent chained 'Eval's do /not/ see this new scope.)
-}
enterEvalContext :: Cxt -> Exp -> Eval Val
enterEvalContext cxt = enterContext cxt . evalExp

{-|
Bind @\$_@ to the given topic value in a new lexical scope, then perform
the specified evaluation in that scope.

Used by "Pugs.Eval"'s implementation of 'Pugs.Eval.reduce' for @\"given\"@.
-}
enterGiven :: VRef   -- ^ Reference to the value to topicalise
           -> Eval a -- ^ Action to perform within the new scope
           -> Eval a
enterGiven topic action = do
    sym <- genSym "$_" topic
    enterLex [sym] action

{-|
Bind @&continue@ and @&break@ to subs that break out of the @when@ body
and topicalising block respectively, then perform the given evaluation
in the new lexical scope.

Note that this function is /not/ responsible for performing the actual @when@
test, nor is it responsible for adding the implicit @break@ to the end of the
@when@'s block--those are already taken care of by 'Pugs.Eval.reduce'
(see the entry for @('Syn' \"when\" ... )@).
-}
enterWhen :: Exp      -- ^ The expression that @&break@ should be bound to
          -> Eval Val -- ^ The @when@'s body block, as an evaluation
          -> Eval Val
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

{-|
Generate a new Perl6 operation from a Haskell function, give it a name, and
generate a @('Pad' -> 'Pad')@ transformer that can be used to install it into
a pad.

This transformer is passed into a given \'action\' function, which is
expected to apply the pad-transformer (e.g. in a new lexical scope), then
perform some evaluation in that scope.

Most of the time, this \'action\' is an anonymous function that passes its
argument into 'enterLex'.
-}
genSymPrim :: (MonadSTM m) 
           => String                -- ^ Name installed in 'Pad'
                                    --     (must have leading @&@ sigil)
           -> ([Val] -> Eval Val)   -- ^ The actual primitive to wrap
           -> ((Pad -> Pad) -> m t) -- ^ A (lambda) function that the 'Pad'
                                    --     transformer is given to
           -> m t -- ^ Result of passing the pad-transformer to the \'action\'
genSymPrim symName@('&':name) prim action = do
    newSym <- genSym symName . codeRef $ mkPrim
        { subName = name
        , subBody = Prim prim
        }
    action newSym
genSymPrim _ _ _ = error "need a &name"

{-|
Generate a Perl6 primitive that, when called, will activate the /current/
continuation (i.e. one that can be used to immediately break out of whatever 
evaluation we are about to perform). This is great for @&last@ and the like.

This produces a pad-transformer @('Pad' -> 'Pad')@. This transformer is given
to an \'action\' function, which is expected to apply it (e.g. in a lexical
scope), then perform some evaluation in that scope.
-}
genSymCC :: String -- ^ Name of the primitive in the symbol table ('Pad').
         -> ((Pad -> Pad) -> Eval Val) -- ^ An \'action\' function that will
                                       --     take the pad-transformer and use
                                       --     it to perform some evaluation 
         -> Eval Val -- ^ Result of passing the pad-transformer to the 
                     --     \'action\'
genSymCC symName action = callCC $ \esc -> do
    genSymPrim symName (const $ esc undef) action

{-|
Create a Perl6 @&?BLOCK_EXIT@ function that, when activated, breaks out of
the block scope by activating the current continuation. The block body
evaluation is then performed in a new lexical scope with @&?BLOCK_EXIT@
installed.

Used by 'Pugs.Eval.reduce' when evaluating @('Syn' \"block\" ... )@ 
expressions.
-}
enterBlock :: Eval Val -> Eval Val
enterBlock action = callCC $ \esc -> do
    env <- ask
    exitRec <- genSubs env "&?BLOCK_EXIT" $ escSub esc
    local (\e -> e{ envOuter = Just env }) $ enterLex exitRec action
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
    doCC :: (Val -> Eval b) -> [Val] -> Eval b
    doCC cc [v] = cc =<< evalVal v
    doCC _  _   = internalError "enterSub: doCC list length /= 1"
    orig :: VCode -> VCode
    orig sub = sub { subBindings = [], subParams = (map fst (subBindings sub)) }
    fixEnv :: (Val -> Eval Val) -> Env -> Eval (Env -> Env)
    fixEnv cc env
        | typ >= SubBlock = do
            blockRec <- genSym "&?BLOCK" (codeRef (orig sub))
            return $ \e -> e
                { envOuter = Just env
                , envPackage = maybe (envPackage e) envPackage (subEnv sub)
                , envLexical = combine [blockRec]
                    (subPad sub `unionPads` envLexical env) }
        | otherwise = do
            subRec <- sequence
                [ genSym "&?SUB" (codeRef (orig sub))
                , genSym "$?SUBNAME" (scalarRef $ VStr $ subName sub)]
            -- retRec    <- genSubs env "&return" retSub
            callerRec <- genSubs env "&?CALLER_CONTINUATION" (ccSub cc)
            return $ \e -> e
                { envLexical = combine (concat [subRec, callerRec]) (subPad sub)
                , envPackage = maybe (envPackage e) envPackage (subEnv sub)
                , envOuter   = maybe Nothing envOuter (subEnv sub)
                }
    ccSub :: (Val -> Eval Val) -> Env -> VCode
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

headVal :: [Val] -> Eval Val
headVal []    = retEmpty
headVal (v:_) = return v

