{-# OPTIONS_GHC -fglasgow-exts -cpp -fallow-overlapping-instances #-}

module Pugs.Eval.Var (
    findVar, findVarRef, findSub,
    inferExpType,  inferExpCxt, FindSubFailure(..),
    packageOf, toPackage, toQualified,
) where
import qualified Data.Map as Map
import Pugs.Internals
import Pugs.AST
import Pugs.Types
import Pugs.Embed.Perl5
import Pugs.Bind
import Pugs.Prim.List (op2Reduce, op1HyperPrefix, op1HyperPostfix, op2Hyper)
import Pugs.Prim.Param (foldParam)
import Pugs.Pretty
import Pugs.Config
import Pugs.Monads
import qualified Pugs.Val as Val
import Pugs.Val hiding (Val, IValue, VUndef, Var)
import qualified Data.ByteString.Char8 as Str

findVar :: Var -> Eval (Maybe VRef)
findVar var
    | SType <- v_sigil var
    , not (isGlobalVar var)
    = return Nothing
    | otherwise = do
        rv <- findVarRef var
        case rv of
            Just ref -> fmap Just $ liftSTM (readTVar ref)
            Nothing
                | SCode == v_sigil var || SCodeMulti == v_sigil var -> do
                    sub <- findSub var Nothing []
                    return $ either (const Nothing) (Just . codeRef) sub
                | otherwise -> return Nothing


lookupShellEnvironment :: ByteString -> Eval (Maybe (TVar VRef))
lookupShellEnvironment name = do
    exists <- evalExp $ App (_Var "&exists") (Just (_Var "%*ENV")) [Val (VStr $ cast name)]
    case exists of
        VBool False -> do
            retError "no such ENV variable" name
        _           -> do
            rv   <- enterLValue (evalExp $ Syn "{}" [_Var "%*ENV", Val (VStr $ cast name)])
            tvar <- liftSTM . newTVar =<< fromVal rv
            return (Just tvar)

findVarRef :: Var -> Eval (Maybe (TVar VRef))
findVarRef var@MkVar{ v_sigil = sig, v_twigil = twi, v_name = name, v_package = pkg }
    | Just var' <- dropVarPkg (__"CALLER") var = do
        maybeCaller <- asks envCaller
        case maybeCaller of
            Just env -> local (const env) $ findVarRef var'
            Nothing -> retError "cannot access CALLER:: in top level" var

    | Just var' <- dropVarPkg (__"ENV") var = fix $ \upLevel -> do
        maybeCaller <- asks envCaller
        case maybeCaller of
            Just env -> local (const env) $ do
                rv <- findVarRef var'
                if isJust rv then return rv else upLevel
            -- final callback: try an "environment" lookup
            -- XXX: how does "@+PATH" differ from "$+PATH"?
            -- XXX: how to tell empty env from nonexistent env?
            --      should we allow writes?
            Nothing -> lookupShellEnvironment (cast name)

    | Just var' <- dropVarPkg (__"OUTER") var = do
        maybeOuter <- asks envOuter
        case maybeOuter of
            Just env -> local (const env) $ findVarRef var'
            Nothing -> retError "cannot access OUTER:: in top level" name

    | pkg /= emptyPkg = doFindVarRef var

    | TMagical <- twi = do
        rv  <- getMagical var
        case rv of
            Nothing  -> doFindVarRef var
            Just val -> do
                tvar <- liftSTM $ newTVar (MkRef . constScalar $ val)
                return $ Just tvar

    | SHash <- sig, nullID == name = do
        {- %CALLER::, %OUTER::, %Package::, etc, all recurse to here. -}
        pad <- asks envLexical
        let plist   = padToList pad
        hlist <- mapM padEntryToHashEntry plist
        let hash    = IHash $ Map.fromList hlist
        let hashref = MkRef hash
        tvar <- liftSTM $ newTVar hashref
        return $ Just tvar
    | otherwise = doFindVarRef var
    where
    padEntryToHashEntry :: (Var, [(TVar Bool, TVar VRef)]) -> Eval (VStr, Val)
    padEntryToHashEntry (key, (_, tvref) : _) = do
        vref   <- liftSTM (readTVar tvref)
        let val = VRef vref
        return (cast key, val)
    padEntryToHashEntry (_, []) = fail "Nonexistant var in pad?"

doFindVarRef :: Var -> Eval (Maybe (TVar VRef))
doFindVarRef var = do
    callCC $ \foundIt -> do
        lexSym  <- fmap (findSym var . envLexical) ask
        when (isJust lexSym) $ foundIt lexSym
        -- XXX - this is bogus; we should not fallback if it's not in lex csope.
        glob    <- liftSTM . readTVar . envGlobal =<< ask
        var'    <- toQualified var
        let globSym = findSym var' glob
        when (isJust globSym) $ foundIt globSym
        -- XXX - ditto for globals
        let globSym = findSym (toGlobalVar var) glob
        when (isJust globSym) $ foundIt globSym
        return Nothing


{-|
  The findSub dispatch system:

  The Eval.Var findSub dispatch system was written before S12's
  dispatch order was specced clearly.  Back then there were no clear
  distinction between single and multiple dispatch, and so both were
  lumped in a single findSub loop.  So "$x.foo.bar" is no different
  from bar(foo($x)), rather, bar(foo($x:):).  But MMD dictates that
  you have to find which of the candidate &bar to be dispatched to
  before you fully evaluate its argument foo($x:) under its specified
  context.  But you can't find out which candidates of &bar to
  dispatch to, unless you know the type of foo($x).  That's a
  chicken-egg problem.  So I wrote a tiny type inferencer to guess a
  type of an Exp without actually evaluating its side effects.  The
  idea is that foo($x) is first inferred, then uses that inferred type
  to decide which bar() to call, and finally evaluate foo($x) in full.
  Using the argument type expected by bar().  But the infer engine
  didn't handle indexed expressions well.  $x[0] etc.  In any case,
  that tiny inferencer is obsolete under the new (November 2005) S12
  and docs/notes/multimethods.pod.  Which would be implemented at PIL
  layer.  The inferencer fix I just committed (r8874) for indexed
  expressions, just checks if both the indice and indexee are simple
  expressions (i.e. things that can be evaluated without side
  effects), and if so, it just evaluates them to find out their actual
  type.

-}

data FindSubFailure
    = NoMatchingMulti
    | NoSuchSub
    | NoSuchMethod !Type
    deriving (Show)

_SUPER :: ByteString
_SUPER = __"SUPER"

findSub :: Var        -- ^ Name, with leading @\&@.
        -> Maybe Exp  -- ^ Invocant
        -> [Exp]      -- ^ Other arguments
        -> Eval (Either FindSubFailure VCode)
findSub _var _invs _args = case _invs of
    Nothing -> findBuiltinSub NoMatchingMulti _var
    _ | not (isQualifiedVar _var) -> case unwrap _inv of
        Val vv@VV{}     -> withExternalCall callMethodVV vv
        Val sv@PerlSV{} -> withExternalCall callMethodPerl5 sv
        inv' -> do
            typ <- evalInvType inv'
            findTypedSub (cast typ) _var
      | Just var' <- dropVarPkg _SUPER _var -> do
        pkg <- asks envPackage
        findSuperSub pkg var'
      | otherwise -> do
        findBuiltinSub NoMatchingMulti _var
    where
    _inv = fromJust _invs

    -- findSuperSub :: (_var :: Var, _invs :: Maybe Exp, _args :: [Exp])
    --     => Pkg -> Var -> Eval (Either FindSubFailure VCode)
    findSuperSub pkg var = do
        subs    <- findWithSuper pkg var
        subs'   <- either (flip findBuiltinSub var) (return . Right) subs
        case subs' of
            -- Recursion prevention -- SUPER::foo should not go back to ThisClas::foo
            Right sub | cast (Str.cons '&' $ subName sub) == var{ v_package = pkg } -> do
                return (Left . NoSuchMethod $ cast pkg)
            _   -> do
                return subs'

    -- findTypedSub :: (_var :: Var, _invs :: Maybe Exp, _args :: [Exp])
    --     => Pkg -> Var -> Eval (Either FindSubFailure VCode)
    findTypedSub pkg var = do
        subs    <- findWithPkg pkg var
        either (flip findBuiltinSub var) (return . Right) subs

    evalInvType :: Exp -> Eval Type
    evalInvType x = inferExpType $ unwrap x

    withExternalCall callMeth inv = do
        fmap (err . NoSuchMethod $ valType inv) $ do
            metaSub <- possiblyBuildMetaopVCode _var
            if isJust metaSub then return metaSub else callMeth

    -- callMethodVV :: (_var :: Var, _invs :: Maybe Exp, _args :: [Exp])
    --     => Eval (Maybe VCode)
    callMethodVV = do
        let methName = cast (v_name _var)
        -- Look up the proto for the method in VV land right here
        -- Whether it matched or not, it's the proto's signature
        -- that's available to the inferencer, not any of its children's
        -- (this is because MMD in newland is performed _after_ everything
        -- has been reduced.)
        return . Just $ mkPrim
            { subName     = methName
            , subParams   = makeParams ["Object", "List", "Named"]
            , subReturns  = mkType "Any"
            , subBody     = Prim $ \(inv:named:pos:_) -> do
                invVV   <- fromVal inv      :: Eval Val.Val
                posVVs  <- fromVals pos     :: Eval [Val.Val]
                namVVs  <- do
                    list <- fromVal named
                    fmap Map.fromList $ forM list $ \(k, v) -> do
                        key <- fromVal k
                        val <- fromVal v
                        return (key, [val])   :: Eval (ID, [Val.Val])

                -- This is the Capture object we are going to work with
                let capt = CaptMeth invVV [MkFeed posVVs namVVs]

                return . castV $ "CCall " ++ show methName ++ " " ++ show capt
            }

    -- callMethodPerl5 :: (_var :: Var, _invs :: Maybe Exp, _args :: [Exp])
    --     => Eval (Maybe VCode)
    callMethodPerl5 = do
        let name = cast (v_name _var)
        return . Just $ mkPrim
            { subName     = name
            , subParams   = makeParams ["Object", "List", "Named"]
            , subReturns  = mkType "Scalar::Perl5"
            , subBody     = Prim $ \(inv:named:pos:_) -> do
                sv      <- fromVal inv
                posSVs  <- fromVals pos
                namSVs  <- fmap concat (fromVals named)
                let svs = posSVs ++ namSVs
                found   <- liftIO $ canPerl5 sv name
                found'  <- liftIO $ if found
                    then return found
                    else canPerl5 sv (__"AUTOLOAD")
                if not found'
                    then do
                        -- XXX - when svs is empty, this could call back here infinitely
                        --       add an extra '&' to force no-reinterpretation.
                        evalExp $
                            App (Var _var{ v_sigil = SCodeMulti }) Nothing
                                (map (Val . PerlSV) (sv:svs))
                    else do
                        subSV   <- liftIO . bufToSV $ name
                        runInvokePerl5 subSV sv svs
            }

    -- findWithPkg :: (_var :: Var, _invs :: Maybe Exp, _args :: [Exp])
    --     => Pkg -> Var -> Eval (Either FindSubFailure VCode)
    findWithPkg pkg var = do
        subs <- findSub' var{ v_package = pkg }
        maybe (findWithSuper pkg var) (return . Right) subs

    -- findWithSuper :: (_var :: Var, _invs :: Maybe Exp, _args :: [Exp])
    --     => Pkg -> Var -> Eval (Either FindSubFailure VCode)
    findWithSuper pkg var = do
        -- get superclasses
        attrs <- fmap (fmap (filter (/= pkg) . nub)) $ findAttrs pkg
        if isNothing attrs || null (fromJust attrs) then fmap (err NoMatchingMulti) (findSub' var) else do
        (`fix` (fromJust attrs)) $ \run pkgs -> do
            if null pkgs then return (Left $ NoSuchMethod (cast pkg)) else do
            subs <- findWithPkg (head pkgs) var
            either (const $ run (tail pkgs)) (return . Right) subs

    -- findSub' :: (_var :: Var, _invs :: Maybe Exp, _args :: [Exp]) => Var -> Eval (Maybe VCode)
    findSub' var = do
        subSyms     <- findSyms var
        lens        <- mapM argSlurpLen (unwrap $ maybeToList _invs ++ _args)
        doFindSub (sum lens) subSyms

    argSlurpLen :: Exp -> Eval Int
    argSlurpLen (Val val) = valSlurpLen val
    argSlurpLen (Var name) = do
        val <- evalExp (Var name)
        valSlurpLen val
    argSlurpLen (Syn "," list) =  return $ length list
    argSlurpLen _ = return 1 -- XXX

    valSlurpLen :: Val -> Eval Int
    valSlurpLen (VList list) = return $ length list
    valSlurpLen (VRef (MkRef (IArray av))) = array_fetchSize av
    valSlurpLen (VRef (MkRef (IHash hv))) = hash_fetchSize hv
    valSlurpLen _  = return 1 -- XXX

    -- doFindSub :: (_var :: Var, _invs :: Maybe Exp, _args :: [Exp])
    --     => Int -> [(Var, Val)] -> Eval (Maybe VCode)
    doFindSub slurpLen subSyms = do
        subs' <- subs slurpLen subSyms
        -- let foo (x, sub) = show x ++ show (map paramContext $ subParams sub)
        -- trace (unlines $ map foo $ sort subs') return ()
        return $ case sort subs' of
            ((_, sub):_)    -> Just sub
            _               -> Nothing

    -- subs :: (_invs :: Maybe Exp, _args :: [Exp])
    --     => Int -> [(Var, Val)] -> Eval [((Bool, Bool, Int, Int), VCode)]
    subs slurpLen subSyms = fmap catMaybes . forM subSyms $ \(_, val) -> do
        sub@(MkCode{ subReturns = ret, subParams = prms }) <- fromVal val
        let rv = return $ arityMatch sub (length (maybeToList _invs ++ _args)) slurpLen
        maybeM rv $ \fun -> do
            -- if deltaFromCxt ret == 0 then return Nothing else do
            let pairs = map (typeOfCxt . paramContext) prms
                            `zip` (map unwrap $ maybeToList _invs ++ _args)
            deltaCxt    <- deltaFromCxt ret
            deltaArgs   <- mapM deltaFromPair pairs
            let bound = either (const False) (const True) $ bindParams sub _invs _args
            return ((isMulti sub, bound, sum deltaArgs, deltaCxt), fun)

    -- findBuiltinSub :: (_var :: Var, _invs :: Maybe Exp, _args :: [Exp])
    --     => FindSubFailure -> Var -> Eval (Either FindSubFailure VCode)
    findBuiltinSub failure var = do
        sub <- findSub' var
        maybe (fmap (err failure) $ possiblyBuildMetaopVCode var) (return . Right) sub

    -- firstArg :: (_args :: [Exp]) => [Exp]
    firstArg = [maybe (Val undef) id (listToMaybe _args)]

    buildPrefixHyper name var = do
        let rv = fmap (either (const Nothing) Just) $
                findSub var Nothing firstArg
        maybeM rv $ \code -> return $ mkPrim
            { subName     = name
            , subType     = SubPrim
            , subAssoc    = subAssoc code
            , subParams   = subParams code
            , subReturns  = mkType "List"
            , subBody     = Prim
                (\x -> op1HyperPrefix code (listArg x))
            }

    buildPostfixHyper name var = do
        let rv = fmap (either (const Nothing) Just) $
                findSub var Nothing firstArg
        maybeM rv $ \code -> return $ mkPrim
            { subName     = name
            , subType     = SubPrim
            , subAssoc    = subAssoc code
            , subParams   = subParams code
            , subReturns  = mkType "List"
            , subBody     = Prim
                (\x -> op1HyperPostfix code (listArg x))
            }

    buildInfixHyper name var = do
        let rv = fmap (either (const Nothing) Just) $
                findSub var Nothing (take 2 (_args ++ [Val undef, Val undef]))
        maybeM rv $ \code -> return $ mkPrim
            { subName     = name
            , subType     = SubPrim
            , subAssoc    = subAssoc code
            , subParams   = makeParams ["Any", "Any"]
            , subReturns  = mkType "List"
            , subBody     = Prim (\[x, y] -> op2Hyper code x y)
            }

    -- possiblyBuildMetaopVCode :: (_args :: [Exp]) => Var -> Eval (Maybe VCode)
    possiblyBuildMetaopVCode var@MkVar{ v_categ = cat, v_name = name }
        | C_prefix <- cat, '\171' <- Str.last buf = do
            buildPrefixHyper buf var{ v_name = cast $ Str.init buf }
        | C_prefix <- cat, __"<<" `Str.isSuffixOf` buf = do
            buildPrefixHyper buf var{ v_name = cast $ dropEnd 2 buf }
        | C_postfix <- cat, '\187' <- Str.head buf = do
            buildPostfixHyper buf var{ v_name = cast $ Str.tail buf }
        | C_postfix <- cat, __">>" `Str.isPrefixOf` buf = do
            buildPostfixHyper buf var{ v_name = cast $ Str.drop 2 buf }
        | C_infix <- cat, '\187' <- Str.head buf, '\171' <- Str.last buf = do
            buildInfixHyper buf var{ v_name = cast $ Str.init (Str.tail buf) }
        | C_infix <- cat, __">>" `Str.isPrefixOf` buf, __"<<" `Str.isSuffixOf` buf = do
            buildInfixHyper buf var{ v_name = cast $ Str.take 2 (dropEnd 2 buf) }
        | C_prefix <- cat, '[' <- Str.head buf, ']' <- Str.last buf = do
            -- Strip the trailing "]" from op
            let (op, keep)
                    | Str.index buf 1 == '\\'   = (Str.drop 2 (Str.init buf), True)
                    | otherwise                 = (Str.tail (Str.init buf), False)

            -- We try to find the userdefined sub.
            -- We use the first two elements of invs as invocants, as these are the
            -- types of the op.
                rv = fmap (either (const Nothing) Just) $
                    findSub (var{ v_categ = C_infix, v_name = cast op }) Nothing
                        (take 2 $ _args ++ [Val undef, Val undef])
            maybeM rv $ \code -> return $ mkPrim
                { subName     = buf
                , subType     = SubPrim
                , subAssoc    = ANil
                , subParams   = makeParams $
                    if any isLValue (subParams code)
                        then ["rw!List"] -- XXX - does not yet work for the [=] case
                        else ["List"]
                , subReturns  = anyType
                , subBody     = Prim $ \[vs] -> do
                    list_of_args <- fromVal vs
                    op2Reduce keep list_of_args (VCode code)
                }
            -- Now we construct the sub. Is there a more simple way to do it?
        | otherwise = return Nothing
        where
        buf = cast name

metaVar :: Pkg -> Var
-- metaVar = MkVar SType TNil globalPkg CNil . cast
metaVar pkg = cast (':':'*':cast pkg)
    {-
MkVar
    { v_sigil   = SType
    , v_twigil  = TGlobal
    , v_package = emptyPkg
    , v_categ   = CNil
    , v_name    = cast pkg
    }
    -}

err :: b -> Maybe a -> Either b a
err _ (Just j) = Right j
err x Nothing  = Left x

listArg :: [Val] -> Val
listArg [x] = x
listArg xs = VList xs

makeParams :: [String] -> [Param]
makeParams = map (\p -> p{ isWritable = isLValue p }) . foldr foldParam [] . map takeWord
    where
    takeWord = takeWhile isWord . dropWhile (not . isWord)
    isWord   = not . (`elem` "(),:")

deltaFromCxt :: Type -> Eval Int
deltaFromCxt x  = do
    cls <- asks envClasses
    cxt <- asks envContext
    return $ deltaType cls (typeOfCxt cxt) x

deltaFromPair :: (Type, Exp) -> Eval Int
deltaFromPair (x, y) = do
    cls <- asks envClasses
    typ <- inferExpType y
    return $ deltaType cls x typ

findAttrs pkg = do
    maybeM (findVar $ metaVar pkg) $ \ref -> do
        meta    <- readRef ref
        fetch   <- doHash meta hash_fetchVal
        fmap (map (cast :: String -> Pkg)) (fromVal =<< fetch "is")

{-|
Take an expression, and attempt to predict what type it will evaluate to
/without/ actually evaluating it.
-}
inferExpType :: Exp -> Eval Type
inferExpType exp@(Var var)
    | TAttribute <- v_twigil var = fromVal =<< evalExp exp
    | TPrivate <- v_twigil var   = fromVal =<< evalExp exp
    | otherwise = do
        rv  <- findVar var
        case rv of
            Nothing  -> return $ typeOfSigilVar var
            Just ref -> do
                let typ = refType ref
                cls <- asks envClasses
                if isaType cls "List" typ
                    then return typ
                    else fromVal =<< readRef ref
inferExpType (Val val) = fromVal val
inferExpType (App (Val val) _ _) = do
    sub <- fromVal val
    return $ subReturns sub
inferExpType (App (Var var) (Just inv) _)
    | var == cast "&new"
    = inferExpType $ unwrap inv
inferExpType (App (Var name) invs args) = do
    sub <- findSub name invs args
    case sub of
        Right sub    -> return $ subReturns sub
        Left _       -> return $ mkType "Any"
inferExpType (Ann (Cxt cxt) _) | typeOfCxt cxt /= (mkType "Any") = return $ typeOfCxt cxt
inferExpType (Ann (Cxt _) exp) = inferExpType exp
inferExpType (Ann (Pos _) exp) = inferExpType exp
inferExpType (Pad _ _ exp) = inferExpType exp
inferExpType (Sym _ _ exp) = inferExpType exp
inferExpType (Stmts _ exp) = inferExpType exp
inferExpType (Syn "," _)    = return $ mkType "List"
inferExpType (Syn "\\[]" _) = return $ mkType "Array"
inferExpType (Syn "\\{}" _) = return $ mkType "Hash"
inferExpType (Syn "&{}" _)  = return $ mkType "Code"
inferExpType (Syn "@{}" _)  = return $ mkType "Array"
inferExpType (Syn "%{}" _)  = return $ mkType "Hash"
inferExpType (Syn "=>" _)   = return $ mkType "Pair"
inferExpType exp@(Syn "{}" [_, idxExp]) = if isSimpleExp exp
    then fromVal =<< enterRValue (evalExp exp)
    else fmap typeOfCxt (inferExpCxt idxExp)
inferExpType exp@(Syn "[]" [_, idxExp]) = if isSimpleExp exp
    then fromVal =<< enterRValue (evalExp exp)
    else fmap typeOfCxt (inferExpCxt idxExp)
inferExpType (Syn "sub" [exp]) = inferExpType exp
inferExpType _ = return anyType

isSimpleExp :: Exp -> Bool
isSimpleExp Var{}           = True
isSimpleExp Val{}           = True
isSimpleExp (Ann _ x)       = isSimpleExp x
isSimpleExp (Syn _ xs)      = all isSimpleExp xs
isSimpleExp _               = False


{-|
Return the context that an expression bestows upon a hash or array
subscript. See 'reduce' for @\{\}@ and @\[\]@.
-}
inferExpCxt :: Exp -> Eval Cxt
inferExpCxt exp = return $ if isScalarLValue exp
    then cxtItemAny
    else cxtSlurpyAny
{-
inferExpCxt :: Exp -- ^ Expression to find the context of
         -> Eval Cxt
inferExpCxt (Ann (Pos {}) exp)           = inferExpCxt exp
inferExpCxt (Ann (Cxt cxt) _)            = return cxt
inferExpCxt (Syn "," _)            = return cxtSlurpyAny
inferExpCxt (Syn "[]" [_, exp])    = inferExpCxt exp
inferExpCxt (Syn "{}" [_, exp])    = inferExpCxt exp
inferExpCxt (Syn (sigil:"{}") _) = return $ cxtOfSigil sigil
inferExpCxt (Val (VList {}))       = return cxtSlurpyAny
inferExpCxt (Val (VRef ref))       = do
    cls <- asks envClasses
    let typ = refType ref
    return $ if isaType cls "List" typ
        then cxtSlurpyAny
        else CxtItem typ
inferExpCxt (Val {})                = return cxtItemAny
inferExpCxt (Var (sigil:_))         = return $ cxtOfSigil sigil
inferExpCxt (App (Var "&list") _ _) = return cxtSlurpyAny
inferExpCxt (App (Var "&item") _ _) = return cxtSlurpyAny
inferExpCxt (App (Var name) invs args)   = do
    -- inspect the return type of the function here
    env <- ask
    sub <- findSub name invs args
    return $ case sub of
        Right sub
            | isaType (envClasses env) "Scalar" (subReturns sub)
            -> CxtItem (subReturns sub)
        _ -> cxtSlurpyAny
inferExpCxt _                      = return cxtSlurpyAny
-}

{-|
Evaluate the \'magical\' variable associated with a given name. Returns 
@Nothing@ if the name does not match a known magical.
-}
getMagical :: Var -- ^ Name of the magical var to evaluate
           -> Eval (Maybe Val)
getMagical var = Map.findWithDefault (return Nothing) var magicalMap

magicalMap :: Map Var (Eval (Maybe Val))
magicalMap = Map.fromList
    [ (cast "$?FILE"     , posSym posName)
    , (cast "$?LINE"     , posSym posBeginLine)
    , (cast "$?COLUMN"   , posSym posBeginColumn)
    , (cast "$?POSITION" , posSym pretty)
    , (cast "$?MODULE"   , constSym "main")
    , (cast "$?OS"       , constSym (getConfig "osname"))
    , (cast "$?CLASS"    , fmap (Just . VType . cast) (asks envPackage))
    , (cast ":?CLASS"    , fmap (Just . VType . cast) (asks envPackage))
    , (cast "$?PACKAGE"  , fmap (Just . VType . cast) (asks envPackage))
    , (cast ":?PACKAGE"  , fmap (Just . VType . cast) (asks envPackage))
    , (cast "$?ROLE"     , fmap (Just . VType . cast) (asks envPackage))
    , (cast ":?ROLE"     , fmap (Just . VType . cast) (asks envPackage))
    ]

posSym :: Value a => (Pos -> a) -> Eval (Maybe Val)
posSym f = fmap (Just . castV . f) $ asks envPos

constSym :: String -> Eval (Maybe Val)
constSym = return . Just . VStr

findSyms :: Var -> Eval [(Var, Val)]
findSyms var
    | isGlobalVar var    = findWith findGlobal
    | isQualifiedVar var = case dropVarPkg (__"OUTER") var of
        Just var' -> do
            maybeOuter <- asks envOuter
            case maybeOuter of
                Just env -> local (const env) $ findSyms var'
                Nothing  -> return []
        _              -> findWith findQualified
    | otherwise         = findWith (findLexical `mplus` findPackage)
    where
    findWith f = runMaybeT f >>= maybe (return []) return

    -- $x should look up $x in the current pad first.
    findLexical :: MaybeT Eval [(Var, Val)]
    findLexical = do
        lex <- lift $ asks envLexical
        padSym lex var
        
    -- $x then fallbacks to $This::Package::x, or maybe $*x.
    findPackage :: MaybeT Eval [(Var, Val)]
    findPackage = do
        glob <- lift $ askGlobal
        pkg  <- lift $ asks envPackage
        padSym glob var
            `mplus` padSym glob (toPackage pkg var)
            `mplus` padSym glob (toGlobalVar var)

    -- $Foo::x is just $Foo::x, or maybe $*Foo::x.
    findQualified :: MaybeT Eval [(Var, Val)]
    findQualified = do
        glob <- lift $ askGlobal
        padSym glob var
            `mplus` padSym glob (toGlobalVar var)

    -- $*Foo::x is just that.
    findGlobal :: MaybeT Eval [(Var, Val)]
    findGlobal = do
        glob <- lift $ askGlobal
        padSym glob (toGlobalVar var)

    padSym :: Pad -> Var -> MaybeT Eval [(Var, Val)]
    padSym pad var = do
        case lookupPad var pad of
            Just tvar -> lift $ do
                refs <- liftSTM $ mapM readTVar tvar
                forM refs $ \ref -> do
                    val <- readRef ref
                    return (var, val)
            Nothing -> mzero
        
arityMatch :: VCode -> Int -> Int -> Maybe VCode
arityMatch sub@MkCode{ subAssoc = assoc, subParams = prms } argLen argSlurpLen
    | A_list <- assoc = Just sub
    | A_chain <- assoc = Just sub
    | isNothing $ find (not . isSlurpy) prms -- XXX - what about empty ones?
    , slurpLen <- length $ filter (\p -> isSlurpy p && v_sigil (paramName p) == SScalar) prms
    , hasArray <- isJust $ find (\p -> isSlurpy p && v_sigil (paramName p) /= SScalar) prms
    , if hasArray then slurpLen <= argSlurpLen else slurpLen == argSlurpLen
    = Just sub
    | reqLen <- length $ filter (\p -> not (isOptional p || isSlurpy p)) prms
    , optLen <- length $ filter (\p -> isOptional p) prms
    , hasArray <- isJust $ find (\p -> isSlurpy p && v_sigil (paramName p) /= SScalar) prms
    , argLen >= reqLen && (hasArray || argLen <= (reqLen + optLen))
    = Just sub
    | otherwise
    = Nothing

toPackage :: Pkg -> Var -> Var
toPackage pkg var
    | isGlobalVar var = var
    | otherwise       = var{ v_package = pkg }

packageOf :: Var -> Pkg
packageOf = v_package

toQualified :: Var -> Eval Var
toQualified var@MkVar{ v_twigil = TNil, v_package = pkg }
    | pkg == emptyPkg = do
        currentPkg <- asks envPackage
        return var{ v_package = currentPkg }
toQualified var = return var
