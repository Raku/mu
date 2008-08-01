{-# OPTIONS_GHC -fglasgow-exts -fallow-overlapping-instances -funbox-strict-fields -fparr #-}

module Pugs.Eval.Var (
    findVar, findVarRef, findSub,
    inferExpType,  inferExpCxt, FindSubFailure(..),
    packageOf, toPackage, toQualified,
) where
import qualified Data.Map as Map
import qualified StringTable.AtomMap as AtomMap
import Pugs.Internals
import Pugs.AST
import Pugs.Types
import Pugs.Bind
import Pugs.Prim.List (op2Reduce, op1HyperPrefix, op1HyperPostfix, op2Hyper)
import Pugs.Prim.Param (foldParam)
import Pugs.Pretty
import Pugs.Config
import Pugs.Monads
import Pugs.Class hiding (Val)
import qualified Pugs.Val as Val
import qualified Data.ByteString.Char8 as Buf

findVar :: Var -> Eval (Maybe VRef)
findVar var
    | SType <- v_sigil var
    , not (isGlobalVar var)
    = return Nothing
    | otherwise = do
        rv <- findVarRef var
        case rv of
            Just ref    -> fmap Just (readPadEntry ref)
            Nothing     -> return Nothing

constPadEntry :: VRef -> PadEntry
constPadEntry r = PEConstant{ pe_type = refType r, pe_proto = r, pe_flags = mempty }

lookupShellEnvironment :: ByteString -> Eval (Maybe PadEntry)
lookupShellEnvironment name = do
    exists <- evalExp $ App (_Var "&exists") (Just (_Var "%*ENV")) [Val (VStr $ cast name)]
    case exists of
        VBool False -> do
            die "no such ENV variable" name
        _           -> do
            rv   <- enterLValue (evalExp $ Syn "{}" [_Var "%*ENV", Val (VStr $ cast name)])
            ref  <- fromVal rv
            return (Just (constPadEntry ref))

findVarRef :: Var -> Eval (Maybe PadEntry)
findVarRef var@MkVar{ v_sigil = sig, v_twigil = twi, v_name = name, v_package = pkg }
    | Just var' <- dropVarPkg (__"CALLER") var = do
        maybeCaller <- asks envCaller
        case maybeCaller of
            Just env -> local (const env) $ findVarRef var'
            Nothing -> die "cannot access CALLER:: in top level" var

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

    | Just var' <- dropVarPkg (__"OUTER") var = ($ var') . fix $ \outerLevel v -> do
        mpads <- asks envLexPads
        case mpads of
            (_:outers@(outer:_))  -> local (\env -> env{ envLexPads = outers }) $ do
                case dropVarPkg (__"OUTER") v of
                    Just v' -> outerLevel v'
                    _       -> do
                        pad <- case outer of
                            PRuntime p  -> return p
                            PCompiling p-> readMPad p
                        return (lookupPad v pad)
            _       -> die "cannot access OUTER:: in top level" name

    | pkg /= emptyPkg = doFindVarRef var

    | TMagical <- twi = do
        rv  <- getMagical var
        case rv of
            Nothing  -> doFindVarRef var
            Just val -> return (Just (constPadEntry (MkRef . constScalar $ val)))

    | SHash <- sig, nullID == name = do
        {- %CALLER::, %OUTER::, %Package::, etc, all recurse to here. -}
        pad <- asks envLexical
        let plist   = padToList pad
        hlist <- mapM padEntryToHashEntry plist
        let hash    = IHash $ Map.fromList hlist
        return $ Just (constPadEntry $ MkRef hash)
    | otherwise = doFindVarRef var
    where
    padEntryToHashEntry :: (Var, PadEntry) -> Eval (VStr, Val)
    padEntryToHashEntry (key, entry) = do
        vref   <- readPadEntry entry
        let val = VRef vref
        return (cast key, val)

doFindVarRef :: Var -> Eval (Maybe PadEntry)
doFindVarRef var = do
    lexSym  <- fmap (lookupPad var . envLexical) ask
    if isJust lexSym then return lexSym else do
    -- XXX - this is bogus; we should not fallback if it's not in lex scope.
    glob    <- readMPad . envGlobal =<< ask
    var'    <- toQualified var
    let globSym = lookupPad var' glob
    if isJust globSym then return globSym else do
    -- XXX - ditto for globals
    let globSym = lookupPad (toGlobalVar var) glob
    if isJust globSym then return globSym else do
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

_NEXT :: ByteString
_NEXT = __"NEXT"


-- This no longer handles multi dispatch now. Yay!
findSub :: Var        -- ^ Name, with leading @\&@.
        -> Maybe Exp  -- ^ Invocant
        -> [Exp]      -- ^ Other arguments
        -> Eval (Either FindSubFailure VCode)
findSub _var _invs _args
    | Nothing <- _invs = do
        findBuiltinSub NoMatchingMulti _var
    | not (isQualifiedVar _var) = do
        case unwrap _inv of
            Val vv@VV{}     -> withExternalCall callMethodVV vv
            Val sv@PerlSV{} -> withExternalCall callMethodVV sv
            inv' -> do
                typ <- evalInvType inv'
                if typ == mkType "Scalar::Perl5" -- code for "VV"
                    then evalExp inv' >>= withExternalCall callMethodVV
                    else findTypedSub (cast typ) _var
    | Just var' <- dropVarPkg _SUPER _var = do
        pkg <- asks envPackage
        findSuperSub pkg var'
    | Just var' <- dropVarPkg _NEXT _var = do
        typ <- evalInvType _inv
        findSuperSub (cast typ) var'
    | otherwise = do
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
            Right sub | cast (Buf.cons '&' $ subName sub) == var{ v_package = pkg } -> do
                return (Left . NoSuchMethod $ cast pkg)
            _   -> do
                return subs'

    -- findTypedSub :: (_var :: Var, _invs :: Maybe Exp, _args :: [Exp])
    --     => Pkg -> Var -> Eval (Either FindSubFailure VCode)
    findTypedSub pkg var = do
        subs    <- findWithPkg pkg var
        either (flip findBuiltinSub var) (return . Right) subs

    evalInvType :: Exp -> Eval Type
    evalInvType x = inferExpType x

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
                    fmap AtomMap.fromList $ forM list $ \(k, v) -> do
                        key <- fromVal k
                        val <- fromVal v
                        return (key, val)   :: Eval (ID, Val.Val)
                rv <- tryT $ VV invVV ./ (methName, posVVs, namVVs)
                case rv of
                    VError (VStr s) _
                        | "Can't locate object method" `isPrefixOf` s || "Can't call method" `isPrefixOf` s -> do
                        let capt = mi_arguments (cast (methName, (invVV:posVVs), namVVs) :: Call)
                        rv' <- tryT . evalExp $ App (Var _var) Nothing [Syn "|" [Val (VV (mkVal capt))]]
                        case rv' of
                            VError (VStr s') _ | "No compatible subroutine found" `isPrefixOf` s' -> EvalT $ return (RException rv)
                            VError{} -> EvalT $ return (RException rv')
                            _ -> return rv'
                    VError{} -> EvalT $ return (RException rv)
                    _ -> return rv
            }

    -- callMethodPerl5 :: (_var :: Var, _invs :: Maybe Exp, _args :: [Exp])
    --     => Eval (Maybe VCode)
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
        -- XXX - "reverse" below is a crude hack before we have C3 dispatch;
        --     - this is such that "class X is Object is Moose" can dispatch with Moose first.
        (`fix` (reverse $ fromJust attrs)) $ \run pkgs -> do
            if null pkgs then return (Left $ NoSuchMethod (cast pkg)) else do
            subs <- findWithPkg (head pkgs) var
            either (const $ run (tail pkgs)) (return . Right) subs

    -- findSub' :: (_var :: Var, _invs :: Maybe Exp, _args :: [Exp]) => Var -> Eval (Maybe VCode)
    findSub' var = do
        subSyms     <- findCodeSyms var
        lens        <- mapM argSlurpLen _invs_args
        doFindSub lens subSyms

    argSlurpLen :: Exp -> Eval Int
    argSlurpLen (Val val) = valSlurpLen val
    argSlurpLen (Var name) = do
        val <- enterLValue $ evalExp (Var name)
        valSlurpLen val
    argSlurpLen (Syn "," list) = return $ length list
    argSlurpLen (Syn "named" _) = return 0
    argSlurpLen _ = return 1 -- XXX

    valSlurpLen :: Val -> Eval Int
    valSlurpLen (VList list) = return $ length list
    valSlurpLen (VRef (MkRef (IArray av))) = array_fetchSize av
    valSlurpLen (VRef (MkRef (IHash hv))) = hash_fetchSize hv
    valSlurpLen _  = return 1 -- XXX

    -- doFindSub :: (_var :: Var, _invs :: Maybe Exp, _args :: [Exp])
    --     => Int -> [(Var, Val)] -> Eval (Maybe VCode)
    doFindSub slurpLens subSyms = do
        subs' <- subs slurpLens subSyms
        -- warn (unlines $ map (\(x, y) -> show (x, subParams y)) subs') ""
        return $ case sort subs' of
            ((_, sub):_)    -> Just sub
            _               -> Nothing

    _invs_args = map unwrap (maybe _args (:_args) _invs)

    -- subs :: (_invs :: Maybe Exp, _args :: [Exp])
    --     => Int -> [(Var, Val)] -> Eval [((Bool, Bool, Int, Int), VCode)]
    subs slurpLens subSyms = fmap catMaybes . forM subSyms $ \sub@MkCode{ subReturns = ret } -> do
        let (named, positional) = partition isNamedArg _invs_args
            isNamedArg (Syn "named" _) = True
            isNamedArg _               = False
            rv = return $ arityMatch sub (length positional) (length named) slurpLens

        maybeM rv $ \fun -> do
            -- if deltaFromCxt ret == 0 then return Nothing else do
            (deltaArgs, deltaCxt) <- case bindParams sub _invs _args of
                Left{}  -> return ([maxBound], 0)
                Right s -> do
                    ds  <- forM (subBindings s) $ \(prm, arg) -> case arg of
                        Syn "param-default" _   -> return Nothing
                        _  | isSlurpy prm       -> return Nothing
                        _                       -> do
                            argType <- inferExpType arg
                            return (Just $ deltaType (typeOfParam prm) argType)
                    cxt <- asks envContext
                    return (catMaybes ds, deltaType (typeOfCxt cxt) ret)
            return ((isMulti sub, sum deltaArgs, -(length deltaArgs), deltaCxt), fun)

    -- findBuiltinSub :: (_var :: Var, _invs :: Maybe Exp, _args :: [Exp])
    --     => FindSubFailure -> Var -> Eval (Either FindSubFailure VCode)
    findBuiltinSub failure var = do
        subSyms     <- findCodeSyms var
        if null subSyms then (fmap (err NoSuchSub) (possiblyBuildMetaopVCode var)) else do
        lens        <- mapM argSlurpLen _invs_args
        sub         <- doFindSub lens subSyms
        maybe (fmap (err failure) $ possiblyBuildMetaopVCode var) (return . Right) sub

    -- firstArg :: (_args :: [Exp]) => [Exp]
    firstArg = [maybe (Val undef) id (listToMaybe _args)]
    firstTwoArgs
        | [] <- _args       = [Val undef, Val undef]
        | [arg] <- _args    = [arg, Val undef]
        | otherwise         = take 2 _args

    metaPrim = mkPrim
        { subName = cast (v_name _var)
        , subType     = SubPrim
        , subReturns  = mkType "List"
        }

    buildPrefixHyper var = do
        let rv = fmap (either (const Nothing) Just) $
                findSub var Nothing firstArg
        maybeM rv (return . makePrefixHyperCode)
        
    makePrefixHyperCode code = metaPrim
        { subAssoc    = subAssoc code
        , subParams   = subParams code
        , subBody     = Prim
            (\x -> op1HyperPrefix code (listArg x))
        }

    buildPostfixHyper var = do
        let rv = fmap (either (const Nothing) Just) $
                findSub var Nothing firstArg
        maybeM rv $ \code -> return $ metaPrim
            { subAssoc    = subAssoc code
            , subParams   = subParams code
            , subBody     = Prim
                (\x -> op1HyperPostfix code (listArg x))
            }

    buildInfixHyper var = do
        let rv = fmap (either (const Nothing) Just) $
                findSub var Nothing firstTwoArgs
        maybeM rv (return . makeInfixHyperCode)

    makeInfixHyperCode code = metaPrim
        { subAssoc    = subAssoc code
        , subParams   = makeParams ["Any", "Any"]
        , subBody     = Prim (\[x, y] -> op2Hyper code x y)
        }

    buildReduce var foldOrScan nilOrHyper nilOrPost = do
        let rv = fmap (either (const Nothing) Just) $
                findSub var Nothing firstTwoArgs
        maybeM rv $ \code -> return . maybePost $ metaPrim
            { subAssoc    = ANil
            , subParams   = makeParams $
                if any isLValue (subParams code)
                    then ["rw!List"] -- XXX - does not yet work for the [=] case
                    else ["List"]
            , subReturns  = anyType
            , subBody     = Prim $ \[vs] -> do
                list_of_args <- fromVal vs
                op2Reduce (foldOrScan == MScan) list_of_args . VCode $ case nilOrHyper of
                    MHyper  -> makeInfixHyperCode code
                    _       -> code
            }
        where
        maybePost 
            | MPost <- nilOrPost    = makePrefixHyperCode
            | otherwise             = id

    -- possiblyBuildMetaopVCode :: (_args :: [Exp]) => Var -> Eval (Maybe VCode)
    possiblyBuildMetaopVCode var@MkVar{ v_meta = meta } = case meta of
        MPost           -> buildPrefixHyper var'                    -- +<<
        MPre            -> buildPostfixHyper var'                   -- >>+
        MHyper          -> buildInfixHyper var'                     -- >>+<<
        MFold           -> buildReduce varInfix MFold MNil MNil     -- [+]
        MScan           -> buildReduce varInfix MScan MNil MNil     -- [\+]
--      MFoldPost       -> buildReduce varInfix MFold MNil MPost    -- [+]
--      MScanPost       -> buildReduce varInfix MScan MNil MPost    -- [\+]
        MHyperFold      -> buildReduce varInfix MFold MHyper MNil   -- [>>+<<]
        MHyperScan      -> buildReduce varInfix MScan MHyper MNil   -- [>>+<<]
--      MHyperFoldPost  -> buildReduce varInfix MFold MHyper MPost  -- [>>+<<]
--      MHyperScanPost  -> buildReduce varInfix MScan MHyper MPost  -- [>>+<<]
        _               -> return Nothing
        where
        var' = var{ v_meta = MNil }
        varInfix = var{ v_meta = MNil, v_categ = C_infix }

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

findAttrs :: Pkg -> Eval (Maybe [Pkg])
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
                if isaType "List" typ
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
    return (either (const anyType) subReturns sub)
inferExpType (Ann (Cxt cxt) _) | typeOfCxt cxt /= (mkType "Any") = return $ typeOfCxt cxt
inferExpType (Ann _ exp) = inferExpType exp
inferExpType (Sym _ _ _ _ exp) = inferExpType exp
inferExpType (Stmts _ exp) = inferExpType exp
inferExpType (Syn "," _)    = return $ mkType "List"
inferExpType (Syn "\\[]" _) = return $ mkType "Array"
inferExpType (Syn "\\{}" _) = return $ mkType "Hash"
inferExpType (Syn "&{}" _)  = return $ mkType "Code"
inferExpType (Syn "@{}" _)  = return $ mkType "Array"
inferExpType (Syn "%{}" _)  = return $ mkType "Hash"
inferExpType (Syn "=>" _)   = return $ mkType "Pair"
inferExpType (Syn "named" [_, exp])   = inferExpType exp
inferExpType (Syn "rx" _)   = return $ mkType "Regex"
inferExpType (Syn "match" _)= return $ mkType "Match"
inferExpType (Syn "//" _)   = return $ mkType "Regex" -- XXX Wrong
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

{-|
Evaluate the \'magical\' variable associated with a given name. Returns 
@Nothing@ if the name does not match a known magical.
-}
getMagical :: Var -- ^ Name of the magical var to evaluate
           -> Eval (Maybe Val)
getMagical var = Map.findWithDefault (return Nothing) var magicalMap

magicalMap :: Map Var (Eval (Maybe Val))
magicalMap = Map.fromList
    [ (cast "$?FILE"     , posSym ((cast . posName) :: Pos -> String))
    , (cast "$?LINE"     , posSym posBeginLine)
    , (cast "$?COLUMN"   , posSym posBeginColumn)
    , (cast "$?POSITION" , posSym pretty)
    , (cast "$?MODULE"   , constSym "Main")
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

-- Find symbols, up and including multis.
findCodeSyms :: Var -> Eval [VCode]
findCodeSyms var
    | isGlobalVar var    = findWith findGlobal
    | isQualifiedVar var = case dropVarPkg (__"OUTER") var of
        Just var'       -> die "outer - not yet implemented" var'
        _               -> findWith findQualified
    | otherwise          = do
        rv <- findWith findLexical
        if null rv then findWith findPackage else return rv
    where
    findWith f = runMaybeT f >>= maybe (return []) return

    -- $x should look up $x in the current pad first.
    findLexical :: MaybeT Eval [VCode]
    findLexical = do
        lex <- lift $ asks envLexical
        padSym lex var
        
    -- $Foo::x is just $Foo::x, or maybe $*Foo::x.
    findQualified :: MaybeT Eval [VCode]
    findQualified = do
        glob <- lift $ askGlobal
        padSym glob var
            `mplus` padSym glob (toGlobalVar var)

    -- $x then fallbacks to $This::Package::x, or maybe $*x.
    findPackage :: MaybeT Eval [VCode]
    findPackage = do
        -- XXX - This is bogus; pending Pad fixup code in Pugs.Parser
        glob <- lift $ askGlobal
        pkg  <- lift $ asks envPackage
        padSym glob (toPackage pkg var)
            `mplus` padSym glob (toGlobalVar var)

    -- $*Foo::x is just that.
    findGlobal :: MaybeT Eval [VCode]
    findGlobal = do
        glob <- lift $ askGlobal
        padSym glob (toGlobalVar var)

    padSym :: Pad -> Var -> MaybeT Eval [VCode]
    padSym pad var = do
        case lookupPad var pad of
            Just entry -> lift $ do
                ref     <- readPadEntry entry
                readCodesFromRef ref
            Nothing -> mzero

data ArityMatchData = MkArityMatchData
    { d_reqLen      :: !Int
    , d_optLen      :: !Int
    , d_slurpLen    :: !Int
    , d_hasArray    :: !Bool
    , d_hasHash     :: !Bool
    }

arityMatch :: VCode -> Int -> Int -> [Int] -> Maybe VCode
arityMatch sub@MkCode{ subAssoc = assoc, subParams = prms } posLen namLen argSlurpLens
    | A_list    <- assoc = Just sub
    | A_chain   <- assoc = Just sub

    | argLen >= reqLen
    , hasArray || ((if hasHash then posLen else argLen) <= (reqLen + optLen + slurpLen))
    , if hasArray then slurpLen <= argSlurpLen else slurpLen == argSlurpLen
    = Just sub

    | otherwise
    = Nothing
    where
    argLen      = posLen + namLen
    argSlurpLen = sum (drop (reqLen + optLen) argSlurpLens)
    ~(MkArityMatchData reqLen optLen slurpLen hasArray hasHash) = foldl unwindPrm initArityMatchData prms

initArityMatchData :: ArityMatchData
initArityMatchData = MkArityMatchData 0 0 0 False False

unwindPrm :: ArityMatchData -> Param -> ArityMatchData
unwindPrm dat p
    | isSlurpy p = case v_sigil (paramName p) of
        SArray  -> dat{ d_hasArray = True }
        SHash   -> dat{ d_hasHash  = True }
        _       -> dat{ d_slurpLen = succ (d_slurpLen dat) }
    | isOptional p  = dat{ d_optLen = succ (d_optLen dat) }
    | otherwise     = dat{ d_reqLen = succ (d_reqLen dat) }

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

