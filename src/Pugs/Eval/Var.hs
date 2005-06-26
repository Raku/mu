{-# OPTIONS_GHC -fglasgow-exts -cpp #-}
{-# OPTIONS_GHC -#include "UnicodeC.h" #-}

module Pugs.Eval.Var (
    findVar, findVarRef,
    evalVar, findSub,, evalExpType,
) where
import qualified Data.Map as Map
import Pugs.Internals
import Pugs.AST
import Pugs.Types
import Pugs.Embed.Perl5
import Pugs.Bind
import Pugs.Prim.List (op2Fold, op1HyperPrefix, op1HyperPostfix, op2Hyper)
import Pugs.Prim.Param (foldParam)
import Pugs.Context
import Pugs.Pretty
import Pugs.Config

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
    | (_:'?':_) <- name = do
        rv  <- getMagical name
        case rv of
            Nothing  -> doFindVarRef name
            Just val -> do
                tvar <- liftSTM $ newTVar (MkRef . constScalar $ val)
                return $ Just tvar
    | "%" <- name = do
        {- %CALLER::, %OUTER::, %Package::, etc, all recurse to here. -}
        pad <- asks envLexical
        let plist   = padToList pad
        hlist <- mapM padEntryToHashEntry plist
        let hash    = IHash $ Map.fromList hlist
        let hashref = MkRef hash
        tvar <- liftSTM $ newTVar hashref
        return $ Just tvar
    | otherwise = doFindVarRef name
    where
    padEntryToHashEntry :: (Var, [(TVar Bool, TVar VRef)]) -> Eval (VStr, Val)
    padEntryToHashEntry (key, (_, tvref) : _) = do
        vref   <- liftSTM (readTVar tvref)
        let val = VRef vref
        return (key, val)
    padEntryToHashEntry (_, []) = do fail "Nonexistant var in pad?"
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

findSub :: String     -- ^ Name, with leading @\&@.
        -> Maybe Exp  -- ^ Invocant
        -> [Exp]      -- ^ Other arguments
        -> Eval (Maybe VCode)
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
                    invokePerl5 subSV sv svs envSV (enumCxt $ envContext env)
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
        fmap length (fromVal listVal :: Eval [Val])
    argSlurpLen (Var name) = do
        listMVal <- evalVar name
        listVal  <- fromVal listMVal
        fmap length (fromVal listVal :: Eval [Val])
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
evalExpType (Syn "," _)    = return $ mkType "List"
evalExpType (Syn "\\[]" _) = return $ mkType "Array"
evalExpType (Syn "\\{}" _) = return $ mkType "Hash"
evalExpType (Syn "&{}" _)  = return $ mkType "Code"
evalExpType (Syn "@{}" _)  = return $ mkType "Array"
evalExpType (Syn "%{}" _)  = return $ mkType "Hash"
evalExpType _ = return $ mkType "Any"

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
getMagical ":?CLASS"    = fmap (Just . VType . mkType) (asks envPackage)
getMagical "$?PACKAGE"  = fmap (Just . VType . mkType) (asks envPackage)
getMagical ":?PACKAGE"  = fmap (Just . VType . mkType) (asks envPackage)
getMagical "$?ROLE"     = fmap (Just . VType . mkType) (asks envPackage)
getMagical ":?ROLE"     = fmap (Just . VType . mkType) (asks envPackage)
getMagical _            = return Nothing

posSym :: Value a => (Pos -> a) -> Eval (Maybe Val)
posSym f = fmap (Just . castV . f) $ asks envPos
constSym :: String -> Eval (Maybe Val)
constSym = return . Just . castV

findSyms :: Var -> Eval [(String, Val)]
findSyms name = do
    lex  <- asks envLexical
    glob <- askGlobal
    pkg  <- fromVal =<< readVar "$*PACKAGE"
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

toGlobal :: String -> String
toGlobal name
    | (sigil, identifier) <- break (\x -> isAlpha x || x == '_') name
    , last sigil /= '*'
    = sigil ++ ('*':identifier)
    | otherwise = name

evalVar :: Var -> Eval Val
evalVar name = do
    v <- findVar name
    case v of
        Just var -> readRef var
        _ | (':':rest) <- name -> return $ VType (mkType rest)
        _ -> retError "Undeclared variable" name

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

toPackage :: String -> String -> String
toPackage pkg name
    | (sigil, identifier) <- break (\x -> isAlpha x || x == '_') name
    , last sigil /= '*'
    = concat [sigil, pkg, "::", identifier]
    | otherwise = name

