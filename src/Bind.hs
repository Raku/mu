{-# OPTIONS -fglasgow-exts #-}

{-
    Parameter binding.

    A star was bound upon her brows,
    A light was on her hair
    As sun upon the golden boughs
    In Lorien the fair...
-}

module Bind where
import Internals
import AST

type MaybeError a = Either String a

isRequired prm = not ( isOptional prm || isNamed prm )

bindNames :: [Exp] -> [Param] -> (Bindings, [Exp], [Param])
bindNames exps prms = (bound, exps', prms')
    where
    prms' = prms \\ (map fst bound)
    (bound, exps') = foldr doBind ([], []) (map unPair exps)
    doBind (name, exp) (bound, exps) 
        | Just prm <- find ((name ==) . tail . paramName) prms
        = ( ((prm, exp) : bound), exps )
        | otherwise
        = ( bound, (exp:exps) )

emptyHashExp :: Exp
emptyHashExp  = Val $ VHash $ vCast $ VList []
emptyArrayExp :: Exp
emptyArrayExp = Val $ VArray $ vCast $ VList []

bindHash :: [Exp] -> [Param] -> MaybeError Bindings
bindHash _ []          = return []
bindHash [] [p]         = return [ (p, emptyHashExp) ]
bindHash vs (p:ps@(_:_))= do
    first <- (bindHash vs [p])
    return $ first ++ (ps `zip` repeat emptyHashExp)
bindHash vs [p]         = return [ (p, App "circumfix:{}" [] vs) ] -- XXX cast to Hash

bindArray :: [Exp] -> [Param] -> MaybeError Bindings
bindArray vs ps = do
    case foldM (doBindArray (Syn "," vs)) ([],0) prms of
        Left errMsg     -> fail errMsg
        Right (bound,_) -> return $ reverse bound
    where
    prms = map (\p -> (p, (head (paramName p)))) ps 

doSlice :: Exp -> [VInt] -> Exp
doSlice v ns = Syn "[]" [v, Val $ VList $ map VInt ns]

-- XXX - somehow force failure
doIndex :: Exp -> VInt -> Exp
doIndex v n = Syn "cxt" [Val $ VStr "Scalar", Syn "[]" [v, Val $ VInt n]]

doBindArray :: Exp -> (Bindings, VInt) -> (Param, Char) -> MaybeError (Bindings, VInt)
doBindArray _ (xs, -1) (p, '@') = return (((p, emptyArrayExp):xs), -1)
doBindArray _ (_, -1) (p, '$') = fail $ "Slurpy array followed by slurpy scalar: " ++ show p
doBindArray v (xs, n)  (p, '@') = return (((p, doSlice v [n..]):xs), -1)
doBindArray v (xs, n)  (p, '$') = case v of
    (Syn "," [])    -> fail $ "Insufficient arguments for slurpy scalar"
    _               -> return (((p, doIndex v n):xs), n+1)
doBindArray _ (_, _)  (_, x) = internalError $ "doBindArray: unexpected char: " ++ (show x)

bindEmpty :: Param -> MaybeError (Param, Exp)
bindEmpty p = case paramName p of
    ('@':_) -> return (p, emptyArrayExp)
    ('$':_) -> fail $ "Unbound slurpy scalar: " ++ show p
    (x:_)   -> internalError $ "bindEmpty: unexpected char: " ++ (show x)
    []      -> internalError $ "bindEmpty: empty string encountered"

isPair :: Exp -> Bool
isPair (Syn "=>" [(Val _), _])   = True
isPair (Val (VPair (_, _)))      = True
isPair _                         = False

unPair :: Exp -> (String, Exp)
unPair (Syn "=>" [(Val k), exp]) = (vCast k, exp)
unPair (Val (VPair (k, v)))             = (vCast k, Val v)
unPair x                                = error ("Not a pair: " ++ show x)

-- performs a binding and then verifies that it's complete in one go
bindParams :: VSub -> [Exp] -> [Exp] -> MaybeError VSub
bindParams sub invsExp argsExp = do
    case bindSomeParams sub invsExp argsExp of
        Left errMsg -> Left errMsg
        Right boundSub -> finalizeBindings boundSub

-- verifies that all invocants and required params were given
-- and binds default values to unbound optionals
finalizeBindings :: VSub -> MaybeError VSub
finalizeBindings sub@Sub{ subParams = params, subBindings = bindings } = do
    let boundInvs = filter (\x -> isInvocant (fst x)) bindings -- bound invocants
        invocants = takeWhile isInvocant params                  -- expected invocants

    -- Check that we have enough invocants bound
    when (length boundInvs /= length invocants) $ do
        fail $ "Wrong number of invocant parameters: "
            ++ (show $ length boundInvs) ++ " actual, "
            ++ (show $ length invocants) ++ " expected"
   
    let (boundReq, boundOpt) = partition (\x -> isRequired (fst x)) bindings -- bound params which are required
        (reqPrms, optPrms)   = span isRequired params -- all params which are required, and all params which are opt

    -- Check length of required parameters
    when (length boundReq < length reqPrms) $ do
        fail $ "Insufficient number of required parameters: "
            ++ (show $ length boundReq) ++ " actual, "
            ++ (show $ length reqPrms) ++ " expected"

    let unboundOptPrms = optPrms \\ (map fst boundOpt) -- unbound optParams are allPrms - boundPrms
        optPrmsDefaults = map paramDefault $ unboundOptPrms -- get a list of default values
        boundDefOpts = unboundOptPrms `zip` (map Parens optPrmsDefaults) -- turn into exprs, so that +$y = $x will work
        
    return sub {
        subBindings = ((subBindings sub) ++ boundDefOpts)
    }

-- takes invocants and arguments, and creates a binding from the remaining params in the sub
bindSomeParams :: VSub -> [Exp] -> [Exp] -> MaybeError VSub
bindSomeParams sub@Sub{ subBindings = bindings, subParams = params } invsExp argsExp = do
    if (subName sub) == "&foo" then trace (unlines ["sub: " ++ (show (subName sub)), "binding: " ++ (show (map snd bindings)),  "invs: " ++ (show invsExp), "args: " ++ (show argsExp), "params :" ++ (show (map paramName (subParams sub)))]) $ return () else do return ()

    let (invPrms, argPrms) = span isInvocant params
        (givenInvs, givenArgs) = if null invPrms
            then ([], (invsExp++argsExp))
            else (invsExp, argsExp)

    let boundInv                = invPrms `zip` givenInvs -- invocants are just bound, params to given
        (namedArgs, posArgs)    = partition isPair givenArgs -- pairs are named arguments, they go elsewhere
        (boundNamed, namedForSlurp, allPosPrms) = bindNames namedArgs argPrms -- bind pair args to params. namedForSlup = leftover pair args
        (posPrms, slurpyPrms)   = break isSlurpy allPosPrms -- split any prms not yet bound, into regular and slurpy. allPosPrms = not bound by named
        boundPos                = posPrms `zip` posArgs -- bind all the unbound params in positional order
        posForSlurp             = drop (length posPrms) posArgs -- and whatever's left will be slurped

    -- Bind slurpy arrays and hashes
    let (slurpNamed, slurpPos) = partition (('%' ==) . head . paramName) slurpyPrms
        defaultPos      = if hasDefaultArray  then [] else [defaultArrayParam]
        defaultNamed    = if hasDefaultHash   then [] else [defaultHashParam]
        defaultScalar   = if hasDefaultScalar then [] else [] -- XXX - fetch from *@_
        hasDefaultArray = isJust (find (("@_" ==) . paramName) slurpPos)
                        || null slurpPos
        hasDefaultHash  = isJust (find (("%_" ==) . paramName) slurpNamed)
        hasDefaultScalar= isJust (find (("$_" ==) . paramName) params)
        
    boundHash   <- bindHash namedForSlurp (slurpNamed ++ defaultNamed) -- put leftover named args in %_
    boundArray  <- bindArray posForSlurp (slurpPos ++ defaultPos) -- put leftover positional args in @_
    boundScalar <- return $ defaultScalar `zip` (givenInvs ++ givenArgs) -- put, uh, something in $_

    let newBindings = concat [bindings, boundInv, boundNamed, boundPos, boundHash, boundArray, boundScalar]
    let newParams = params \\ (map fst newBindings);
    
    if (subName sub) == "&foo" then trace (unlines ["new binding: " ++ (show newBindings), "new params :" ++ (show (map paramName newParams)), "bound params :" ++ (show (map paramName (map fst newBindings)))]) $ return () else do return ()

    return sub
        { subBindings = newBindings
        , subParams   = newParams
        }
