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

bindNames :: [Exp] -> [Param] -> ([(Param, Exp)], [Exp], [Param])
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

bindHash :: [Exp] -> [Param] -> MaybeError [(Param, Exp)]
bindHash _ []          = return []
bindHash [] [p]         = return [ (p, emptyHashExp) ]
bindHash vs (p:ps@(_:_))= do
    first <- (bindHash vs [p])
    return $ first ++ (ps `zip` repeat emptyHashExp)
bindHash vs [p]         = return [ (p, App "circumfix:{}" [] vs) ] -- XXX cast to Hash

bindArray :: [Exp] -> [Param] -> MaybeError [(Param, Exp)]
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

doBindArray :: Exp -> ([(Param, Exp)], VInt) -> (Param, Char) -> MaybeError ([(Param, Exp)], VInt)
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

bindParams :: [Param] -> [Exp] -> [Exp] -> MaybeError [(Param, Exp)]
bindParams prms invsExp argsExp = do
    let (invocants, nameables) = span isInvocant prms
        (invs, args) = if null invocants
            then ([], (invsExp++argsExp))
            else (invsExp, argsExp)

    -- Check length of invocant parameters
    when (length invs /= length invocants) $ do
        fail $ "Wrong number of invocant parameters: "
            ++ (show $ length invs) ++ " actual, "
            ++ (show $ length invocants) ++ " expected"

    -- Bind invs to invocants, pairs to names
    let boundInv                = invocants `zip` invs
        (named, positional)     = partition isPair args
        (boundNamed, restNamed, restPrms) = bindNames named nameables
        (params, slurpy)        = break isSlurpy restPrms
        (required, optional)    = span isRequired params

    -- Check length of required parameters
    when (length positional < length required) $ do
        fail $ "Insufficient number of required parameters: "
            ++ (show $ length positional) ++ " actual, "
            ++ (show $ length required) ++ " expected"

    -- Bind positionals to requireds, defaults to optionals
    let (req, opt)  = length required `splitAt` positional
        boundReq    = required `zip` req
        defaults    = map paramDefault $ drop (length opt) optional
        optExps     = opt ++ defaults
        boundOpt    = optional `zip` optExps
        restPos     = drop (length optional) opt
    
    -- Bind slurpy arrays and hashes
    let (slurpNamed, slurpPos) = partition (('%' ==) . head . paramName) slurpy
        defaultPos      = if hasDefaultArray  then [] else [defaultArrayParam]
        defaultNamed    = if hasDefaultHash   then [] else [defaultHashParam]
        defaultScalar   = if hasDefaultScalar then [] else [defaultHashParam]
        hasDefaultArray = isJust (find (("@_" ==) . paramName) slurpPos)
                        || null slurpPos
        hasDefaultHash  = isJust (find (("%_" ==) . paramName) slurpNamed)
        hasDefaultScalar= isJust (find (("$_" ==) . paramName) prms)

    boundHash   <- bindHash restNamed (slurpNamed ++ defaultNamed)
    boundArray  <- bindArray restPos (slurpPos ++ defaultPos)
    boundScalar <- return $ defaultScalar `zip` (invs ++ args)

    return $ concat [boundInv, boundNamed, boundReq, boundOpt, boundHash, boundArray, boundScalar]
