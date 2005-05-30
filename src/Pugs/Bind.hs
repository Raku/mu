{-# OPTIONS_GHC -fglasgow-exts #-}

{-|
    Parameter binding.

>   A star was bound upon her brows,
>   A light was on her hair
>   As sun upon the golden boughs
>   In Lorien the fair...
-}

module Pugs.Bind where
import Pugs.Internals
import Pugs.AST
import Pugs.Types

{-|
Contains either a valid value of @a@ (@Right@), or a @String@ error
message (@Left@).
-}
type MaybeError a = Either String a

isRequired :: Param -> Bool
isRequired prm = not ( isOptional prm || isNamed prm )

{-|
Match up named arguments with named parameters, producing a list of new
bindings, and lists of remaining unbound args and params.
-}
bindNames :: [Exp] -- ^ List of argument expressions to be bound
          -> [Param] -- ^ List of parameters to try binding; includes both
                     --     named params and positional params
          -> (Bindings, [Exp], [Param]) -- ^ Bindings made;
                                        --   remaining (unbound) named args;
                                        --   remaining (positional) params
bindNames exps prms = (bound, exps', prms')
    where
    prms' = prms \\ (map fst bound)
    (bound, exps') = foldr doBind ([], []) (map unPair exps)
    doBind (name, exp) (bound, exps) 
        | Just prm <- find ((name ==) . tail . paramName) prms
        = ( ((prm, exp) : bound), exps )
        | otherwise
        = ( bound, (Syn "=>" [Val (VStr name), exp]:exps) )

emptyHashExp :: Exp
emptyHashExp  = Val $ VList [] -- VHash $ vCast $ VList []
emptyArrayExp :: Exp
emptyArrayExp = Val $ VList [] -- VArray $ vCast $ VList []

{-|
Create a binding from the slurpy hash parameter (e.g. @\*%_@) to a hash
containing all the remaining named arguments. If multiple slurpy hashes
are given, only the first gets the arguments--the rest get an empty hash.
Used by 'bindSomeParams'.
-}
bindHash :: [Exp]   -- ^ Named arguments (pair expressions) that were not
                    --     consumed by explicit named parameters
         -> [Param] -- ^ List of slurpy hash parameters
         -> MaybeError Bindings
bindHash _ []           = return []
bindHash [] [p]         = return [ (p, emptyHashExp) ]
bindHash vs (p:ps@(_:_))= do
    first <- (bindHash vs [p])
    return $ first ++ (ps `zip` repeat emptyHashExp)
bindHash vs [p]         = return [ (p, Syn "\\{}" [Syn "," vs]) ] -- XXX cast to Hash

{-|
Create bindings from the slurpy scalar and array parameters to the remaining
positional arguments. The first slurpy array param gets all of the remaining
args; subsequent slurpy array params get an empty array. Slurpy scalars may
not appear after slurpy array params.

Returns the bindings performed, and the sub's new 'SlurpLimit'.

Mostly uses 'doBindArray' to do its dirty work. Used by 'bindSomeParams'.

>[12:16] <scook0> autrijus: At the moment, if you call a sub that has multiple slurpy arrays, 
>                   Pugs deliberately binds the first one normally, and makes all the rest empty
>[12:17] <scook0> Is this proper behaviour, or is it just a quirk of the current implementation?
>[12:17] <autrijus> no, that's specced.
>[12:17] <autrijus> i.e. correct
-}
bindArray :: [Exp]      -- ^ List of slurpable argument expressions
          -> [Param]    -- ^ List of all slurpy positional params (scalar and array)
          -> SlurpLimit -- ^ The sub's current 'SlurpLimit'
          -> MaybeError (Bindings, SlurpLimit)
bindArray vs ps oldLimit = do
    let exp = Cxt cxtSlurpyAny (Syn "," vs)
    case foldM (doBindArray exp) ([], 0) prms of
        Left errMsg      -> fail errMsg
        Right (bound, n) -> do
            let newLimit = case prms of
                    ((_, '@'):_) -> oldLimit
                    _    | n > 0 -> (n, exp) : oldLimit
                    _            -> oldLimit
            return (reverse bound, newLimit)
    where
    prms = map (\p -> (p, (head (paramName p)))) ps 

{-|
Construct an expression representing an infinite slice of the given
array expression, beginning at element /n/ (i.e. @\@array\[\$n...\]@).

Used by 'doBindArray' to bind a slurpy array parameter to the rest of
the slurpable arguments.
-}
doSlice :: Exp -- ^ The array expression to slice
        -> VInt -- ^ Index of the first element in the resulting slice (/n/)
        -> Exp 
doSlice v n = Syn "[...]" [v, Val $ VInt n]

-- XXX - somehow force failure
{-|
Construct an expression representing element /n/ in the given array
expression (i.e. @\@array\[\$n\]@).

Used by 'doBindArray' to bind a particular slurpy scalar parameter to one of 
the slurpable arguments.
-}
doIndex :: Exp -> VInt -> Exp
doIndex v n = Syn "[]" [Syn "val" [v], Val $ VInt n]

doBindArray :: Exp -> (Bindings, VInt) -> (Param, Char) -> MaybeError (Bindings, VInt)
doBindArray _ (xs, -1) (p, '@') = return (((p, emptyArrayExp):xs), -1)
doBindArray _ (_, -1)  (p, '$') = fail $ "Slurpy array followed by slurpy scalar: " ++ show p
doBindArray v (xs, n)  (p, '@') = return (((p, doSlice v n):xs), -1)
doBindArray v (xs, n)  (p, '$') = case v of
    (Syn "," [])    -> fail $ "Insufficient arguments for slurpy scalar"
    _               -> return (((p, doIndex v n):xs), n+1)
doBindArray _ (_, _)  (_, x) = internalError $ "doBindArray: unexpected char: " ++ (show x)

{-|
(Does this even get used? It seems to be a leftover fragment of 
'doBindArray'...)
-}
bindEmpty :: Param -> MaybeError (Param, Exp)
bindEmpty p = case paramName p of
    ('@':_) -> return (p, emptyArrayExp)
    ('$':_) -> fail $ "Unbound slurpy scalar: " ++ show p
    (x:_)   -> internalError $ "bindEmpty: unexpected char: " ++ (show x)
    []      -> internalError $ "bindEmpty: empty string encountered"

{-|
Return @True@ if the given expression represents a pair (i.e. it uses the
\"=>\" pair constructor).
-}
isPair :: Exp -> Bool
isPair (Pos _ exp) = isPair exp
isPair (Cxt _ exp) = isPair exp
isPair (Syn "=>" [(Val _), _])   = True
isPair (App (Var "&infix:=>") Nothing [(Cxt _ (Val _)), _])   = True
isPair (App (Var "&infix:=>") Nothing [(Val _), _])   = True
isPair _                         = False

{-|
Decompose a pair-constructor 'Exp'ression (\"=>\") into a Haskell pair
(@key :: 'String'@, @value :: 'Exp'@).
-}
unPair :: Exp -> (String, Exp)
unPair (Pos _ exp) = unPair exp
unPair (Cxt _ exp) = unPair exp
unPair (Syn "=>" [(Val k), exp]) = (vCast k, exp)
unPair (App (Var "&infix:=>") Nothing [(Cxt _ (Val k)), exp]) = (vCast k, exp)
unPair (App (Var "&infix:=>") Nothing [(Val k), exp]) = (vCast k, exp)
unPair x                                = error ("Not a pair: " ++ show x)

{-|
Bind parameters to a callable, then verify that the binding is complete
(i.e. all mandatory params are bound; all unspecified params have default
bindings).

Uses 'bindSomeParams' to perform the initial binding, then uses
'finalizeBindings' to check all required params and give default values to
any unbound optional ones. Once this is complete, /everything/ should be
bound.

Note that while 'bindParams' produces values /representing/ the bindings from
params to args, it does not actually introduce any symbols--that occurs later
on in the call process.
-}
bindParams :: VCode       -- ^ A code object to perform bindings on
           -> (Maybe Exp) -- ^ (Optional) explicit invocant
           -> [Exp]       -- ^ List of arguments (actual params) to bind
           -> MaybeError VCode -- ^ Returns either a new 'VCode' with all the
                               --     bindings in place, or an error message
bindParams sub invExp argsExp = do
    case bindSomeParams sub invExp argsExp of
        Left errMsg -> Left errMsg
        Right boundSub -> finalizeBindings boundSub

{-|
Verify that all invocants and required parameters are bound, and give default
values to any unbound optional parameters.
-}
finalizeBindings :: VCode -> MaybeError VCode
finalizeBindings sub = do
    let params    = subParams sub
        bindings  = subBindings sub
        boundInvs = filter (\x -> isInvocant (fst x)) bindings -- bound invocants
        invocants = takeWhile isInvocant params                -- expected invocants

    -- Check that we have enough invocants bound
    when (not $ null invocants) $ do
        let cnt = length invocants
            act = length boundInvs
        fail $ "Wrong number of invocant parameters: "
            ++ (show $ act) ++ " actual, "
            ++ (show $ act + cnt) ++ " expected"
            
    let (boundReq, boundOpt) = partition (\x -> isRequired (fst x)) bindings -- bound params which are required
        (reqPrms, optPrms)   = span isRequired params -- all params which are required, and all params which are opt

    -- Check length of required parameters
    when (length boundReq < length reqPrms) $ do
        fail $ "Insufficient number of required parameters: "
            ++ (show $ length boundReq) ++ " actual, "
            ++ (show $ length reqPrms) ++ " expected"

    let unboundOptPrms = optPrms \\ (map fst boundOpt) -- unbound optParams are allPrms - boundPrms
        optPrmsDefaults = [ Syn "default" [paramDefault prm] | prm <- unboundOptPrms ] -- get a list of default values
        boundDefOpts = unboundOptPrms `zip` optPrmsDefaults -- turn into exprs, so that +$y = $x will work
        
    return sub {
        subBindings = ((subBindings sub) ++ boundDefOpts)
    }

{-|
Take a code object and lists of invocants and arguments, and produce (if
possible) a new 'VCode' value representing the same code object, with as many
parameters bound as possible (using the given invocants and args).
-}
bindSomeParams :: VCode       -- ^ Code object to perform bindings on
               -> (Maybe Exp) -- ^ Explicit invocant expression
               -> [Exp]       -- ^ List of argument expressions
               -> MaybeError VCode -- ^ A new 'VCode' structure, augmented
                                   --     with the new bindings
bindSomeParams sub invExp argsExp = do
    let params     = subParams sub
        bindings   = subBindings sub
        slurpLimit = subSlurpLimit sub
        (invPrms, argPrms) = span isInvocant params
        (givenInvs, givenArgs) = if null invPrms
            then ([], (maybeToList invExp++argsExp))
            else (maybeToList invExp, argsExp)

    let boundInv                = invPrms `zip` givenInvs -- invocants are just bound, params to given
        (namedArgs, posArgs)    = partition isPair givenArgs -- pairs are named arguments, they go elsewhere
        (boundNamed, namedForSlurp, allPosPrms) = bindNames namedArgs argPrms -- bind pair args to params. namedForSlup = leftover pair args
        (posPrms, slurpyPrms)   = break isSlurpy allPosPrms -- split any prms not yet bound, into regular and slurpy. allPosPrms = not bound by named
        boundPos                = posPrms `zip` posArgs -- bind all the unbound params in positional order
        posForSlurp             = drop (length posPrms) posArgs -- and whatever's left will be slurped

    -- Bind slurpy arrays and hashes
    let (slurpNamed, slurpPos) = partition (('%' ==) . head . paramName) slurpyPrms
        -- defaultPos      = if hasDefaultArray  then [] else [defaultArrayParam]
        defaultNamed    = if hasDefaultHash   then [] else [defaultHashParam]
        defaultScalar   = if hasDefaultScalar then [] else [] -- XXX - fetch from *@_
        hasDefaultHash  = isJust (find (("%_" ==) . paramName) slurpNamed)
        hasDefaultScalar= isJust (find (("$_" ==) . paramName) params)
        
    boundHash   <- bindHash namedForSlurp (slurpNamed ++ defaultNamed) -- put leftover named args in %_
    (boundArray, newSlurpLimit) <- bindArray posForSlurp slurpPos slurpLimit
    boundScalar <- return $ defaultScalar `zip` (givenInvs ++ givenArgs) -- put, uh, something in $_

    let newBindings = concat [bindings, boundInv, boundNamed, boundPos, boundHash, boundArray, boundScalar]
    let newParams = params \\ (map fst newBindings);
    
    return sub
        { subBindings   = newBindings
        , subParams     = newParams
        , subSlurpLimit = newSlurpLimit
        }

