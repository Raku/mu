{-# OPTIONS -fglasgow-exts #-}

{-
    Evaluation and reduction engine.

    Home is behind, the world ahead,
    And there are many paths to tread
    Through shadows to the edge of night,
    Until the stars are all alight.
    Then world behind and home ahead,
    We'll wander back to home and bed...
-}

module Eval where
import Internals

import AST
import Bind
import Prim
import Context

data Env = Env { cxt :: Cxt
               , sym :: Symbols
               , cls :: ClassTree
               } deriving (Show)
emptyEnv = Env { cxt = "List"
               , sym = initSyms
               , cls = initTree
               }

addSym :: Env -> [(String, Val)] -> Env
addSym env [] = env
addSym env ((var, val):vs) = env{ sym = (var, val):(sym $ addSym env vs) }

evaluate :: Env -> Exp -> Val
evaluate env@Env{ cxt = cxt, cls = cls } exp
    | Val v <- val  = v
    | otherwise     = VError "Invalid expression" exp
    where
    (env', val) = reduce env exp
    isaContext = isaType cls cxt

-- OK... Now let's implement the hideously clever autothreading algorithm.
-- First pass - thread thru all() and none()
-- Second pass - thread thru any() and one()

juncType :: Val -> Maybe (JuncType, [Val])
juncType v
    | VJunc j l <- v
    = Just (j, l)
    | otherwise
    = Nothing

juncTypeIs :: Val -> [JuncType] -> Maybe (JuncType, [Val])
juncTypeIs v js
    | Just (j, l) <- juncType v
    , j `elem` js
    = Just (j, l)
    | otherwise
    = Nothing

chainFun :: Env -> Params -> Exp -> Params -> Exp -> [Val] -> Val
chainFun env p1 f1 p2 f2 (v1:v2:vs)
    | VBool False <- applyFun env (chainArgs p1 [v1, v2]) f1
    = VBool False
    | otherwise
    = applyFun env (chainArgs p1 (v2:vs)) f2
    where
    chainArgs prms vals = map chainArg (prms `zip` vals)
    chainArg (p, v) = ApplyArg (paramName p) v False

data ApplyArg = ApplyArg
    { argName       :: String
    , argValue      :: Val
    , argCollapsed  :: Bool
    }
    deriving (Show, Eq, Ord)

applyFun :: Env -> [ApplyArg] -> Exp -> Val
applyFun env bound (Prim f) = f [ argValue arg | arg <- bound, (argName arg !! 1) /= '_' ]
applyFun env bound body
    | Val val   <- exp          = val
    | otherwise                 = VError "Invalid expression" exp
    where
    (fenv, exp) = reduce (env `addSym` formal) body
    formal = filter (not . null . fst) $ map argNameValue bound
    argNameValue (ApplyArg name val _) = (name, val)

apply :: Env -> VSub -> [Exp] -> [Exp] -> ((Env -> Env), Exp)
apply env@Env{ cls = cls } Sub{ subParams = prms, subFun = fun } invs args =
    case bindParams prms invs args of
        Left errMsg     -> retVal $ VError errMsg (Val VUndef)
        Right bindings  -> retVal $ juncApply eval (reverse . fst $ foldl doBind ([],env) bindings)
    where
    eval bound = applyFun env bound fun
    doBind :: ([ApplyArg], Env) -> (Param, Exp) -> ([ApplyArg], Env)
    doBind (bs, env) (prm@Param{ paramName = name, paramContext = cxt}, exp) =
        let (val, coll) = expToVal env cxt exp in
        (((ApplyArg name val coll): bs), env `addSym` [(name, val)])
    expToVal env cxt exp = (evaluate env{ cxt = cxt } exp, isCollapsed cxt)
    isCollapsed cxt
        | isaType cls "Bool" cxt        = True
        | isaType cls "Junction" cxt    = True
        | otherwise                     = False

juncApply f args
    | (before, (ApplyArg name (VJunc j vs) coll):after) <- break isTotalJunc args
    = VJunc j [ juncApply f (before ++ ((ApplyArg name v coll):after)) | v <- vs ]
    | (before, (ApplyArg name (VJunc j vs) coll):after) <- break isPartialJunc args
    = VJunc j [ juncApply f (before ++ ((ApplyArg name v coll):after)) | v <- vs ]
    | (val:_) <- [ val | (ApplyArg _ val@(VError _ _) _) <- args ]
    = val
    | otherwise
    = f args

isTotalJunc (ApplyArg _ (VJunc JAll _) b)   = not b
isTotalJunc (ApplyArg _ (VJunc JNone _) b)  = not b
isTotalJunc _                   = False

isPartialJunc (ApplyArg _ (VJunc JOne _) b) = not b
isPartialJunc (ApplyArg _ (VJunc JAny _) b) = not b
isPartialJunc _                 = False

toGlobal name
    | (sigil, identifier) <- break (\x -> isAlpha x || x == '_') name
    , last sigil /= '*'
    = sigil ++ ('*':identifier)
    | otherwise = name

retVal :: Val -> ((Env -> Env), Exp)
retVal val = (id, Val val)

isGlobalExp (Syn name _) = name `elem` map ("&infix:" ++) (words ":= ::=")
isGlobalExp _ = False

reduce :: Env -> Exp -> ((Env -> Env), Exp)
reduce env@Env{ sym = sym } exp@(Var var _)
    | Just val <- lookup var sym
    = retVal val
    | Just val <- lookup (toGlobal var) sym
    = retVal val
    | otherwise
    = retVal $ VError ("Undefined variable " ++ var) exp

reduce env@Env{ cxt = cxt } exp@(Syn name exps)
    | name `isInfix` ";"
    , [left, right]     <- exps
    , (lead, final)     <- buildStatements exps
    , (env', exp)       <- foldl (runStatement "Any") (env, Val VUndef) lead
    , (env', exp)       <- runStatement cxt (env', exp) final
    = (const env', exp)
    | name `isInfix` ":="
    , [Var var _, exp]  <- exps
    , (fenv, Val val)   <- reduce env exp
    = (combineEnv fenv var val, Val val)
    | name `isInfix` "::="
    , [Var var _, Val val]  <- exps
    = (combineEnv id var val, Val VUndef)
    | name `isInfix` "=>"
    , [keyExp, valExp]  <- exps
    , key               <- evaluate env keyExp
    , val               <- evaluate env valExp
    = retVal $ VPair key val
    | name `isInfix` ","
    = retVal $ VList $ concatMap (vCast . evaluate env{ cxt = "List" }) exps
    | name `isInfix` "[]"
    , [listExp, rangeExp]   <- exps
    , list      <- evaluate env{ cxt = "List" } listExp
    , range     <- evaluate env{ cxt = "List" } rangeExp
    , slice     <- unfoldr (doSlice $ vCast list) (map vCast $ vCast range)
    = retVal $ VList slice
    where
    doSlice :: [Val] -> [VInt] -> Maybe (Val, [VInt])
    doSlice vs [] = Nothing
    doSlice vs (n:ns)
        | genericLength vs > n  = Just ((vs `genericIndex` n), ns)
        | otherwise             = Nothing
    buildStatements exps
        | ((Syn name' exps'):rest)  <- exps
        , name' `isInfix` ";"
        = buildStatements (exps' ++ rest)
        | (global, local)   <- partition isGlobalExp exps
        , stmts             <- global ++ local
        = (init stmts, last stmts)
    runStatement :: Cxt -> (Env, Exp) -> Exp -> (Env, Exp)
    runStatement cxt (env, (Val val)) exp
        | VError _ _    <- val
        = (env, Val val)
        | (fenv, exp)   <- reduce env{ cxt = cxt } exp
        = (fenv env, exp)
        | otherwise
        = (env, Val $ VError "Unterminated statement" exp)
    combineEnv f var val env = (f env) `addSym` [(var, val)]
    isInfix name s = name == "&infix:" ++ s

reduce env@Env{ cxt = cxt, cls = cls } exp@(App name invs args)
    | Just sub <- findSub name
    = applySub sub invs args
    | otherwise
    = retVal $ VError ("No compatible subroutine found: " ++ name) exp
    where
    applySub sub invs args
        -- list-associativity
        | Sub{ subAssoc = "list" }      <- sub
        , (App name' invs' args'):rest  <- args
        , name == name'
        , null invs'
        = applySub sub [] (args' ++ rest)
        -- fix subParams to agree with number of actual arguments
        | Sub{ subAssoc = "list", subParams = (p:_) }   <- sub
        , null invs
        = apply env sub{ subParams = (length args) `replicate` p } [] args
        -- chain-associativity
        | Sub{ subAssoc = "chain", subFun = fun, subParams = prm }   <- sub
        , (App name' invs' args'):rest              <- args
        , Just sub'                                 <- findSub name'
        , Sub{ subAssoc = "chain", subFun = fun', subParams = prm' } <- sub'
        , null invs'
        = applySub sub{ subFun = Prim $ chainFun env prm' fun' prm fun } [] (args' ++ rest)
        -- fix subParams to agree with number of actual arguments
        | Sub{ subAssoc = "chain", subParams = (p:_) }   <- sub
        , null invs
        = apply env sub{ subParams = (length args) `replicate` p } [] args -- XXX Wrong
        -- normal application
        | otherwise
        = apply env sub invs args
    findSub name
        | ((_, sub):_) <- sort (subs name)  = Just sub
        | otherwise                         = Nothing
    subs name = [
        ( (isGlobal, subT, isMulti sub, bound, distance, order)
        , fromJust fun
        )
        | ((n, val), order) <- sym env `zip` [0..]
        , let sub@(Sub{ subType = subT, subReturns = ret, subParams = prms }) = vCast val
        , n == name || n == toGlobal name
        , let isGlobal = '*' `elem` n
        , let fun = arityMatch sub (invs ++ args) -- XXX Wrong
        , isJust fun
        , deltaFromCxt ret /= 0
        , let invocants = filter isInvocant prms
        , let prms' = if null invocants then prms else invocants
        , let distance = (deltaFromCxt ret : map (deltaFromScalar . paramContext) prms')
        , let bound = either (const False) (const True) $ bindParams prms invs args
        ]
    deltaFromCxt            = deltaType cls cxt
    deltaFromScalar ('*':x) = deltaFromScalar x
    deltaFromScalar x       = deltaType cls x "Scalar"

reduce env (Parens exp) = reduce env exp
reduce env other = (id, other)

arityMatch sub@Sub{ subAssoc = assoc, subParams = prms } args
    | assoc == "list"               = Just sub
    | isJust $ find isSlurpy prms
    , assoc == "pre"                = Just sub
--  | (length prms == length args)  = Just sub -- XXX optionals
--  | (length prms >= length args)  = Just sub
    | otherwise                     = Just sub
    | otherwise                     = Nothing
