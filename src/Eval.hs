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

applyFun :: Env -> Exp -> [Val] -> Val
applyFun env (Prim f) vals = f vals
applyFun env body vals
    | Val val   <- exp          = val
    | otherwise                 = VError "Invalid expression" exp
    where
    (fenv, exp) = reduce (env `addSym` [("$_", head vals),("@_", VList vals)]) body

chainFun :: Env -> Exp -> Exp -> [Val] -> Val
chainFun env f1 f2 (v1:v2:vs)
    | VBool False <- applyFun env f1 [v1, v2]
    = VBool False
    | otherwise
    = applyFun env f2 (v2:vs)

apply :: Env -> VSub -> [Exp] -> ((Env -> Env), Exp)
apply env@Env{ cls = cls } Sub{ subParams = prms, subFun = fun } exps
    = retVal $ juncApply eval args
    where
    eval = applyFun env fun
    args = map expToVal (prms `zip` exps)
    expToVal (cxt, exp) = (evaluate env{ cxt = cxt } exp, isaType cls cxt "Bool")

juncApply f args
    | (before, (VJunc j vs, cxt):after) <- break isTotalJunc args
    = VJunc j [ juncApply f (before ++ ((v, cxt):after)) | v <- vs ]
    | (before, (VJunc j vs, cxt):after) <- break isPartialJunc args
    = VJunc j [ juncApply f (before ++ ((v, cxt):after)) | v <- vs ]
    | (val, _):_ <- [ err | err@(VError _ _, _) <- args ]
    = val
    | otherwise
    = f $ map fst args 

retVal :: Val -> ((Env -> Env), Exp)
retVal val = (id, Val val)

reduce :: Env -> Exp -> ((Env -> Env), Exp)
reduce env@Env{ sym = sym } exp@(Var var _)
    | Just val <- lookup var sym
    = retVal val
    | otherwise
    = retVal $ VError ("Undefined variable " ++ var) exp

reduce env@Env{ cxt = cxt } exp@(Syn name exps)
    | name `isInfix` ";"
    , [left, right]     <- exps
    , (env', exp)       <- runStatement "Any" (env, Val VUndef) left
    , (env', exp)       <- runStatement cxt   (env', exp)       right
    = (const env', exp)
    | name `isInfix` ":="
    , [Var var _, exp]  <- exps
    , (fenv, Val val)   <- reduce env exp
    = (combineEnv fenv var val, Val val)
    | name `isInfix` "::="
    , [Var var _, Val val]  <- exps
    = (combineEnv id var val, Val VUndef)
    where
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

reduce env@Env{ cxt = cxt, cls = cls } exp@(App name exps)
    | Just sub <- findSub name
    = applySub sub exps
    | otherwise
    = retVal $ VError ("Undefined subroutine " ++ name ++ (show $ sym env)) exp
    where
    applySub sub exps
        -- list-associativity
        | Sub{ subAssoc = "list" }  <- sub
        , (App name' exps'):rest    <- exps
        , name == name'
        = applySub sub (exps' ++ rest)
        -- fix subParams to agree with number of actual arguments
        | Sub{ subAssoc = "list", subParams = (p:_) }   <- sub
        , trace ("meow " ++ (show exps)) True
        = apply env sub{ subParams = (length exps) `replicate` p } exps
        -- chain-associativity
        | Sub{ subAssoc = "chain", subFun = fun }   <- sub
        , (App name' exps'):rest                    <- exps
        , Just sub'                                 <- findSub name'
        , Sub{ subAssoc = "chain", subFun = fun' }  <- sub'
        = applySub sub{ subFun = Prim $ chainFun env fun' fun } (exps' ++ rest)
        -- fix subParams to agree with number of actual arguments
        | Sub{ subAssoc = "chain", subParams = (p:_) }   <- sub
        = apply env sub{ subParams = (length exps) `replicate` p } exps -- XXX Wrong
        -- apply normally
        | Sub{ subParams = [('*':p)] }              <- sub -- XXX Wrong
        = apply env sub{ subParams = (length exps) `replicate` p } exps
        | otherwise
        = apply env sub exps
    findSub name
        | ((_, sub):_) <- sort (subs name)  = Just sub
        | otherwise                         = Nothing
    subs name = [ ((subT, deltaFromCxt ret : map deltaFromScalar prms), sub)
           | (n, val) <- sym env
           , let sub@(Sub{ subType = subT, subReturns = ret, subParams = prms }) = vCast val
           , n == name
           , arityMatch sub prms exps
           , deltaFromCxt ret /= 0
           ]
    deltaFromCxt        = deltaType cls cxt
    deltaFromScalar x   = deltaType cls x "Scalar"
    arityMatch Sub{ subAssoc = assoc, subParams = prms } x y
        | assoc == "list"           = True
        | Just _ <- find ((== '*') . head) prms
        , assoc == "pre"            = True
        | (length x) == (length y)  = True  -- XXX - slurping star
        | otherwise                 = False

reduce env (Parens exp) = reduce env exp
reduce env other = (id, other)

