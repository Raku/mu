{-# OPTIONS -fglasgow-exts #-}

{-
    Evaluation and reduction engine.

    Tree and flower and leaf and grass,
    Let them pass! Let them pass!
    Hill and water under sky,
    Pass them by! Pass them by!
-}

module Eval where
import AST
import Prim

type Env = ()
emptyEnv = ()

evaluate :: Env -> Exp -> Val
evaluate env exp
    | Val v <- reduce env exp   = v
    | otherwise                 = VError "invalid expression" exp

-- Lazy evaluation for lists.
-- Context propagation.

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

reduce :: Env -> Exp -> Exp
reduce env (Op1 name exp)
    | VError _ _ <- arg
    = Val $ arg
    | VJunc j l <- arg
    = if name == "?"
        then Val $ VBool (vCast arg)
        else Val $ VJunc j [ reval a | a <- l ]
    | otherwise
    = Val $ op1 name (vCast arg)
    where
    arg = evaluate env exp
    reval = evaluate env . Op1 name . Val

reduce env (OpCmp name exp1 exp2)
    | OpCmp _ _ exp1b <- exp1
    = reduce env $ Op2 "&&" exp1 $ Op2 name exp1b exp2
    | otherwise
    = reduce env $ Op2 name exp1 exp2

{- XXX - this really wants a rewrite with multi subs -}
reduce env (Op2 name exp1 exp2)
    | name `elem` words " ! & | ^ && || ^^ // and or xor err " -- XXX contextify
    = Val $ op arg1 arg2
    | VError _ _ <- arg1 = Val $ arg1
    | VError _ _ <- arg2 = Val $ arg2
    -- two junctions, all/none at left
    | Just (j1, l1) <- arg1 `juncTypeIs` [JAll, JNone]
    , Just (j2, l2) <- juncType arg2
    = Val $ VJunc j1 [ VJunc j2 [ reval a1 a2 | a2 <- l2 ] | a1 <- l1 ]
    -- two junctions, all/none at right
    | Just (j1, l1) <- juncType arg1
    , Just (j2, l2) <- arg2 `juncTypeIs` [JAll, JNone]
    = Val $ VJunc j2 [ VJunc j1 [ reval a1 a2 | a1 <- l1 ] | a2 <- l2 ]
    -- two junctions with all low prec.
    | Just (j1, l1) <- juncType arg1
    , Just (j2, l2) <- juncType arg2
    = Val $ VJunc j1 [ VJunc j2 [ reval a1 a2 | a2 <- l2 ] | a1 <- l1 ]
    -- one junctions at left
    | Just (j, l) <- juncType arg1
    = Val $ VJunc j [ reval a arg2 | a <- l ]
    -- one junctions at right
    | Just (j, l) <- juncType arg2
    = Val $ VJunc j [ reval arg1 a | a <- l ]
    | otherwise
    = Val $ op arg1 arg2
    where
    op = op2 name
    arg1 = evaluate env exp1
    arg2 = evaluate env exp2
    reval x y = evaluate env $ Op2 name (Val x) (Val y)

reduce env other = other

