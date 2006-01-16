{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans #-}
{-# OPTIONS_GHC -#include "../../UnicodeC.h" #-}

module Pugs.Prim.Numeric (
    op2Numeric, op1Floating, op1Round, op1Numeric,
    op2Exp, op2Divide, op2Modulus,
) where
import Pugs.Internals
import Pugs.AST
import Pugs.Types

import Pugs.Prim.Lifts

--- XXX wrong: try num first, then int, then vcast to Rat (I think)
op2Numeric :: (forall a. (Num a) => a -> a -> a) -> Val -> Val -> Eval Val
op2Numeric f x y
    | VUndef <- x = op2Numeric f (VInt 0) y
    | VUndef <- y = op2Numeric f x (VInt 0)
    | (VInt x', VInt y') <- (x, y)  = return $ VInt $ f x' y'
    | (VRat x', VInt y') <- (x, y)  = return $ VRat $ f x' (y' % 1)
    | (VInt x', VRat y') <- (x, y)  = return $ VRat $ f (x' % 1) y'
    | (VRat x', VRat y') <- (x, y)  = return $ VRat $ f x' y'
    | VRef r <- x = do
        x' <- readRef r
        op2Numeric f x' y
    | VRef r <- y = do
        y' <- readRef r
        op2Numeric f x y'
    | otherwise = do
        x' <- fromVal x
        y' <- fromVal y
        return . VNum $ f x' y'

op1Floating :: (Double -> Double) -> Val -> Eval Val
op1Floating f v = do
    foo <- fromVal v
    return $ VNum $ f foo

op1Round :: (Double -> Integer) -> Val -> Eval Val
op1Round f v = do
    case v of
       VInt i -> return $ VInt $ i
       VRat r -> return $ VInt $ f ((fromRational r)::Double)
       VNum n -> return $ VInt $ f n
       _      -> fail "Can't round non-numeric value(?)"

op1Numeric :: (forall a. (Num a) => a -> a) -> Val -> Eval Val
op1Numeric f VUndef     = return . VInt $ f 0
op1Numeric f (VInt x)   = return . VInt $ f x
op1Numeric f l@(VList _)= fmap (VInt . f) (fromVal l)
op1Numeric f (VRat x)   = return . VRat $ f x
op1Numeric f (VRef x)   = op1Numeric f =<< readRef x
op1Numeric f x          = fmap (VNum . f) (fromVal x)

op2Exp :: Val -> Val -> Eval Val
op2Exp x y = do
    num1 <- fromVal =<< fromVal' x
    if num1 == (1 :: VNum) then return (VInt 1) else do
    num2 <- fromVal =<< fromVal' y
    case reverse $ show (num2 :: VNum) of
        ('0':'.':_) -> do
            num1 <- fromVal =<< fromVal' x
            if isDigit . head $ show (num1 :: VNum)
                then op2Rat ((^^) :: VRat -> VInt -> VRat) x y
                else op2Num ((**) :: VNum -> VNum -> VNum) x y
        _ -> op2Num ((**) :: VNum -> VNum -> VNum) x y

op2Divide :: Val -> Val -> Eval Val
op2Divide x y
    | VInt x' <- x, VInt y' <- y
    = if y' == 0 then err else return . VRat $ x' % y'
    | VInt x' <- x, VRat y' <- y
    = if y' == 0 then err else return . VRat $ (x' % 1) / y'
    | VRat x' <- x, VInt y' <- y
    = if y' == 0 then err else return . VRat $ x' / (y' % 1)
    | VRat x' <- x, VRat y' <- y
    = if y' == 0 then err else return . VRat $ x' / y'
    | otherwise
    = op2Num (/) x y
    where
    err = fail "Illegal division by zero"

op2Modulus :: Val -> Val -> Eval Val
op2Modulus x y
    | VInt x' <- x, VInt y' <- y
    = if y' == 0 then err else return . VInt $ x' `mod` y'
    | VInt x' <- x, VRat y' <- y
    = if y' == 0 then err else return . VInt $ x' `mod` (truncate y')
    | VRat x' <- x, VInt y' <- y
    = if y' == 0 then err else return . VInt $ (truncate x') `mod` y'
    | VRat x' <- x, VRat y' <- y
    = if y' == 0 then err else return . VInt $ (truncate x') `mod` (truncate y')
    | VRef ref <- x
    = do
        x' <- readRef ref
        op2Modulus x' y
    | VRef ref <- y
    = do
        y' <- readRef ref
        op2Modulus x y'
    | otherwise      -- pray for the best
    = op2Int mod x y -- typeErr
    where
    err = fail "Illegal modulus zero"
