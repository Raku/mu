{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans #-}

module Pugs.Prim.Numeric (
    op2Numeric, op1Floating, op1Numeric,
) where
import Pugs.Internals
import Pugs.AST

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

op1Numeric :: (forall a. (Num a) => a -> a) -> Val -> Eval Val
op1Numeric f VUndef     = return . VInt $ f 0
op1Numeric f (VInt x)   = return . VInt $ f x
op1Numeric f l@(VList _)= fmap (VInt . f) (fromVal l)
op1Numeric f (VRat x)   = return . VRat $ f x
op1Numeric f (VRef x)   = op1Numeric f =<< readRef x
op1Numeric f x          = fmap (VNum . f) (fromVal x)
