{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans #-}

module Pugs.Prim.Lifts (
  op1Cast, op2Cast,
  op2Array,
  vCastStr, vCastRat,
  op2Str, op2Num, op2Bool, op2Int, op2Rat,
) where
import Pugs.AST
import Pugs.Types

op1Cast :: (Value n) => (n -> Val) -> Val -> Eval Val
op1Cast f val = fmap f (fromVal =<< fromVal' val)

op2Cast :: (Value n, Value m) => (n -> m -> Val) -> Val -> Val -> Eval Val
op2Cast f x y = do
    x' <- fromVal =<< fromVal' x
    y' <- fromVal =<< fromVal' y
    return (f x' y')

op2Array :: (forall a. ArrayClass a => a -> [Val] -> Eval ()) -> Val -> Val -> Eval Val
op2Array f x y = do
    f    <- doArray x f
    vals <- fromVal y
    f vals
    size <- doArray x array_fetchSize
    idx  <- size
    return $ castV idx

vCastStr :: Val -> Eval VStr
vCastStr = fromVal
vCastRat :: Val -> Eval VRat
vCastRat = fromVal

op2Str :: (Value v1, Value v2) => (v1 -> v2 -> VStr) -> Val -> Val -> Eval Val
op2Str f x y = do
    x' <- fromVal x
    y' <- fromVal y
    return $ VStr $ f x' y'

op2Num    :: (Value v1, Value v2) => (v1 -> v2 -> VNum) -> Val -> Val -> Eval Val
op2Num  f = op2Cast $ (VNum .) . f

op2Bool   :: (Value v1, Value v2) => (v1 -> v2 -> VBool) -> Val -> Val -> Eval Val
op2Bool f = op2Cast $ (VBool .) . f

op2Int    :: (Value v1, Value v2) => (v1 -> v2 -> VInt) -> Val -> Val -> Eval Val
op2Int  f = op2Cast $ (VInt .) . f

op2Rat    :: (Value v1, Value v2) => (v1 -> v2 -> VRat) -> Val -> Val -> Eval Val
op2Rat  f = op2Cast $ (VRat .) . f

