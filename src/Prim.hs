{-# OPTIONS -fglasgow-exts #-}

{-
    Primitive operators.

    Learn now the lore of Living Creatures!
    First name the four, the free peoples:
    Eldest of all, the elf-children;
    Dwarf the delver, dark are his houses;
    Ent the earthborn, old as mountains;
    Man the mortal, master of horses...
-}

module Prim where
import Internals
import AST

op1 :: Ident -> (forall a. Context a => a) -> Val
op1 "!"  = \x -> case op1 "?" x of
    VBool True  -> VBool False
    VBool False -> VBool True
op1 "+"  = op1Numeric id
op1 "-"  = op1Numeric negate
op1 "~"  = VStr
op1 "?"  = VBool
op1 "*"  = VList
op1 "**" = VList . map (id $!)
op1 "+^" = VInt . (toInteger . (complement :: Word -> Word))
op1 "~^" = VStr . mapStr complement
op1 "?^" = op1 "!"
op1 "\\" = VRef
op1 "..."= op1Range
op1 "not"= op1 "!"
op1 s    = \x -> VError ("not implemented: " ++ s) (Val x)

mapStr :: (Word8 -> Word8) -> [Word8] -> String
mapStr f = map (chr . fromEnum . f)

mapStr2 :: (Word8 -> Word8 -> Word8) -> [Word8] -> [Word8] -> String
mapStr2 f x y = map (chr . fromEnum . uncurry f) $ x `zip` y

op2 :: Ident -> Val -> Val -> Val
op2 "*"  = op2Numeric (*)
op2 "/"  = op2Divide
op2 "%"  = op2Int mod
op2 "x"  = \x y -> VStr (concat $ (vCast y :: VInt) `genericReplicate` (vCast x :: VStr))
op2 "xx" = \x y -> VList ((vCast y :: VInt) `genericReplicate` x)
op2 "+&" = op2Int (.&.)
op2 "+<<"= op2Int shiftL
op2 "+>>"= op2Int shiftR
op2 "~&" = op2Str $ mapStr2 (.&.)
op2 "~<<"= \x y -> VStr $ mapStr (`shiftL` vCast y) (vCast x)
op2 "~>>"= \x y -> VStr $ mapStr (`shiftR` vCast y) (vCast x)
op2 "**" = op2Num (**)
op2 "+"  = op2Numeric (+)
op2 "-"  = op2Numeric (-)
op2 "~"  = op2Str (++)
op2 "+|" = op2Int (.|.)
op2 "+^" = op2Int xor
op2 "~|" = op2Str $ mapStr2 (.|.)
op2 "~^" = op2Str $ mapStr2 xor
op2 "!"  = op2Junction JNone
op2 "&"  = op2Junction JAll
op2 "^"  = op2Junction JOne
op2 "|"  = op2Junction JAny
op2 "=>" = VPair
op2 "cmp"= op2Ord vCastStr
op2 "<=>"= op2Ord vCastNum
op2 ".." = op2Range
op2 "!=" = op2Cmp vCastNum (/=)
op2 "==" = op2Cmp vCastNum (==)
op2 "<"  = op2Cmp vCastNum (<)
op2 "<=" = op2Cmp vCastNum (<=)
op2 ">"  = op2Cmp vCastNum (>)
op2 ">=" = op2Cmp vCastNum (>=)
op2 "ne" = op2Cmp vCastStr (/=)
op2 "eq" = op2Cmp vCastStr (==)
op2 "lt" = op2Cmp vCastStr (<)
op2 "le" = op2Cmp vCastStr (<=)
op2 "gt" = op2Cmp vCastStr (>)
op2 "ge" = op2Cmp vCastStr (>=)
op2 "&&" = op2Logical not
op2 "||" = op2Logical (id :: Bool -> Bool)
op2 "^^" = op2Bool ((/=) :: Bool -> Bool -> Bool)
op2 "//" = op2Logical isJust
op2 ","  = op2ChainedList
-- XXX pipe forward XXX
op2 "and"= op2 "&&"
op2 "or" = op2 "||"
op2 "xor"= op2 "^^"
op2 "err"= op2 "//"
op2 ";"  = \x y -> y -- XXX wrong! LoL!
op2 s    = \x y -> VError ("not implemented: " ++ s) (Val $ VPair x y)

vCastStr :: Val -> VStr
vCastStr = vCast
vCastNum :: Val -> VNum
vCastNum = vCast

op2Str  f x y = VStr  $ f (vCast x) (vCast y)
op2Num  f x y = VNum  $ f (vCast x) (vCast y)
op2Rat  f x y = VRat  $ f (vCast x) (vCast y)
op2Bool f x y = VBool $ f (vCast x) (vCast y)
op2Int  f x y = VInt  $ f (vCast x) (vCast y)

op1Range (VStr s)    = VList $ map VStr $ strRangeInf s
op1Range (VNum n)    = VList $ map VNum [n ..]
op1Range x           = VList $ map VInt [vCast x ..]

op2Range (VStr s) y  = VList $ map VStr $ strRange s (vCast y)
op2Range (VNum n) y  = VList $ map VNum [n .. vCast y]
op2Range x (VNum n)  = VList $ map VNum [vCast x .. n]
op2Range x y         = VList $ map VInt [vCast x .. vCast y]

op2Divide x y
    | VInt x' <- x
    , VInt y' <- y
    = VRat $ x' % y'
    | otherwise
    = op2Num (/) x y

op2ChainedList x y
    | VList xs <- x, VList ys <- y  = VList $ xs ++ ys
    | VList xs <- x                 = VList $ xs ++ [y]
    | VList ys <- y                 = VList (x:ys)
    | otherwise                     = VList [x, y]

op2Logical f x y = if f (vCast x) then x else y

op2DefinedOr = undefined

op2Cmp f cmp x y = VBool $ f x `cmp` f y

op2Ord f x y = VInt $ case f x `compare` f y of
    LT -> -1
    EQ -> 0
    GT -> 1

op2Junction j x y
    | VJunc xj xl <- x, xj == j
    , VJunc yj yl <- y, yj == j
    = VJunc j (xl ++ yl)
    | VJunc xj l <- x, xj == j
    = VJunc j (y:l)
    | VJunc yj l <- y, yj == j
    = VJunc j (x:l)
    | otherwise
    = VJunc j [x, y]

op1Numeric :: (forall a. (Num a) => a -> a) -> Val -> Val
op1Numeric f VUndef     = VInt $ f 0
op1Numeric f (VInt x)   = VInt $ f x
op1Numeric f (VList l)  = VInt $ f $ genericLength l
op1Numeric f x          = VNum $ f (vCast x)

op2Numeric :: (forall a. (Num a) => a -> a -> a) -> Val -> Val -> Val
op2Numeric f x y
    | (VInt x', VInt y') <- (x, y)
    = VInt $ f x' y'
    | otherwise
    = VNum $ f (vCast x) (vCast y)
