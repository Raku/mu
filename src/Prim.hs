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
import Junc
import AST
import Pretty
import Parser

op0 :: Ident -> [Val] -> Val
op0 ","  = VList . concatMap vCast
op0 "!"  = VJunc JNone . mkSet
op0 "&"  = opJuncAll
op0 "^"  = opJuncOne
op0 "|"  = opJuncAny
op0 s    = \x -> VError ("unimplemented listOp: " ++ s) (Val $ VList x)

op1 :: Ident -> Env -> (forall a. Context a => a) -> Val
op1 "!"    _ = fmapVal not
op1 "+"    _ = op1Numeric id
op1 "-"    _ = op1Numeric negate
op1 "~"    _ = VStr
op1 "?"    _ = VBool
op1 "*"    _ = VList
op1 "**"   _ = VList . map (id $!)
op1 "+^"   _ = VInt . (toInteger . (complement :: Word -> Word))
op1 "~^"   _ = VStr . mapStr complement
op1 "?^"   e = op1 "!" e
op1 "\\"   _ = VRef
op1 "..."  _ = op1Range
op1 "not"  e = op1 "!" e
op1 "any"  _ = opJuncAny
op1 "all"  _ = opJuncAll
op1 "one"  _ = opJuncOne
op1 "none" _ = VJunc JNone
op1 "perl" _ = \x -> VStr $ pretty (x :: Val)
op1 "eval" e = opEval e
op1 "rand" e = \(x :: VNum) -> VNum $ unsafePerformIO $ getStdRandom (randomR (0, if x == 0 then 1 else x))
op1 s      _ = \x -> VError ("unimplemented unaryOp: " ++ s) (Val x)

opEval :: Env -> String -> Val
opEval env str = case ( runParser parseProgram () "" str ) of
    Left err    -> VError (showErr err) (NonTerm $ errorPos err)
    Right ast   -> (evl env) env ast

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
op2 "**" = op2Rat ((^^) :: VRat -> VInt -> VRat)
op2 "+"  = op2Numeric (+)
op2 "-"  = op2Numeric (-)
op2 "~"  = op2Str (++)
op2 "+|" = op2Int (.|.)
op2 "+^" = op2Int xor
op2 "~|" = op2Str $ mapStr2 (.|.)
op2 "~^" = op2Str $ mapStr2 xor
op2 "=>" = VPair
op2 "cmp"= op2Ord vCastStr
op2 "<=>"= op2Ord vCastRat
op2 ".." = op2Range
op2 "!=" = op2Cmp vCastRat (/=)
op2 "==" = op2Cmp vCastRat (==)
op2 "<"  = op2Cmp vCastRat (<)
op2 "<=" = op2Cmp vCastRat (<=)
op2 ">"  = op2Cmp vCastRat (>)
op2 ">=" = op2Cmp vCastRat (>=)
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
op2 "!!" = op2Bool (\x y -> not x && not y)
-- XXX pipe forward XXX
op2 "and"= op2 "&&"
op2 "or" = op2 "||"
op2 "xor"= op2 "^^"
op2 "err"= op2 "//"
op2 "nor"= op2 "!!"
op2 ";"  = \x y -> y -- XXX wrong! LoL!
op2 s    = \x y -> VError ("unimplemented binaryOp: " ++ s) (App s [] [Val x, Val y])

vCastStr :: Val -> VStr
vCastStr = vCast
vCastRat :: Val -> VRat
vCastRat = vCast

op2Str  f x y = VStr  $ f (vCast x) (vCast y)
op2Num  f x y = VNum  $ f (vCast x) (vCast y)
op2Rat  f x y = VRat  $ f (vCast x) (vCast y)
op2Bool f x y = VBool $ f (vCast x) (vCast y)
op2Int  f x y = VInt  $ f (vCast x) (vCast y)

op1Range (VStr s)    = VList $ map VStr $ strRangeInf s
op1Range (VRat n)    = VList $ map VRat [n ..]
op1Range (VNum n)    = VList $ map VNum [n ..]
op1Range x           = VList $ map VInt [vCast x ..]

op2Range (VStr s) y  = VList $ map VStr $ strRange s (vCast y)
op2Range (VNum n) y  = VList $ map VNum [n .. vCast y]
op2Range x (VNum n)  = VList $ map VNum [vCast x .. n]
op2Range (VRat n) y  = VList $ map VRat [n .. vCast y]
op2Range x (VRat n)  = VList $ map VRat [vCast x .. n]
op2Range x y         = VList $ map VInt [vCast x .. vCast y]

op2Divide x y
    | VInt x' <- x, VInt y' <- y
    = if y' == 0 then err else VRat $ x' % y'
    | VInt x' <- x, VRat y' <- y
    = if y' == 0 then err else VRat $ (x' % 1) / y'
    | VRat x' <- x, VInt y' <- y
    = if y' == 0 then err else VRat $ x' / (y' % 1)
    | VRat x' <- x, VRat y' <- y
    = if y' == 0 then err else VRat $ x' / y'
    | otherwise
    = op2Num (/) x y
    where
    err = VError ("Illegal division by zero: " ++ "/") (App "/" [] [Val x, Val y])

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

op1Numeric :: (forall a. (Num a) => a -> a) -> Val -> Val
op1Numeric f VUndef     = VInt $ f 0
op1Numeric f (VInt x)   = VInt $ f x
op1Numeric f l@(VList _)= VInt $ f (vCast l)
op1Numeric f (VRat x)   = VRat $ f x
op1Numeric f x          = VNum $ f (vCast x)

--- XXX wrong: try num first, then int, then vcast to Rat (I think)
op2Numeric :: (forall a. (Num a) => a -> a -> a) -> Val -> Val -> Val
op2Numeric f x y
    | (VInt x', VInt y') <- (x, y)  = VInt $ f x' y'
    | (VRat x', VInt y') <- (x, y)  = VRat $ f x' (y' % 1)
    | (VInt x', VRat y') <- (x, y)  = VRat $ f (x' % 1) y'
    | (VRat x', VRat y') <- (x, y)  = VRat $ f x' y'
    | otherwise                     = VNum $ f (vCast x) (vCast y)

primOp :: String -> String -> Params -> String -> Symbol
primOp sym assoc prms ret = (name, sub)
    where
    name = '&':'*':fixity ++ ':':sym
    sub  = VSub $ Sub { isMulti     = True
                      , subType     = SubRoutine
                      , subAssoc    = assoc
                      , subParams   = prms
                      , subReturns  = ret
                      , subFun      = (Prim f)
                      }
    f :: Env -> [Val] -> Val
    f    = case arity of
        0 -> \env (x:_) -> op0 sym (vCast x)
        1 -> \env [x]   -> op1 sym env (vCast x)
        2 -> \env [x,y] -> op2 sym (vCast x) (vCast y)
        _ -> error (show arity)
    (arity, fixity) = case assoc of
        "pre"       -> (1, "prefix")
        "post"      -> (1, "postfix")
        "circum"    -> (1, "circumfix")
        "left"      -> (2, "infix")
        "right"     -> (2, "infix")
        "non"       -> (2, "infix")
        "chain"     -> (2, "infix")
        "list"      -> (0, "infix")
        other       -> (0, other)
        
primDecl str = primOp sym assoc (reverse $ foldr foldParam [] prms) ret
    where
    (ret:assoc:sym:prms') = words str
    takeWord = takeWhile isAlphaNum . dropWhile (not . isAlphaNum)
    prms = map takeWord prms'

doFoldParam cxt [] []       = [buildParam cxt "" "$a" (Val VUndef)]
doFoldParam cxt [] (p:ps)   = (buildParam cxt "" (strInc $ paramName p) (Val VUndef):p:ps)
doFoldParam cxt (s:name) ps = (buildParam cxt [s] name (Val VUndef) : ps)

foldParam :: String -> Params -> Params
foldParam "List"    = doFoldParam "List" "*@x"
foldParam "?Num=1"  = \ps -> (buildParam "Num" "?" "$x" (Val $ VNum 1):ps)
foldParam x         = doFoldParam x ""

-- XXX -- Junctive Types -- XXX --

initSyms = map primDecl . filter (not . null) . lines $ "\
\\n   Bool      pre     !       (Bool)\
\\n   Num       pre     +       (Num)\
\\n   Num       pre     -       (Num)\
\\n   Str       pre     ~       (Str)\
\\n   Bool      pre     ?       (Bool)\
\\n   List      pre     *       (List)\
\\n   List      pre     **      (List)\
\\n   Int       pre     +^      (Int)\
\\n   Int       pre     ~^      (Str)\
\\n   Bool      pre     ?^      (Bool)\
\\n   Ref       pre     \\      (Any)\
\\n   List      pre     ...     (Str|Num)\
\\n   Bool      pre     not     (Bool)\
\\n   Str       pre     perl    (List)\
\\n   Any       pre     eval    (Str)\
\\n   Num       pre     rand    (?Num=1)\
\\n   Junction  pre     any     (List)\
\\n   Junction  pre     all     (List)\
\\n   Junction  pre     one     (List)\
\\n   Junction  pre     none    (List)\
\\n   Junction  list    |       (List)\
\\n   Junction  list    &       (List)\
\\n   Junction  list    ^       (List)\
\\n   Junction  list    !       (List)\
\\n   Num       left    *       (Num, Num)\
\\n   Num       left    /       (Num, Num)\
\\n   Num       left    %       (Num, Num)\
\\n   Str       left    x       (Str, Int)\
\\n   List      left    xx      (Any, Int)\
\\n   Int       left    +&      (Int, Int)\
\\n   Int       left    +<<     (Int, Int)\
\\n   Int       left    +>>     (Int, Int)\
\\n   Str       left    ~&      (Str, Str)\
\\n   Str       left    ~<<     (Str, Str)\
\\n   Str       left    ~>>     (Str, Str)\
\\n   Num       right   **      (Num, Num)\
\\n   Num       left    +       (Num, Num)\
\\n   Num       left    -       (Num, Num)\
\\n   Str       left    ~       (Str, Str)\
\\n   Int       left    +|      (Int, Int)\
\\n   Int       left    +^      (Int, Int)\
\\n   Str       left    ~|      (Str, Str)\
\\n   Str       left    ~^      (Str, Str)\
\\n   Pair      non     =>      (Any, Any)\
\\n   Int       non     cmp     (Str, Str)\
\\n   Int       non     <=>     (Num, Num)\
\\n   List      non     ..      (Any, Any)\
\\n   Bool      chain   !=      (Num, Num)\
\\n   Bool      chain   ==      (Num, Num)\
\\n   Bool      chain   <       (Num, Num)\
\\n   Bool      chain   <=      (Num, Num)\
\\n   Bool      chain   >       (Num, Num)\
\\n   Bool      chain   >=      (Num, Num)\
\\n   Bool      chain   ne      (Str, Str)\
\\n   Bool      chain   eq      (Str, Str)\
\\n   Bool      chain   lt      (Str, Str)\
\\n   Bool      chain   le      (Str, Str)\
\\n   Bool      chain   gt      (Str, Str)\
\\n   Bool      chain   ge      (Str, Str)\
\\n   Scalar    left    &&      (Bool, Bool)\
\\n   Scalar    left    !!      (Bool, Bool)\
\\n   Scalar    left    ||      (Bool, Bool)\
\\n   Scalar    left    ^^      (Bool, Bool)\
\\n   Scalar    left    //      (Bool, Bool)\
\\n   List      list    ,       (List)\
\\n   List      pre     <==     (List)\
\\n   List      left    ==>     (List, Code)\
\\n   Scalar    left    and     (Bool, Bool)\
\\n   Scalar    left    or      (Bool, Bool)\
\\n   Scalar    left    xor     (Bool, Bool)\
\\n   Scalar    left    err     (Bool, Bool)\
\\n   Any       list    ;       (Any)\
\\n"
