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

op0 :: Ident -> [Val] -> Eval Val
op0 ","  = return . VList . concatMap vCast
op0 "!"  = return . VJunc . Junc JNone emptySet . mkSet
op0 "&"  = return . opJuncAll
op0 "^"  = return . opJuncOne
op0 "|"  = return . opJuncAny
op0 "time"  = \_ -> do
    clkt <- liftIO getClockTime
    return $ VInt $ toInteger $ tdSec $ diffClockTimes clkt epochClkT
    where
    epochClkT = toClockTime epoch
    epoch = CalendarTime 1970 January 1 0 0 0 0 Thursday 0 "UTC" 0 False
op0 s    = \x -> return $ VError ("unimplemented listOp: " ++ s) (Val $ VList x)

op1 :: Ident -> Val -> Eval Val
op1 "!"    = return . fmapVal not
op1 "+"    = return . op1Numeric id
op1 "++"   = \mv -> do
    val <- readMVal mv
    liftIO $ writeIORef (vCast mv) $ case val of
        (VStr str)  -> VStr $ strInc str
        _           -> op1Numeric (\x -> x + 1) (vCast val)
    return val
op1 "--"   = \mv -> do
    val <- liftIO $ readIORef (vCast mv)
    liftIO $ writeIORef (vCast mv) $
        op1Numeric (\x -> x - 1) (vCast val)
    return val
op1 "-"    = return . op1Numeric negate
op1 "~"    = return . VStr . vCast
op1 "?"    = return . VBool . vCast
op1 "*"    = return . VList . vCast
op1 "**"   = return . VList . map (id $!) . vCast
op1 "+^"   = return . VInt . (toInteger . (complement :: Word -> Word)) . vCast
op1 "~^"   = return . VStr . mapStr complement . vCast
op1 "?^"   = op1 "!"
op1 "\\"   = return . VRef
op1 "..."  = return . op1Range
op1 "not"  = op1 "!"
op1 "any"  = return . opJuncAny . vCast
op1 "all"  = return . opJuncAll . vCast
op1 "one"  = return . opJuncOne . vCast
op1 "none" = return . VJunc . Junc JNone emptySet . mkSet . vCast
op1 "perl" = return . VStr . (pretty :: Val -> VStr)
op1 "eval" = opEval . vCast
op1 "last" = \v -> do
    shiftT $ \_ -> return VUndef
op1 "return" = \v -> return (VError "cannot return outside a subroutine" (Val v))

-- Side-effectful function: how far into Monadic IO shall we go?
op1 "rand"  = \v -> do
    let x = vCast v
    rand <- liftIO $ randomRIO (0, if x == 0 then 1 else x)
    return $ VNum rand
op1 "print" = \v -> do
    v <- readMVal v
    vals <- mapM readMVal (vCast v)
    liftIO . putStr . concatMap vCast $ vals
    return $ VBool True
op1 "say" = \v -> do
    op1 "print" v
    liftIO $ putStrLn ""
    return $ VBool True
op1 "die" = \v -> do
    return $ VError (concatMap vCast . vCast $ v) (Val v)
op1 "exit" = \v -> do
    if vCast v
        then liftIO $ exitWith (ExitFailure $ vCast v)
        else liftIO $ exitWith ExitSuccess
-- handle timely destruction
op1 "open" = \v -> do
    fh <- liftIO $ openFile (vCast v) ReadMode
    return $ VHandle fh
op1 "close" = \v -> do
    liftIO $ hClose (vCast v)
    return $ VBool True
op1 "<>" = \v -> do
    str <- readFrom v
    cxt <- asks envContext
    return $ if ((cxt ==) `any` ["Array", "List"]) -- XXX use isaType here
        then VList $ map VStr $ lines str
        else VStr str
    where
    readFrom VUndef = do
        -- ARGS etc
        glob <- asks envGlobal
        strs <- liftIO $ sequence $ case find ((== "@*ARGS") . symName) glob of
            Nothing     -> [getStdin glob]
            Just sym    -> case symExp sym of
                Val (VList [])  -> [getStdin glob]
                Val (VList xs)  -> map ((hGetContents =<<) . (`openFile` ReadMode) . vCast) xs
                _               -> error "not handled"
        return $ concat strs
    readFrom v = do
        liftIO $ hGetContents $ vCast v
    getStdin glob = do
        case find ((== "$*STDIN") . symName) glob of
            Just sym | (Val v) <- symExp sym -> return $ vCast v
            _                                -> error "impossible"

op1 ""     = return . (\x -> VError ("unimplemented unaryOp: " ++ "") (Val x))

op1 s      = return . (\x -> VError ("unimplemented unaryOp: " ++ s) (Val x))

opEval :: String -> Eval Val
opEval str = do
    env <- ask
    let env' = runRule env id ruleProgram str
    val <- resetT $ local (\_ -> env') $ do
        evl <- asks envEval
        evl (envBody env')
    case val of
        VError _ _  -> return VUndef
        _           -> return val

mapStr :: (Word8 -> Word8) -> [Word8] -> String
mapStr f = map (chr . fromEnum . f)

mapStr2 :: (Word8 -> Word8 -> Word8) -> [Word8] -> [Word8] -> String
mapStr2 f x y = map (chr . fromEnum . uncurry f) $ x `zip` y

op2 :: Ident -> Val -> Val -> Eval Val
op2 "*"  = op2Numeric (*)
op2 "/"  = op2Divide
op2 "%"  = op2Int mod
op2 "x"  = \x y -> return $ VStr (concat $ (vCast y :: VInt) `genericReplicate` (vCast x :: VStr))
op2 "xx" = \x y -> return $ VList ((vCast y :: VInt) `genericReplicate` x)
op2 "+&" = op2Int (.&.)
op2 "+<<"= op2Int shiftL
op2 "+>>"= op2Int shiftR
op2 "~&" = op2Str $ mapStr2 (.&.)
op2 "~<<"= \x y -> return $ VStr $ mapStr (`shiftL` vCast y) (vCast x)
op2 "~>>"= \x y -> return $ VStr $ mapStr (`shiftR` vCast y) (vCast x)
op2 "**" = op2Rat ((^^) :: VRat -> VInt -> VRat)
op2 "+"  = op2Numeric (+)
op2 "-"  = op2Numeric (-)
op2 "~"  = op2Str (++)
op2 "+|" = op2Int (.|.)
op2 "+^" = op2Int xor
op2 "~|" = op2Str $ mapStr2 (.|.)
op2 "~^" = op2Str $ mapStr2 xor
op2 "=>" = \x y -> return $ VPair x y
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
op2 "grep"= op2Grep
op2 "map"= op2Map
op2 s    = \x y -> return $ VError ("unimplemented binaryOp: " ++ s) (App s [] [Val x, Val y])

op2Grep list sub@(VSub _) = op2Grep sub list
op2Grep sub list = do
    vals <- (`filterM` vCast list) $ \x -> do
        evl <- asks envEval
        rv  <- local (\e -> e{ envContext = "Bool" }) $ do
            evl (Syn "()" [Val sub, Syn "invs" [Val x], Syn "args" []])
        return $ vCast rv
    return $ VList vals

op2Map list sub@(VSub _) = op2Map sub list
op2Map sub list = do
    vals <- (`mapM` vCast list) $ \x -> do
        evl <- asks envEval
        rv  <- local (\e -> e{ envContext = "List" }) $ do
            evl (Syn "()" [Val sub, Syn "invs" [Val x], Syn "args" []])
        return $ vCast rv
    return $ VList $ concat vals

vCastStr :: Val -> VStr
vCastStr = vCast
vCastRat :: Val -> VRat
vCastRat = vCast

op2Str  f x y = return $ VStr  $ f (vCast x) (vCast y)
op2Num  f x y = return $ VNum  $ f (vCast x) (vCast y)
op2Rat  f x y = return $ VRat  $ f (vCast x) (vCast y)
op2Bool f x y = return $ VBool $ f (vCast x) (vCast y)
op2Int  f x y = return $ VInt  $ f (vCast x) (vCast y)

op1Range (VStr s)    = VList $ map VStr $ strRangeInf s
op1Range (VRat n)    = VList $ map VRat [n ..]
op1Range (VNum n)    = VList $ map VNum [n ..]
op1Range x           = VList $ map VInt [vCast x ..]

op2Range (VStr s) y  = return $ VList $ map VStr $ strRange s (vCast y)
op2Range (VNum n) y  = return $ VList $ map VNum [n .. vCast y]
op2Range x (VNum n)  = return $ VList $ map VNum [vCast x .. n]
op2Range (VRat n) y  = return $ VList $ map VRat [n .. vCast y]
op2Range x (VRat n)  = return $ VList $ map VRat [vCast x .. n]
op2Range x y         = return $ VList $ map VInt [vCast x .. vCast y]

op2Divide x y
    | VInt x' <- x, VInt y' <- y
    = return $ if y' == 0 then err else VRat $ x' % y'
    | VInt x' <- x, VRat y' <- y
    = return $ if y' == 0 then err else VRat $ (x' % 1) / y'
    | VRat x' <- x, VInt y' <- y
    = return $ if y' == 0 then err else VRat $ x' / (y' % 1)
    | VRat x' <- x, VRat y' <- y
    = return $ if y' == 0 then err else VRat $ x' / y'
    | otherwise
    = op2Num (/) x y
    where
    err = VError ("Illegal division by zero: " ++ "/") (App "/" [] [Val x, Val y])

op2ChainedList x y
    | VList xs <- x, VList ys <- y  = VList $ xs ++ ys
    | VList xs <- x                 = VList $ xs ++ [y]
    | VList ys <- y                 = VList (x:ys)
    | otherwise                     = VList [x, y]

op2Logical f x y = return $ if f (vCast x) then x else y

op2DefinedOr = undefined

op2Cmp f cmp x y = return $ VBool $ f x `cmp` f y

op2Ord f x y = return $ VInt $ case f x `compare` f y of
    LT -> -1
    EQ -> 0
    GT -> 1

op1Numeric :: (forall a. (Num a) => a -> a) -> Val -> Val
op1Numeric f VUndef     = VInt $ f 0
-- op1Numeric f (MVal x)   = op1Numeric f (castV x)
op1Numeric f (VRef x)   = op1Numeric f x
op1Numeric f (VInt x)   = VInt $ f x
op1Numeric f l@(VList _)= VInt $ f (vCast l)
op1Numeric f (VRat x)   = VRat $ f x
op1Numeric f x          = VNum $ f (vCast x)

--- XXX wrong: try num first, then int, then vcast to Rat (I think)
op2Numeric :: (forall a. (Num a) => a -> a -> a) -> Val -> Val -> Eval Val
op2Numeric f x y
    | (VInt x', VInt y') <- (x, y)  = return $ VInt $ f x' y'
    | (VRat x', VInt y') <- (x, y)  = return $ VRat $ f x' (y' % 1)
    | (VInt x', VRat y') <- (x, y)  = return $ VRat $ f (x' % 1) y'
    | (VRat x', VRat y') <- (x, y)  = return $ VRat $ f x' y'
    | otherwise                     = return $ VNum $ f (vCast x) (vCast y)

primOp :: String -> String -> Params -> String -> Symbol
primOp sym assoc prms ret = Symbol SOur name (Val sub)
    where
    name = '&':'*':fixity ++ ':':sym
    sub  = VSub $ Sub { isMulti     = True
                      , subName     = sym
                      , subPad      = []
                      , subType     = SubPrim
                      , subAssoc    = assoc
                      , subParams   = prms
                      , subReturns  = ret
                      , subFun      = (Prim f)
                      }
    f :: [Val] -> Eval Val
    f    = case arity of
        0 -> \(x:_) -> op0 sym (vCast x)
        1 -> \x     -> case x of
            [x]   -> op1 sym x
            [x,y] -> op2 sym x y
            x     -> op0 sym x
        2 -> \[x,y] -> op2 sym (vCast x) (vCast y)
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
    takeWord = takeWhile isWord . dropWhile (not . isWord)
    isWord = not . (`elem` "|(),")
    prms = map takeWord prms'

doFoldParam cxt [] []       = [buildParam cxt "" "$?1" (Val VUndef)]
doFoldParam cxt [] (p:ps)   = (buildParam cxt "" (strInc $ paramName p) (Val VUndef):p:ps)
doFoldParam cxt (s:name) ps = (buildParam cxt [s] name (Val VUndef) : ps)

foldParam :: String -> Params -> Params
foldParam "List"    = doFoldParam "List" "*@?0"
foldParam ""        = id
foldParam ('?':str) = \ps -> (buildParam "Num" "?" "$?1" (Val $ VNum (read def)):ps)
    where
    (cxt, ('=':def)) = break (== '=') str
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
\\n   Any       post    ++      (LValue)\
\\n   Num       post    --      (LValue)\
\\n   Bool      pre     not     (Bool)\
\\n   List      pre     map     (Code, Array)\
\\n   List      pre     grep    (Code, Array)\
\\n   List      pre     map     (Array: Code)\
\\n   List      pre     grep    (Array: Code)\
\\n   List      left    zip     (List)\
\\n   List      pre     keys    (Hash)\
\\n   List      pre     values  (Hash)\
\\n   List      pre     kv      (Hash)\
\\n   Str       pre     perl    (List)\
\\n   Any       pre     eval    (Str)\
\\n   Any       pre     last    (?Num=1)\
\\n   Any       pre     exit    (?Num=0)\
\\n   Num       pre     rand    (?Num=1)\
\\n   Num       pre     time    ()\
\\n   Action    pre     print   (List)\
\\n   Action    pre     say     (List)\
\\n   Action    pre     die     (List)\
\\n   Any       pre     do      (Str)\
\\n   IO        pre     open    (Str)\
\\n   Any       pre     return  (Any)\
\\n   Any       pre     <>      ()\
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
