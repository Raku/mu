{-# OPTIONS_GHC -fglasgow-exts #-}

module Pugs.Prim.List (
    op0Zip, op1Pick, op1Sum,
    op1Min, op1Max, op1Uniq,
    op2FoldL, op2Fold, op2Grep, op2Map, op2Join,
    sortByM,
    op1HyperPrefix, op1HyperPostfix, op2Hyper,
) where
import Pugs.Internals
import Pugs.AST
import Pugs.Types
import Pugs.Monads
import qualified Data.Set as Set

import Pugs.Prim.Numeric
import Pugs.Prim.Lifts

op0Zip :: [Val] -> Eval Val
op0Zip = fmap (VList . concat . op0Zip') . mapM fromVal

op0Zip' :: [[Val]] -> [[Val]]
op0Zip' lists | all null lists = []
op0Zip' lists = (map zipFirst lists):(op0Zip' (map zipRest lists))
    where
    zipFirst []     = undef
    zipFirst (x:_)  = x
    zipRest  []     = []
    zipRest  (_:xs) = xs

op1Pick :: Val -> Eval Val
op1Pick (VRef r) = op1Pick =<< readRef r
op1Pick (VList []) = return undef
op1Pick (VList vs) = do
    rand <- liftIO $ randomRIO (0, length vs - 1)
    return $ vs !! rand
op1Pick (VJunc (MkJunc _ _ set)) | Set.null set = return undef
op1Pick (VJunc (MkJunc JAny _ set)) = do -- pick mainly works on 'any'
    rand <- liftIO $ randomRIO (0 :: Int, (Set.size set) - 1)
    return $ (Set.elems set) !! rand
op1Pick (VJunc (MkJunc JNone _ _)) = return undef
op1Pick (VJunc (MkJunc JAll _ set)) =
    if (Set.size $ set) == 1 then return $ head $ Set.elems set
    else return undef
op1Pick (VJunc (MkJunc JOne dups set)) =
    if (Set.size $ set) == 1 && (Set.size $ dups) == 0
    then return $ head $ Set.elems set
    else return undef
op1Pick v = retError "pick not defined" v

op1Sum :: Val -> Eval Val
op1Sum list = do
    vals <- fromVal list
    foldM (op2Numeric (+)) undef vals

op1Min :: Val -> Eval Val
op1Min v = op1MinMax (== False) v

op1Max :: Val -> Eval Val
op1Max v = op1MinMax (== True) v

-- min_or_max is a function which negates truth/falsehood.
-- This is necessary as op1MinMax should cope with min() as well as max().
op1MinMax :: (Bool -> Bool) -> Val -> Eval Val
op1MinMax min_or_max v = do
    -- We want to have a real Haskell list
    args    <- fromVal v
    -- Extract our comparator sub, or Nothing if none was specified
    (valList, cmp) <- case args of
        (v:vs) -> do
            ifValTypeIsa v "Code"
                (return (vs, Just v))
                (ifValTypeIsa (last args) "Code"
                    (return (init args, Just $ last args))
                    (return (args, Nothing)))
        _  -> return (args, Nothing)
    -- Now let our helper function do the rest
    op1MinMax' min_or_max cmp valList
    where
    op1MinMax' :: (Bool -> Bool) -> (Maybe Val) -> [Val] -> Eval Val
    -- The min or max of an empty list is undef.
    op1MinMax' _ _ [] = return undef
    -- We have to supply our own comparator...
    op1MinMax' _ Nothing valList = foldM default_compare (head valList) valList
    -- or use the one of the user
    op1MinMax' min_or_max (Just subVal) valList = do
          sub <- fromVal subVal
          evl <- asks envEval
          -- Here we execute the user's sub
          foldM (\a b -> do
              rv  <- local (\e -> e{ envContext = cxtItem "Int" }) $ do
                  evl (App (Val sub) Nothing [Val a, Val b])
              int <- fromVal rv
              -- If the return value from the sub was
              --   -1 ==> a < b
              --    0 ==> a == b
              --   +1 ==> a > b
              -- We call min_or_max so we can work for both min() and max().
              return $ if min_or_max (int > (0::VInt)) then a else b) (head valList) valList
    -- This is the default comparision function, which will be used if the user
    -- hasn't specified a own comparision function.
    default_compare a b = do
        a' <- vCastRat a
        b' <- vCastRat b
        let cmp = if a' < b' then (-1) else if a' == b' then 0 else 1
        return $ if min_or_max (cmp > (0::VInt)) then a else b

op1Uniq :: Val -> Eval Val
op1Uniq v = do
    -- We want to have a real Haskell list
    args    <- fromVal v
    -- Extract our comparator sub, or Nothing if none was specified
    (valList, cmp) <- case args of
        (v:vs) -> do
            ifValTypeIsa v "Code"
                (return (vs, Just v))
                (ifValTypeIsa (last args) "Code"
                    (return (init args, Just $ last args))
                    (return (args, Nothing)))
        _  -> return (args, Nothing)
    -- After this parameter unpacking, we begin doing the real work.
    op1Uniq' cmp valList
    where
    op1Uniq' :: (Maybe Val) -> [Val] -> Eval Val
    -- If the user didn't specify an own comparasion sub, we can simply use
    -- Haskell's nub.
    op1Uniq' Nothing valList = return . VList $ nub valList
    -- Else, we have to write our own nubByM and use that.
    op1Uniq' (Just subVal) valList = do
        sub <- fromVal subVal
        evl <- asks envEval
        -- Here we execute the user's sub
        result <- nubByM (\a b -> do
            rv  <- local (\e -> e{ envContext = cxtItem "Bool" }) $ do
                evl (App (Val sub) Nothing [Val a, Val b])
            -- The sub returns either true or false.
            bool <- fromVal rv
            return . VBool $ bool) valList
        return . VList $ result
    -- This is the same as nubBy, only lifted into the Eval monad
    nubByM :: (Val -> Val -> Eval Val) -> [Val] -> Eval [Val]
    nubByM eq l = nubByM' l []
      where
        nubByM' [] _      = return []
        nubByM' (y:ys) xs = do
            -- elemByM returns a Val, but we need a VBool, so we have to use fromVal.
            cond <- fromVal =<< elemByM eq y xs
            if cond then nubByM' ys xs else do
                result <- nubByM' ys (y:xs)
                return (y:result)
        elemByM :: (Val -> Val -> Eval Val) -> Val -> [Val] -> Eval Val
        elemByM _  _ []     = return . VBool $ False
        elemByM eq y (x:xs) = do
            cond <- fromVal =<< eq x y
            -- Same here (we need a VBool, not a Var).
            if cond then return . VBool $ cond else elemByM eq y xs

op2FoldL :: Val -> Val -> Eval Val
op2FoldL sub@(VCode _) list = op2FoldL list sub
op2FoldL list sub = do
    code <- fromVal sub
    op2Fold list $ VCode code{ subAssoc = "left" }

op2Fold :: Val -> Val -> Eval Val
op2Fold sub@(VCode _) list = op2Fold list sub
op2Fold list sub = do
    code <- fromVal sub
    args <- fromVal list
    let arity = length $ subParams code
    if arity < 2 then fail "Cannot reduce() using a unary or nullary function." else do
    -- n is the number of *additional* arguments to be passed to the sub.
    -- Ex.: reduce { $^a + $^b       }, ...   # n = 1
    -- Ex.: reduce { $^a + $^b + $^c }, ...   # n = 2
    let n = arity - 1
    -- Break on empty list.
    if null args then return undef else do
    let doFold xs = do
        evl <- asks envEval
        local (\e -> e{ envContext = cxtItemAny }) $ do
            evl (App (Val sub) Nothing (map Val xs))
    case subAssoc code of
        "right" -> do
            let args' = reverse args
            foldMn args' n (doFold . reverse)
        "chain" -> if arity /= 2
            then fail
                "When reducing using a chain-associative sub,\nthe sub must take exactly two arguments."
            else callCC $ \esc -> do
                let doFold' x y = do
                    val <- doFold [x, y]
                    case val of
                        VBool False -> esc val
                        _           -> return y
                foldM doFold' (head args) (tail args)
                return $ VBool True
        "non"   -> fail $ "Cannot reduce over non-associativity"
        _       -> foldMn args n doFold -- "left", "pre"
    where
    -- This is a generalized foldM.
    -- It takes an input list (from which the first elem will be used as start
    -- value), the number of additional arguments, and a reducing function.
    foldMn :: [Val] -> Int -> ([Val] -> Eval Val) -> Eval Val
    foldMn list n f = foldM (\a b -> f (a:b)) (head list) $ list2LoL n $ drop 1 list

op2Grep :: Val -> Val -> Eval Val
op2Grep sub@(VCode _) list = op2Grep list sub
op2Grep (VList [v@(VRef _)]) sub = op2Grep v sub
op2Grep list sub = do
    args <- fromVal list
    vals <- (`filterM` args) $ \x -> do
        evl <- asks envEval
        rv  <- local (\e -> e{ envContext = cxtItem "Bool" }) $ do
            evl (App (Val sub) Nothing [Val x])
        fromVal rv
    return $ VList vals

op2Map :: Val -> Val -> Eval Val
op2Map sub@(VCode _) list = op2Map list sub
op2Map (VList [v@(VRef _)]) sub = op2Map v sub
op2Map list sub = do
    args  <- fromVal list
    arity <- liftM length $ (fromVal sub >>= return . subParams)
    evl  <- asks envEval
    vals <- mapMn args arity $ \x -> do
        rv  <- local (\e -> e{ envContext = cxtSlurpyAny }) $ do
            evl (App (Val sub) Nothing (map Val x))
        fromVal rv
    return $ VList vals
    where
    -- Takes a list, an arity, and a function.
    mapMn           :: [Val] -> Int -> ([Val] -> Eval [Val]) -> Eval [Val]
    mapMn list n f   = mapMn' (list2LoL n list) f
    -- Takes a LoL and a function and applies the function to the inputlist.
    mapMn'          :: [[Val]] -> ([Val] -> Eval [Val]) -> Eval [Val]
    mapMn' (x:xs) f  = liftM2 (++) (f x) (mapMn' xs f)
    mapMn' []     _  = return []

{-|
Takes an int and a list and returns a LoL.
Ex.:

> list2LoL 3 [1,2,3,4,5] = [[1,2,3],[4,5,undef]]
-}
list2LoL :: Int -> [Val] -> [[Val]]
list2LoL n list
    | n == 0           = fail "Cannot map() using a nullary function."
    -- If the list has exactly n elements, we've finished our work.
    | length list == n = [list]
    -- If the list is empty, we're done, too.
    | length list == 0 = []
    -- But if the list contains more elems than we need, we process the
    -- first n ones and the rest separately.
    | length list  > n = (list2LoL n $ take n list) ++ (list2LoL n $ drop n list)
    -- And if the list contains less elems than we need, we pad with undefs.
    | length list  < n = list2LoL n $ list ++ [undef :: Val]
    | otherwise        = fail "Invalid arguments to internal function list2LoL passed."

op2Join :: Val -> Val -> Eval Val
op2Join (VList [x@(VRef _)]) y = op2Join x y
op2Join x y = do
    (strVal, listVal) <- ifValTypeIsa x "Scalar"
        (return (x, y))
        (return (y, x))
    str     <- fromVal strVal
    ref     <- fromVal listVal
    list    <- readRef ref
    strList <- fromVals list
    return . VStr . concat . intersperse str $ strList

sortByM :: (Val -> Val -> Eval Bool) -> [Val] -> Eval [Val]
sortByM _ []  = return []
sortByM _ [x] = return [x]
sortByM f xs  = do
    let (as, bs) = splitAt (length xs `quot` 2) xs
    aSorted <- sortByM f as
    bSorted <- sortByM f bs
    doMerge f aSorted bSorted
    where
    doMerge :: (Val -> Val -> Eval Bool) -> [Val] -> [Val] -> Eval [Val]
    doMerge _ [] ys = return ys
    doMerge _ xs [] = return xs
    doMerge f (x:xs) (y:ys) = do
        isLessOrEqual <- f x y
        if isLessOrEqual
            then do
                rest <- doMerge f xs (y:ys)
                return (x:rest)
            else do
                rest <- doMerge f (x:xs) ys
                return (y:rest)

op1HyperPrefix :: VCode -> Val -> Eval Val
op1HyperPrefix sub (VRef ref) = do
    x <- readRef ref
    op1HyperPrefix sub x
op1HyperPrefix sub x
    | VList x' <- x
    = fmap VList $ hyperList x'
    | otherwise
    = fail "Hyper OP only works on lists"
    where
    doHyper x
        | VRef x' <- x
        = doHyper =<< readRef x'
        | otherwise
        = enterEvalContext cxtItemAny $ App (Val $ VCode sub) Nothing [Val x]
    hyperList []     = return []
    hyperList (x:xs) = do
        val  <- doHyper x
        rest <- hyperList xs
        return (val:rest)

op1HyperPostfix :: VCode -> Val -> Eval Val
op1HyperPostfix = op1HyperPrefix

op2Hyper :: VCode -> Val -> Val -> Eval Val
op2Hyper sub (VRef ref) y = do
    x <- readRef ref
    op2Hyper sub x y
op2Hyper sub x (VRef ref) = do
    y <- readRef ref
    op2Hyper sub x y
op2Hyper sub x y
    | VList x' <- x, VList y' <- y
    = fmap VList $ hyperLists x' y'
    | VList x' <- x
    = fmap VList $ mapM ((flip doHyper) y) x'
    | VList y' <- y
    = fmap VList $ mapM (doHyper x) y'
    | otherwise
    = fail "Hyper OP only works on lists"
    where
    doHyper x y = enterEvalContext cxtItemAny $ App (Val $ VCode sub) Nothing [Val x, Val y]
    hyperLists [] [] = return []
    hyperLists xs [] = return xs
    hyperLists [] ys = return ys
    hyperLists (x:xs) (y:ys) = do
        val  <- doHyper x y
        rest <- hyperLists xs ys
        return (val:rest)

