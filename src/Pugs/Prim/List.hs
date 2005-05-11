module Pugs.Prim.List (
    op0Zip, op1Fold, op1Pick, op1Sum,
    op2Fold, op2Grep, op2Map, op2Join,
    sortByM,
) where
import Pugs.Internals
import Pugs.AST
import Pugs.Types
import qualified Data.Set as Set

import Pugs.Prim.Numeric

op0Zip = fmap (VList . concat . op0Zip') . mapM fromVal

op0Zip' :: [[Val]] -> [[Val]]
op0Zip' lists | all null lists = []
op0Zip' lists = (map zipFirst lists):(op0Zip' (map zipRest lists))
    where
    zipFirst []     = undef
    zipFirst (x:_)  = x
    zipRest  []     = []
    zipRest  (_:xs) = xs

op1Fold :: (Val -> Val -> Eval Val) -> Val -> Eval Val
op1Fold op v = do
    args    <- fromVal v
    case args of
        (a:as)  -> foldM op a as
        _       -> return undef

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
op1Pick v = return $ VError "pick not defined" (Val v)

op1Sum :: Val -> Eval Val
op1Sum list = do
    vals <- fromVal list
    foldM (op2Numeric (+)) undef vals

op2Fold :: Val -> Val -> Eval Val
op2Fold sub@(VCode _) list = op2Fold list sub
op2Fold list sub = do
    args <- fromVal list
    if null args then return undef else do
    let doFold x y = do
        evl <- asks envEval
        local (\e -> e{ envContext = cxtItemAny }) $ do
            evl (App (Val sub) [Val x, Val y] [])
    foldM doFold (head args) (tail args)

op2Grep :: Val -> Val -> Eval Val
op2Grep sub@(VCode _) list = op2Grep list sub
op2Grep list sub = do
    args <- fromVal list
    vals <- (`filterM` args) $ \x -> do
        evl <- asks envEval
        rv  <- local (\e -> e{ envContext = cxtItem "Bool" }) $ do
            evl (App (Val sub) [Val x] [])
        fromVal rv
    return $ VList vals

op2Map :: Val -> Val -> Eval Val
op2Map sub@(VCode _) list = op2Map list sub
op2Map list sub = do
    args <- fromVal list
    vals <- (`mapM` args) $ \x -> do
        evl <- asks envEval
        rv  <- local (\e -> e{ envContext = cxtSlurpyAny }) $ do
            evl (App (Val sub) [Val x] [])
        fromVal rv
    return $ VList $ concat vals

op2Join :: Val -> Val -> Eval Val
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
