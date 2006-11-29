-- #prune

-- |
--
-- Module      :  C3
-- Copyright   :  (c) 2006 Caio Marcelo
-- License     :  MIT
--
-- Maintainer  :  cmarcelo@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- C3 method resolution order implementation based on algorithm described
-- in /The Python 2.3 Method Resolution Order, v1.4/, by Michele Simionato
-- available at <http://www.python.org/download/releases/2.3/mro/>. Some
-- tests also copied from Perl module Algorithm::C3.
--
-- The 'main' function contains the test cases.

module Pugs.Class.C3 (linearize) where

import Data.List (nub)
import Control.Monad (when)
--import Control.Monad.Error -- used for testing

-- | Returns the a linearization using C3 algorithm. Takes a function
-- and an element. We can apply the function in this element to obtain
-- its parents.
linearize :: (Monad m, Eq a) => (a -> m [a]) -> a -> m [a]
linearize = linearize' []

-- | Implementation behind linearize. Keeps a list of seen elements to
-- detect loops in the hierarchy.
linearize' :: (Monad m, Eq a) => [a] -> (a -> m [a]) -> a -> m [a]
linearize' seen p root = do
    when (root `elem` seen) $ fail "loop detected in hierarchy"
    root_ps <- p root
    gran_ps <- mapM (linearize' (root : seen) p) root_ps
    let root_ps' = map (\x -> [x]) root_ps 
        gran_ps' = filter (not . null) gran_ps
    a <- merge (gran_ps' ++ root_ps')
    return (root : a)

-- | The merge operation from C3.
merge :: (Monad m, Eq a) => [[a]] -> m [a]
merge []    = return []
merge l     = merge_round candidates l
    where
    candidates = nub (map head l)

-- | Auxiliar function for the merge operation, given a candidate list,
-- find a good candidate, return 'Nothing' if none of them can be used,
-- meaning an impossible merge due conflict. If it finds one, calls
-- 'merge' to find next element in the linearization.
merge_round :: (Monad m, Eq a) => [a] -> [[a]] -> m [a]
merge_round _  [] = return []
merge_round [] _  = fail "merge conflict"
merge_round (c:cs) l@(x:xs)
    | good c l = do
        a <- merge clean_list
        return (c:a)
    | otherwise = merge_round cs l
    where
    clean_list      = filter (not . null) (merge_clean c l)
    merge_clean c   = map (\x -> filter ((/=) c) x)

-- |Returns 'True' if a candidate element isn't present in the tail
-- of each list.
good c []     = True
good c (x:xs)
    | c `elem` (tail x) = False
    | otherwise         = good c xs


{-

-- Tests
main = do
    test_many "Simple example 1" ex1 [[O], [A,O], [B]]
    test_many "Simple example 2" ex2 [[O], [A,B,C,O], [B,O], [C,O],[D]]
    test_many "Python MRO first example" py1 [
        [O], [A, B, C, D, E, F, O], [B, D, E, O],
        [C, D, F, O], [D, O], [E, O], [F, O]]
    test_many "Python MRO second example" py2 [
        [O], [A, B, E, C, D, F, O], [B, E, D, O],
        [C, D, F, O], [D, O], [E, O], [F, O]]
    test_many2 "Python MRO conflict example" py3 [
        (O, Just [O]),
        (A, Just [A, X, Y, O]),
        (B, Just [B, Y, X, O]),
        (X, Just [X, O]),
        (Y, Just [Y, O]),
        (C, Nothing),
        (D, Nothing)]
    test_many "Python MRO example which breaks old Py MRO" py4 [
        [O], [A, O], [B, O], [C, O], [D, O], [E, O],
        [K1, A, B, C, O], [K2, D, B, E, O], [K3, D, A, O],
        [Z, K1, K2, K3, D, A, B, C, E, O]]
    test_many "Complex merge from Algorithm::C3" complex [
        [A], [B], [C],
        [D, A, B, C], [E, D, A, B, C], [F, E, D, A, B, C],
        [G, D, A, B, C], [H, G, D, A, B, C],
        [I, H, G, F, E, D, A, B, C],
        [J, F, E, D, A, B, C],
        [K, J, I, H, G, F, E, D, A, B, C]]
    test "Complex merge with loop #1 (A::C3)" infinite_loop1 K (Left "loop detected in hierarchy")
    test "Complex merge with loop #2 (A::C3)" infinite_loop2 K (Left "loop detected in hierarchy")
    test "Complex merge with loop #3 (A::C3)" infinite_loop3 K (Left "loop detected in hierarchy")
    test "Complex merge with loop #4 (A::C3)" infinite_loop4 K (Left "loop detected in hierarchy")
    test "Complex merge with loop #5 (A::C3)" infinite_loop5 K (Left "loop detected in hierarchy")
    test "Complex merge with loop #6 (A::C3)" infinite_loop6 K (Left "loop detected in hierarchy")
    test "Complex merge with loop #7 (A::C3)" infinite_loop7 K (Left "loop detected in hierarchy")
    test "Complex merge with loop #8 (A::C3)" infinite_loop8 K (Left "loop detected in hierarchy")


        

data Object =
 O | A | B | C | D | E | F | G | H | I | J | K | K1 | K2 | K3 | X | Y | Z
 deriving (Eq, Show)

ex1 x = case x of
    A -> [O]
    _ -> []

ex2 x = case x of
    A -> [B,C]
    B -> [O]
    C -> [O]
    _ -> []

py1 x = case x of
    O -> []
    A -> [B,C]
    B -> [D,E]
    C -> [D,F]
    _ -> [O]

py2 x = case x of
    O -> []
    A -> [B,C]
    B -> [E,D]
    C -> [D,F]
    _ -> [O]

py3 x = case x of
    O -> []
    A -> [X, Y]
    B -> [Y, X]
    C -> [A, B]
    D -> [B, A]
    _ -> [O]

py4 x = case x of
    O -> []
    K1 -> [A,B,C]
    K2 -> [D,B,E]
    K3 -> [D,A]
    Z -> [K1,K2,K3]
    _ -> [O]

complex x = case x of
    D -> [A,B,C]
    E -> [D]
    F -> [E]
    G -> [D]
    H -> [G]
    I -> [H,F]
    J -> [F]
    K -> [J,I]
    _ -> []

infinite_loop1 x = case x of
    E -> [F]
    y -> complex y

infinite_loop2 x = case x of
    C -> [F]
    y -> complex y

infinite_loop3 x = case x of
    A -> [K]
    y -> complex y

infinite_loop4 x = case x of
    J -> [F, K]
    y -> complex y

infinite_loop5 x = case x of
    H -> [G, K]
    y -> complex y

infinite_loop6 x = case x of
    B -> [B]
    y -> complex y

infinite_loop7 x = case x of
    K -> [I, J, K]
    y -> complex y

infinite_loop8 x = case x of
    D -> [A, B, C, H]
    y -> complex y


-- Helper functions for testing
test_many  name h l = mapM_ (\x -> test name h (head x) (Just x)) l
test_many2 name h l = mapM_ (\(x,y) -> test name h x y) l
test name h e result = do
    let m = linearize (return . h) e
    if m == result
        then putStrLn    $ "ok - " ++ name ++ ", element " ++ (show e)
        else do putStrLn $ "not ok - " ++ name ++ ", element " ++ (show e)
                putStrLn $ "# expected: " ++ (show result)
                putStrLn $ "#      got: " ++ (show m)

-}
