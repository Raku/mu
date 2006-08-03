{-# OPTIONS -fallow-undecidable-instances -fallow-incoherent-instances #-}

import Test

import Prelude hiding (lookup)

import Data.List (sort)
import Judy.CollectionsM --as CM
import Judy.Map2 as J

main = no_plan $ do

t "Simple" $ do
    s <- new :: IO (Map2 Int Int)
    lookup 1 s    .=> Nothing
    insert 1 42 s
    lookup 1 s    .=> Just 42

t "Delete" $ do
    s <- new :: IO (Map2 Int Int)
    lookup 3 s    .=> Nothing
    insert 3 42 s
    lookup 3 s    .=> Just 42

    insert 37 59 s
    delete 3 s
    lookup 3 s  .=> Nothing
    lookup 37 s .=> Just 59

t "Overwrite" $ do
    s <- new :: IO (Map2 Int Int)
    insert 3 1234 s
    insert 2222 1234 s
    insert 3 42 s
    insert 123 42 s
    lookup 3 s .=> Just 42

t "Member" $ do
    s <- new :: IO (Map2 Int Int)
    member 3 s     .=> False
    insert 3 42 s
    member 37 s    .=> False
    member 3 s     .=> True
    delete 12345 s --- doesn't exist
    member 3 s     .=> True

t "Elems" $ do
    s <- new :: IO (Map2 Int Int)
    elems s .=> []
    insert 3 42 s
    insert 37 1 s
    insert 0 2 s
    elems s .=> [2,42,1]

t "Keys" $ do
    s <- new :: IO (Map2 Int Int)
    keys s .-= []
    insert 3 42 s
    insert 37 1 s
    insert 0 2 s
    keys s .=> [0,3,37]
    delete 37 s
    delete 15 s
    keys s .=> [0,3]

t "StringValue" $ do
    s <- new :: IO (Map2 Int String)
    toList s .-= []

    insert 22 "string" s
    insert 59 "i am not a number" s
    toList s    .=> [(22, "string"), (59,"i am not a number")] 
    member 22 s .=> True

    delete 22 s
    member 22 s .=> False
    lookup 59 s .=> Just "i am not a number"

t "StringValueDel" $ do
    s <- new :: IO (Map2 Int String)
    toList s .-= []

    insert 22 "string" s
    insert 23 "string" s
    insert 59 "i am not a number" s
    toList s .=> [(22, "string"), (23, "string"), (59,"i am not a number")] 
    member 22 s .=> True

    delete 22 s
    member 22 s .=> False
    lookup 59 s .=> Just "i am not a number"
    lookup 23 s .=> Just "string"

t "SwapMaps" $ do
    m1 <- fromList [(1,2),(2,3),(4,7)] :: IO (J.Map2 Int Int)
    m2 <- fromList [(1,42),(2,42),(3,42)] :: IO (J.Map2 Int Int)
    lookup 2 m1 .=> Just 3
    lookup 2 m2 .=> Just 42
    lookup 3 m2 .=> Just 42

    J.swapMaps m1 m2
    lookup 2 m1 .=> Just 42
    lookup 2 m2 .=> Just 3
    lookup 3 m2 .=> Nothing

t "Alter" $ do
    m <- fromList [(1,2), (2,3), (4,5)] :: IO (J.Map2 Int Int)
    lookup 1 m .=> Just 2

    alter (const (Just 42)) 3 m
    lookup 1 m .=> Just 2
    lookup 3 m .=> Just 42

    alter (const (Just 42)) 2 m
    lookup 2 m .=> Just 42

    alter (const Nothing) 1 m
    lookup 1 m .=> Nothing

t "RevList" $ do
    let l = [(1,2), (2,3), (4,5)]
    m <- fromList l :: IO (J.Map2 Int Int)
    J.toRevList m .=> reverse l

    insert 3 10 m
    J.toRevList m .=> reverse (sort $ (3,10):l)

t "Size" $ do
    let l = [0..] `zip` [1..20]
    m <- fromList l :: IO (J.Map2 Int Int)
    J.size m .=> 20
    delete 0 m
    delete 1 m
    J.size m .=> 18
    insert 2 3 m
    insert 21 3 m
    J.size m .=> 19

t "TakeFirst et al" $ do
    let l = [0..100] `zip` [100..]
    m <- fromList l :: IO (J.Map2 Int Int)
    J.takeFirstElems 5 m  .=> [100..104]
    J.takeFirst 5 m       .=> [0..] `zip` [100..104]
    J.takeFirstElems 10 m .=> [100..109]
    delete 0 m
    delete 1 m
    J.takeFirstElems 3 m  .=> [102..104]
    J.takeFirst 3 m       .=> [2..4] `zip` [102..104]

t "TakeLast et al" $ do
    let l = [0..100] `zip` [100..]
    m <- fromList l :: IO (J.Map2 Int Int)
    J.takeLastElems 5 m  .=> [196..200]
    J.takeLast 5 m       .=> [96..] `zip` [196..200]
    J.takeLastElems 10 m .=> [191..200]
    delete 100 m
    delete 99 m
    J.takeLastElems 3 m  .=> [196..198]
    J.takeLast 3 m       .=> [96..] `zip` [196..198]

-- TODO: test some crazy haskell type as value (to check stableptrs)
