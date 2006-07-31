{-# OPTIONS -fallow-undecidable-instances -fallow-incoherent-instances #-}

import Test

import Prelude hiding (lookup)

import Data.List (sort)
import Judy.CollectionsM --as CM
import Judy.Map2 as J

main = no_plan $ do
    testSimple
    testDelete
    testOverwrite
    testMember
    testElems
    testKeys
    testStringValue
    testStringValueDel
    testSwapMaps
    testAlter
    testRevList

testSimple = do
    say "Simple"
    s <- new :: IO (Map2 Int Int)
    lookup 1 s    .=> Nothing
    insert 1 42 s
    lookup 1 s    .=> Just 42

testDelete = do
    say "Delete"
    s <- new :: IO (Map2 Int Int)
    lookup 3 s    .=> Nothing
    insert 3 42 s
    lookup 3 s    .=> Just 42

    insert 37 59 s
    delete 3 s
    lookup 3 s  .=> Nothing
    lookup 37 s .=> Just 59

testOverwrite = do
    say "Overwrite"
    s <- new :: IO (Map2 Int Int)
    insert 3 1234 s
    insert 2222 1234 s
    insert 3 42 s
    insert 123 42 s
    lookup 3 s .=> Just 42

testMember = do
    say "Member"
    s <- new :: IO (Map2 Int Int)
    member 3 s     .=> False
    insert 3 42 s
    member 37 s    .=> False
    member 3 s     .=> True
    delete 12345 s --- doesn't exist
    member 3 s     .=> True

testElems = do
    say "Elems"
    s <- new :: IO (Map2 Int Int)
    elems s .=> []
    insert 3 42 s
    insert 37 1 s
    insert 0 2 s
    elems s .=> [2,42,1]

testKeys = do
    say "Keys"
    s <- new :: IO (Map2 Int Int)
    keys s .-= []
    insert 3 42 s
    insert 37 1 s
    insert 0 2 s
    keys s .=> [0,3,37]
    delete 37 s
    delete 15 s
    keys s .=> [0,3]

testStringValue = do
    say "StringValue"
    s <- new :: IO (Map2 Int String)
    toList s .-= []

    insert 22 "string" s
    insert 59 "i am not a number" s
    toList s    .=> [(22, "string"), (59,"i am not a number")] 
    member 22 s .=> True

    delete 22 s
    member 22 s .=> False
    lookup 59 s .=> Just "i am not a number"

testStringValueDel = do
    say "StringValueDel"
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

testSwapMaps = do
    say "SwapMaps"
    m1 <- fromList [(1,2),(2,3),(4,7)] :: IO (J.Map2 Int Int)
    m2 <- fromList [(1,42),(2,42),(3,42)] :: IO (J.Map2 Int Int)
    lookup 2 m1 .=> Just 3
    lookup 2 m2 .=> Just 42
    lookup 3 m2 .=> Just 42

    J.swapMaps m1 m2
    lookup 2 m1 .=> Just 42
    lookup 2 m2 .=> Just 3
    lookup 3 m2 .=> Nothing

testAlter = do
    say "Alter"
    m <- fromList [(1,2), (2,3), (4,5)] :: IO (J.Map2 Int Int)
    lookup 1 m .=> Just 2

    alter (const (Just 42)) 3 m
    lookup 1 m .=> Just 2
    lookup 3 m .=> Just 42

    alter (const (Just 42)) 2 m
    lookup 2 m .=> Just 42

    alter (const Nothing) 1 m
    lookup 1 m .=> Nothing

testRevList = do
    say "RevList"
    let l = [(1,2), (2,3), (4,5)]
    m <- fromList l :: IO (J.Map2 Int Int)
    J.toRevList m .=> reverse l

    insert 3 10 m
    J.toRevList m .=> reverse (sort $ (3,10):l)

-- TODO: test some crazy haskell type as value (to check stableptrs)

