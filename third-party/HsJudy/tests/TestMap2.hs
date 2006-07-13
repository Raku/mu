{-# OPTIONS -fallow-undecidable-instances -fallow-incoherent-instances #-}

import Prelude hiding (lookup)

import Data.List (sort)
import Judy.CollectionsM --as CM
import Judy.Map2 as JM


main = do
    putStrLn "# Map2 tests:"
    testSimple
    testDelete
    testOverwrite
    testMember
    testElems
    testKeys
    testStringValue
    testSwapMaps
    testAlter2

check l = do
    if and l
        then putStrLn "ok"
        else putStrLn "BAD"

testSimple = do
    putStr "simple: \t"
    s <- new :: IO (Map2 Int Int)
    a <- lookup 1 s
    alter 1 42 s
    b <- lookup 1 s
    check [a == Nothing, b == Just 42]

testDelete = do
    putStr "delete: \t"
    s <- new :: IO (Map2 Int Int)
    a <- lookup 3 s
    alter 3 42 s
    b <- lookup 3 s
    alter 37 59 s
    delete 3 s
    c <- lookup 3 s
    d <- lookup 37 s
    check [a == Nothing, b == Just 42, c == Nothing, d == Just 59]

testOverwrite = do
    putStr "overwrite: \t"
    s <- new :: IO (Map2 Int Int)
    alter 3 1234 s
    alter 2222 1234 s
    alter 3 42 s
    alter 123 42 s
    a <- lookup 3 s
    check [a == Just 42]

testMember = do
    putStr "member: \t"
    s <- new :: IO (Map2 Int Int)
    a <- member 3 s
    alter 3 42 s
    b <- member 37 s
    c <- member 3 s
    delete 12345 s -- doesn't exist
    d <- member 3 s
    check [not a, not b, c, d]

testElems = do
    putStr "elems:  \t"
    s <- new :: IO (Map2 Int Int)
    a <- elems s
    alter 3 42 s
    alter 37 1 s
    alter 0 2 s
    b <- elems s
    check [a == [], (sort b) == [1,2,42]]

testKeys = do
    putStr "keys:   \t"
    s <- new :: IO (Map2 Int Int)
    a <- keys s
    alter 3 42 s
    alter 37 1 s
    alter 0 2 s
    b <- keys s
    delete 37 s
    delete 15 s
    c <- keys s
    check [a == [], (sort b) == [0, 3, 37], (sort c) == [0, 3]]

testStringValue = do
    putStr "string-value: \t"
    s <- new :: IO (Map2 Int String)
    a <- toList s
    alter 22 "string" s
    alter 59 "i am not a number" s
    b <- toList s
    c <- member 22 s
    delete 22 s
    d <- member 22 s
    e <- lookup 59 s
    check [a == [], (sort b) == [(22, "string"), (59,"i am not a number")],
           c, not d, e == Just "i am not a number"]

testSwapMaps = do
    putStr "swap maps: \t"
    m1 <- fromList [(1,2),(2,3),(4,7)] :: IO (JM.Map2 Int Int)
    m2 <- fromList [(1,42),(2,42),(3,42)] :: IO (JM.Map2 Int Int)
    a <- lookup 2 m1
    b <- lookup 2 m2
    JM.swapMaps m1 m2
    c <- lookup 2 m1
    d <- lookup 2 m2
    e <- lookup 3 m2
    check [a == Just 3, b == Just 42, c == Just 42, d == Just 3, e == Nothing]

testAlter2 = do
    putStr "alter2: \t"
    m <- fromList [(1,2), (2,3), (4,5)] :: IO (JM.Map2 Int Int)
    a <- lookup 1 m
    JM.alter2 (const (Just 42)) 3 m
    b <- lookup 1 m
    c <- lookup 3 m
    JM.alter2 (const (Just 42)) 2 m
    d <- lookup 2 m
    JM.alter2 (const Nothing) 1 m
    e <- lookup 1 m
    check [a == Just 2, b == Just 2, c == Just 42, d == Just 42, e == Nothing]




-- TODO: test some crazy haskell type as value (to check stableptrs)

