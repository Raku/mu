{-# OPTIONS -fallow-undecidable-instances -fallow-incoherent-instances #-}

import qualified Judy.Map as JMap

import Data.List (sort)
import Judy.CollectionsM --as CM
import Prelude hiding (lookup)


main = do
    putStrLn "# Map tests:"
    
    -- FIXME: A better way to organize this tests must exist...
 
    -- KILLER SEQUENCE:
    --putStrLn $ show $ last $ take 100000 [1..]
    testElems


--    sequence $ take 1000 $ repeat testSimple
    sequence [testSimple, testDelete, testOverwrite, testMember, testElems, testKeys, testIntKey]

check l = do
    if and l
        then putStrLn "ok"
        else putStrLn "BAD"

testSimple = do
    putStr "simple: \t"
    s <- new :: IO (JMap.Map String Int)
    a <- lookup "haha" s
    alter "haha" 42 s
    b <- lookup "haha" s
    check [a == Nothing, b == Just 42]

testDelete = do
    putStr "delete: \t"
    s <- new :: IO (JMap.Map String Int)
    a <- lookup "haha" s
    alter "haha" 42 s
    b <- lookup "haha" s
    alter "ahoy" 59 s
    delete "haha" s
    c <- lookup "haha" s
    d <- lookup "ahoy" s
    check [a == Nothing, b == Just 42, c == Nothing, d == Just 59]

testOverwrite = do
    putStr "overwrite: \t"
    s <- new :: IO (JMap.Map String Int)
    alter "haha" 1234 s
    alter "dois" 1234 s
    alter "haha" 42 s
    alter "oi" 42 s
    a <- lookup "haha" s
    check [a == Just 42]

testMember = do
    putStr "member: \t"
    s <- new :: IO (JMap.Map String Int)
    a <- member "haha" s
    alter "haha" 42 s
    b <- member "ahoy" s
    c <- member "haha" s
    delete "hahaha" s -- doesn't exist
    d <- member "haha" s
    check [not a, not b, c, d]

testElems = do
    putStr "elems:  \t"
    s <- new :: IO (JMap.Map String Int)
    a <- JMap.elems s
    alter "haha" 42 s
    alter "ahoy" 1 s
    alter "nop" 2 s
    b <- JMap.elems s
    check [a == [], (sort b) == [1,2,42]]

testKeys = do
    putStr "keys:   \t"
    s <- new :: IO (JMap.Map String Int)
    a <- JMap.keys s
    alter "haha" 42 s
    alter "ahoy" 1 s
    alter "nop" 2 s
    b <- JMap.keys s
    delete "ahoy" s
    delete "nada" s
    c <- JMap.keys s
    check [a == [], (sort b) == ["ahoy", "haha", "nop"], (sort c) == ["haha", "nop"]]

instance JMap.Stringable Int where
    toString = show
    fromString = read



testIntKey = do
    putStr "int-key map: \t"
    s <- new :: IO (JMap.Map Int String)
    a <- JMap.keys s
    alter 22 "string" s
    alter 59 "i am not a number" s
    b <- JMap.keys s
    c <- JMap.elems s
    d <- member 22 s
    delete 22 s
    e <- member 22 s
    f <- lookup 59 s
    check [a == [], (sort b) == [22,59],
           (sort c) == ["i am not a number", "string"],
           d, not e, f == Just "i am not a number"]


