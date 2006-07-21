{-# OPTIONS -fallow-undecidable-instances -fallow-incoherent-instances #-}


import Data.List (sort)
import System.Mem

--import qualified Judy.MapSL as JSL
import qualified Judy.Map as J

import Judy.Map (Stringable (..))
import Judy.CollectionsM
import Judy.Freeze

import Prelude hiding (lookup)


--type M = JSL.MapSL
--swapMaps = JSL.swapMaps

type M = J.Map
swapMaps = J.swapMaps


main = do
    putStrLn "# Map tests:"
    
    -- FIXME: A better way to organize this tests must exist...
    testSimple
    testDelete
    testOverwrite
    testMember
    testElems
    testKeys
    testIntKey
    testIntKeyDelete
    testLotsOfMem
    testFrozenMap
    testSwapMaps
    testAlter
    
    -- Stress test for MiniGC
    --sequence $ take 5000 $ repeat testAlter2


check l = do
    if and l
        then putStrLn "ok"
        else putStrLn "BAD"

newStringInt = new :: IO (M String Int)
newIntString = new :: IO (M Int String)
type IntIntMap = M Int Int

testSimple = do
    putStr "simple: \t"
    s <- newStringInt
    a <- lookup "haha" s
    insert "haha" 42 s
    b <- lookup "haha" s
    check [a == Nothing, b == Just 42]

testDelete = do
    putStr "delete: \t"
    s <- newStringInt
    a <- lookup "haha" s
    insert "haha" 42 s
    b <- lookup "haha" s
    insert "ahoy" 59 s
    delete "haha" s
    c <- lookup "haha" s
    d <- lookup "ahoy" s
    check [a == Nothing, b == Just 42, c == Nothing, d == Just 59]

testOverwrite = do
    putStr "overwrite: \t"
    s <- newStringInt
    insert "haha" 1234 s
    insert "dois" 1234 s
    insert "haha" 42 s
    insert "oi" 42 s
    a <- lookup "haha" s
    check [a == Just 42]

testMember = do
    putStr "member: \t"
    s <- newStringInt
    a <- member "haha" s
    insert "haha" 42 s
    b <- member "ahoy" s
    c <- member "haha" s
    delete "hahaha" s -- doesn't exist
    d <- member "haha" s
    check [not a, not b, c, d]

testElems = do
    putStr "elems:  \t"
    s <- newStringInt
    a <- elems s
    insert "haha" 42 s
    insert "ahoy" 1 s
    insert "nop" 2 s
    b <- elems s
    check [a == [], (sort b) == [1,2,42]]

testKeys = do
    putStr "keys:   \t"
    s <- newStringInt
    a <- keys s
    insert "haha" 42 s
    insert "ahoy" 1 s
    insert "nop" 2 s
    b <- keys s
    delete "ahoy" s
    delete "nada" s
    c <- keys s
    check [a == [], (sort b) == ["ahoy", "haha", "nop"], (sort c) == ["haha", "nop"]]

instance Stringable Int where
    toString = show
    fromString = read

testIntKey = do
    putStr "int-key map: \t"
    s <- newIntString
    a <- keys s
    insert 22 "string" s
    insert 59 "i am not a number" s
    b <- keys s
    c <- elems s
    d <- member 22 s
    delete 22 s
    e <- member 22 s
    f <- lookup 59 s
    check [a == [], (sort b) == [22,59],
           (sort c) == ["i am not a number", "string"],
           d, not e, f == Just "i am not a number"]

testIntKeyDelete = do
    putStr "int-key del: \t"
    s <- newIntString
    insert 22 "string" s
    insert 23 "string" s
    insert 24 "ahha" s
    insert 25 "oieee" s
    delete 22 s
    a <- lookup 23 s
    check [a == Just "string"]

testLotsOfMem = do
    putStrLn $ "# " ++ (show $ last $ take 100000 [1..])
    putStr "lots of mem:  \t"
    s <- newStringInt
    a <- elems s
    insert "haha" 42 s
    insert "ahoy" 1 s
    insert "nop" 2 s
    b <- elems s
    check [a == [], (sort b) == [1,2,42]]

testFrozenMap = do
    putStr "frozen map: \t"
    let m = fromListF [(1,2),(2,3),(3,4)] :: Frozen IntIntMap
    let a = memberF 1 m
    let b = memberF 2 m
    let c = memberF 42 m
    let d = lookupF 1 m
    let e = lookupF 42 m
    check [a, b, not c, d == Just 2, e == Nothing] 

testSwapMaps = do
    putStr "swap maps: \t"
    m1 <- fromList [(1,2),(2,3),(4,7)] :: IO IntIntMap
    m2 <- fromList [(1,42),(2,42),(3,42)] :: IO IntIntMap
    a <- lookup 2 m1
    b <- lookup 2 m2
    swapMaps m1 m2
    c <- lookup 2 m1
    d <- lookup 2 m2
    e <- lookup 3 m2
    check [a == Just 3, b == Just 42, c == Just 42, d == Just 3, e == Nothing]

testAlter = do
    putStr "alter:   \t"
    m <- fromList [(1,2), (2,3), (4,5)] :: IO IntIntMap
    a <- lookup 1 m
    alter (const (Just 42)) 3 m
    b <- lookup 1 m
    c <- lookup 3 m
    alter (const (Just 42)) 2 m
    d <- lookup 2 m
    alter (const Nothing) 1 m
    e <- lookup 1 m
    check [a == Just 2, b == Just 2, c == Just 42, d == Just 42, e == Nothing]

