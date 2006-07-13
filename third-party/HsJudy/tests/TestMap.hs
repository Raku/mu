{-# OPTIONS -fallow-undecidable-instances -fallow-incoherent-instances #-}

--import qualified Judy.MapSL as JM
import qualified Judy.Map as JM

import Judy.Map (Stringable (..))

import Data.List (sort)
import Judy.CollectionsM --as CM
import Judy.Freeze
import Prelude hiding (lookup)


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
    testLotsOfMem
    testFrozenMap
    testSwapMaps
    testAlter2

check l = do
    if and l
        then putStrLn "ok"
        else putStrLn "BAD"

--newStringInt = new :: IO (JM.MapSL String Int)
--newIntString = new :: IO (JM.MapSL Int String)

newStringInt = new :: IO (JM.Map String Int)
newIntString = new :: IO (JM.Map Int String)

testSimple = do
    putStr "simple: \t"
    s <- newStringInt
    a <- lookup "haha" s
    alter "haha" 42 s
    b <- lookup "haha" s
    check [a == Nothing, b == Just 42]

testDelete = do
    putStr "delete: \t"
    s <- newStringInt
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
    s <- newStringInt
    alter "haha" 1234 s
    alter "dois" 1234 s
    alter "haha" 42 s
    alter "oi" 42 s
    a <- lookup "haha" s
    check [a == Just 42]

testMember = do
    putStr "member: \t"
    s <- newStringInt
    a <- member "haha" s
    alter "haha" 42 s
    b <- member "ahoy" s
    c <- member "haha" s
    delete "hahaha" s -- doesn't exist
    d <- member "haha" s
    check [not a, not b, c, d]

testElems = do
    putStr "elems:  \t"
    s <- newStringInt
    a <- JM.elems s
    alter "haha" 42 s
    alter "ahoy" 1 s
    alter "nop" 2 s
    b <- JM.elems s
    check [a == [], (sort b) == [1,2,42]]

testKeys = do
    putStr "keys:   \t"
    s <- newStringInt
    a <- JM.keys s
    alter "haha" 42 s
    alter "ahoy" 1 s
    alter "nop" 2 s
    b <- JM.keys s
    delete "ahoy" s
    delete "nada" s
    c <- JM.keys s
    check [a == [], (sort b) == ["ahoy", "haha", "nop"], (sort c) == ["haha", "nop"]]

instance Stringable Int where
    toString = show
    fromString = read


testIntKey = do
    putStr "int-key map: \t"
    s <- newIntString
    a <- JM.keys s
    alter 22 "string" s
    alter 59 "i am not a number" s
    b <- JM.keys s
    c <- JM.elems s
    d <- member 22 s
    delete 22 s
    e <- member 22 s
    f <- lookup 59 s
    check [a == [], (sort b) == [22,59],
           (sort c) == ["i am not a number", "string"],
           d, not e, f == Just "i am not a number"]

testLotsOfMem = do
    putStrLn $ "# " ++ (show $ last $ take 100000 [1..])
    putStr "lots of mem:  \t"
    s <- newStringInt
    a <- JM.elems s
    alter "haha" 42 s
    alter "ahoy" 1 s
    alter "nop" 2 s
    b <- JM.elems s
    check [a == [], (sort b) == [1,2,42]]

testFrozenMap = do
    putStr "frozen map: \t"
    let m = fromListF [(1,2),(2,3),(3,4)] :: Frozen (JM.Map Int Int)
    let a = memberF 1 m
    let b = memberF 2 m
    let c = memberF 42 m
    let d = lookupF 1 m
    let e = lookupF 42 m
    check [a, b, not c, d == Just 2, e == Nothing] 

testSwapMaps = do
    putStr "swap maps: \t"
    m1 <- fromList [(1,2),(2,3),(4,7)] :: IO (JM.Map Int Int)
    m2 <- fromList [(1,42),(2,42),(3,42)] :: IO (JM.Map Int Int)
    a <- lookup 2 m1
    b <- lookup 2 m2
    JM.swapMaps m1 m2
    c <- lookup 2 m1
    d <- lookup 2 m2
    e <- lookup 3 m2
    check [a == Just 3, b == Just 42, c == Just 42, d == Just 3, e == Nothing]

testAlter2 = do
    putStr "alter2: \t"
    m <- fromList [(1,2), (2,3), (4,5)] :: IO (JM.Map Int Int)
    a <- lookup 1 m
    JM.alter2 (const (Just 42)) 3 m
    b <- lookup 1 m
    c <- lookup 3 m
    JM.alter2 (const (Just 42)) 2 m
    d <- lookup 2 m
    JM.alter2 (const Nothing) 1 m
    e <- lookup 1 m
    check [a == Just 2, b == Just 2, c == Just 42, d == Just 42, e == Nothing]

