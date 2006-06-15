import qualified Judy.Map as JMap

import Data.List (sort)

main = do
    putStrLn "# Map tests:"
    
    -- FIXME: A better way to do this must exist...
    sequence [testSimple, testDelete, testMember, testElems, testKeys, testIntKey]

check l = do
    if and l
        then putStrLn "ok"
        else putStrLn "BAD"

testSimple = do
    putStr "simple: \t"
    s <- JMap.new
    a <- JMap.lookup "haha" s
    JMap.insert "haha" 42 s
    b <- JMap.lookup "haha" s
    check [a == Nothing, b == Just 42]

testDelete = do
    putStr "delete: \t"
    s <- JMap.new
    a <- JMap.lookup "haha" s
    JMap.insert "haha" 42 s
    b <- JMap.lookup "haha" s
    JMap.insert "ahoy" 59 s
    JMap.delete "haha" s
    c <- JMap.lookup "haha" s
    d <- JMap.lookup "ahoy" s
    check [a == Nothing, b == Just 42, c == Nothing, d == Just 59]

testMember = do
    putStr "member: \t"
    s <- JMap.new
    a <- JMap.member "haha" s
    JMap.insert "haha" 42 s
    b <- JMap.member "ahoy" s
    c <- JMap.member "haha" s
    JMap.delete "hahaha" s -- doesn't exist
    d <- JMap.member "haha" s
    check [not a, not b, c, d]

testElems = do
    putStr "elems:  \t"
    s <- JMap.new
    a <- JMap.elems s
    JMap.insert "haha" 42 s
    JMap.insert "ahoy" 1 s
    JMap.insert "nop" 2 s
    b <- JMap.elems s
    check [a == [], (sort b) == [1,2,42]]

testKeys = do
    putStr "keys:   \t"
    s <- JMap.new
    a <- JMap.keys s
    JMap.insert "haha" 42 s
    JMap.insert "ahoy" 1 s
    JMap.insert "nop" 2 s
    b <- JMap.keys s
    JMap.delete "ahoy" s
    JMap.delete "nada" s
    c <- JMap.keys s
    check [a == [], (sort b) == ["ahoy", "haha", "nop"], (sort c) == ["haha", "nop"]]

instance JMap.Stringable Int where
    toString = show
    fromString = read

testIntKey = do
    putStr "int-key map: \t"
    s <- JMap.new :: IO (JMap.Map Int String)
    a <- JMap.keys s
    JMap.insert 22 "string" s
    JMap.insert 59 "i am not a number" s
    b <- JMap.keys s
    c <- JMap.elems s
    d <- JMap.member 22 s
    JMap.delete 22 s
    e <- JMap.member 22 s
    f <- JMap.lookup 59 s
    check [a == [], (sort b) == [22,59],
           (sort c) == ["i am not a number", "string"],
           d, not e, f == Just "i am not a number"]
