{-# OPTIONS -fallow-undecidable-instances -fallow-incoherent-instances #-}

import Test

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


main = no_plan $ do
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
    --sequence $ take 5000 $ repeat testAlter

newStringInt = new :: IO (M String Int)
newIntString = new :: IO (M Int String)
type IntIntMap = M Int Int

testSimple = do
    say "Simple"
    s <- newStringInt
    lookup "haha" s    .=> Nothing
    insert "haha" 42 s
    lookup "haha" s    .=> Just 42

testDelete = do
    say "Delete"
    s <- newStringInt
    lookup "haha" s    .=> Nothing
    insert "haha" 42 s
    lookup "haha" s    .=> Just 42
    insert "ahoy" 59 s
    delete "haha" s
    lookup "haha" s    .=> Nothing
    lookup "ahoy" s    .=> Just 59

testOverwrite = do
    say "Overwrite"
    s <- newStringInt
    insert "haha" 1234 s
    insert "dois" 1234 s
    insert "haha" 42 s
    insert "oi" 42 s
    lookup "haha" s .=> Just 42

testMember = do
    say "Member"
    s <- newStringInt
    member "haha" s    .=> False
    insert "haha" 42 s
    member "ahoy" s    .=> False
    member "haha" s    .=> True
    delete "hahaha" s  --- doesn't exist
    member "haha" s    .=> True

testElems = do
    say "Elems"
    s <- newStringInt
    elems s .-= []
    insert "haha" 42 s
    insert "ahoy" 1 s
    insert "nop" 2 s
    elems s .-= [1,2,42]

testKeys = do
    say "Keys"
    s <- newStringInt
    keys s .-= []
    insert "haha" 42 s
    insert "ahoy" 1 s
    insert "nop" 2 s
    keys s .-= ["ahoy", "haha", "nop"]
    delete "ahoy" s
    delete "nada" s
    keys s .-= ["haha", "nop"]

instance Stringable Int where
    toString = show
    fromString = read

testIntKey = do
    say "IntKey"
    s <- newIntString
    keys s .-= []

    insert 22 "string" s
    insert 59 "i am not a number" s
    keys s      .-= [22, 59]
    elems s     .-= ["i am not a number", "string"]
    member 22 s .=> True

    delete 22 s
    member 22 s .=> False
    lookup 59 s .=> Just "i am not a number"

testIntKeyDelete = do
    say "IntKeyDelete"
    s <- newIntString
    insert 22 "string" s
    insert 23 "string" s
    insert 24 "ahha" s
    insert 25 "oieee" s
    delete 22 s
    lookup 23 s .=> Just "string"

testLotsOfMem = do
    say "LotsOfMem"
    say $ "your lucky number is " ++ (show $ last $ take 100000 [1..])
    s <- newStringInt
    elems s .-= []
    insert "haha" 42 s
    insert "ahoy" 1 s
    insert "nop" 2 s
    elems s .-= [1, 2, 42]

testFrozenMap = do
    say "FrozenMap"
    let m = fromListF [(1,2),(2,3),(3,4)] :: Frozen IntIntMap
    memberF 1 m  ==> True
    memberF 2 m  ==> True
    memberF 42 m ==> False
    lookupF 1 m  ==> Just 2
    lookupF 42 m ==> Nothing

testSwapMaps = do
    say "SwapMaps"
    m1 <- fromList [(1,2),(2,3),(4,7)] :: IO IntIntMap
    m2 <- fromList [(1,42),(2,42),(3,42)] :: IO IntIntMap
    lookup 2 m1 .=> Just 3
    lookup 2 m2 .=> Just 42

    swapMaps m1 m2
    lookup 2 m1 .=> Just 42
    lookup 2 m2 .=> Just 3
    lookup 3 m2 .=> Nothing

testAlter = do
    say "Alter"
    m <- fromList [(1,2), (2,3), (4,5)] :: IO IntIntMap
    lookup 1 m .=> Just 2
    alter (const (Just 42)) 3 m
    lookup 1 m .=> Just 2
    lookup 3 m .=> Just 42
    alter (const (Just 42)) 2 m
    lookup 2 m .=> Just 42
    alter (const Nothing) 1 m
    lookup 1 m .=> Nothing
