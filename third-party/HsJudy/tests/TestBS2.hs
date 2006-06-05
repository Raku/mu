import qualified Judy.BitSet2 as BS
import Foreign.StablePtr
import Judy.Private (Value)

main = do
    putStrLn "# BitSet tests:"
    -- FIXME: A better way to do this must exist...
    sequence [testIntSimple, testIntGet, testIntSwapBitSets, testIntSetList]
    sequence [testUselessSimple, testUselessGet, testUselessSwapBitSets, testUselessSetList]

check l = do
    if and l
        then putStrLn "ok"
        else putStrLn "bad"


{- Tests for Int type -}
newIntSet :: IO (BS.BitSet Int)
newIntSet = BS.new

testIntSimple = do
    putStr "int simple: \t"
    s <- newIntSet
    BS.set s 3 True
    BS.set s 10 True
    BS.set s 30 True
    xs <- BS.toList s
    BS.clear s
    ys <- BS.toList s
    check [xs == [3,10,30], ys == []]

testIntGet = do
    putStr "int get: \t"
    s <- newIntSet
    a <- BS.get s 2
    BS.set s 1 True
    b <- BS.get s 1
    BS.set s 2 True
    c <- BS.get s 2
    BS.set s 1 False
    d <- BS.get s 1
    BS.clear s
    e <- BS.get s 1
    check [not a, b, c, not d, not e]

testIntSwapBitSets = do
    putStr "int swapBS: \t"
    s1 <- newIntSet
    s2 <- newIntSet
    BS.set s1 1 True
    BS.set s2 2 True
    BS.swapBitSets s1 s2
    xs <- BS.toList s1
    ys <- BS.toList s2
    check [xs == [2], ys == [1]]

testIntSetList = do
    putStr "int setList: \t"
    s <- newIntSet
    BS.setList [1..10] True s
    a <- BS.get s 10
    BS.setList [2..10] False s
    b <- BS.get s 10
    c <- BS.get s 1
    check [a, not b, c]


{- Tests for Useless type -- an enumerable type -}
newUselessSet :: IO (BS.BitSet Useless)
newUselessSet = BS.new

data Useless = A | B | C | D deriving (Show, Eq, Enum)

-- FIXME: Is this really necessary? Useless already derives Enum
instance BS.HashIO Useless
instance BS.UniqueHashIO Useless
instance BS.ReversibleHashIO Useless

testUselessSimple = do
    putStr "ul simple: \t"
    s <- newUselessSet
    BS.set s A True
    BS.set s B True
    xs <- BS.toList s
    BS.clear s
    ys <- BS.toList s
    BS.set s D True
    BS.set s C True
    BS.set s D False
    zs <- BS.toList s
    check [xs == [A,B], ys == [], zs == [C]]

testUselessGet = do
    putStr "ul get: \t"
    s <- newUselessSet
    a <- BS.get s C
    BS.set s D True
    b <- BS.get s D
    BS.set s A True
    c <- BS.get s A
    BS.set s A False
    d <- BS.get s A
    BS.clear s
    e <- BS.get s D
    check [not a, b, c, not d, not e]

testUselessSwapBitSets = do
    putStr "ul swapBS: \t"
    s1 <- newUselessSet
    s2 <- newUselessSet
    BS.set s1 A True
    BS.set s2 B True
    BS.set s2 C True
    BS.swapBitSets s1 s2
    xs <- BS.toList s1
    ys <- BS.toList s2
    check [xs == [B,C], ys == [A]]

testUselessSetList = do
    putStr "ul setList: \t"
    s <- newUselessSet
    BS.setList [A,B,C,D] True s
    a <- BS.get s D
    BS.setList [B,C,D] False s
    b <- BS.get s D
    c <- BS.get s A
    check [a, not b, c]
