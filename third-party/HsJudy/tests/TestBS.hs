import qualified Judy.BitSet as BS
import Judy.HashIO
import Judy.Freeze

main = do
    putStrLn "# BitSet tests:"
    -- FIXME: A better way to do this must exist...
    sequence [testIntSimple, testIntGet, testIntSwapBitSets, testIntSetList,
              testIntNullMember, testIntSize, testIntInsertDelete, testIntFreeze,
              testIntFromListF, testIntToListF]
    sequence [testUselessSimple, testUselessGet, testUselessSwapBitSets, testUselessSetList]

check l = do
    if and l
        then putStrLn "ok"
        else putStrLn "BAD"


-- Tests for Int type. Uses default implementation for Enum types.
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
    putStr "int fromList: \t"
    s <- newIntSet
    BS.fromList [1..10] s
    a <- BS.get s 10
    BS.set s 10 False
    b <- BS.get s 10
    c <- BS.get s 1
    check [a, not b, c]

testIntNullMember = do
    putStr "int nullmember:\t"
    s <- newIntSet
    a <- BS.null s
    b <- BS.member 3 s
    BS.fromList [1..10] s
    c <- BS.member 3 s
    d <- BS.null s
    check [a, not b, c, not d]

testIntSize = do
    putStr "int size:  \t"
    s <- newIntSet
    a <- BS.size s
    BS.fromList [1..10] s
    b <- BS.size s
    BS.clear s
    c <- BS.size s
    BS.set s 3 True
    BS.set s 2 True
    d <- BS.size s
    check [a == 0, b == 10, c == 0, d == 2]

testIntInsertDelete = do
    putStr "int ins/del: \t"
    s <- newIntSet
    BS.insert 1 s
    a <- BS.toList s
    BS.delete 1 s
    b <- BS.toList s
    BS.insert 1 s 
    BS.insert 2 s
    BS.insert 42 s
    c <- BS.toList s
    BS.insert 1 s
    d <- BS.toList s
    BS.delete 1 s
    BS.delete 2 s
    e <- BS.toList s
    BS.delete 1 s
    f <- BS.toList s
    check [a == [1], b == [], c == [1,2,42],
           d == c, e == [42], f == [42]]

testIntFreeze = do
    putStr "int freeze: \t"
    s <- newIntSet
    BS.insert 1 s
    BS.insert 2 s
    ice <- freeze s
    let a = BS.memberF 1 ice
        b = BS.memberF 2 ice
        c = BS.memberF 42 ice
    check [a, b, not c]

testIntFromListF = do
    putStr "int fromF: \t"
    let ice = BS.fromListF [42, 59] :: Frozen (BS.BitSet Int)
        a = BS.memberF 42 ice
        b = BS.memberF 1 ice
        c = BS.memberF 59 ice
        d = BS.memberF 2 ice
    check [a, not b, c, not d]

testIntToListF = do
    putStr "int toF: \t"
    s <- newIntSet
    BS.insert 1 s
    BS.insert 59 s
    ice <- freeze s
    let xs = BS.toListF ice
        a = BS.memberF 1 ice
        b = BS.memberF 2 ice
    check [xs == [1,59], a, not b]
    


-- Tests for Useless type.
newUselessSet :: IO (BS.BitSet Useless)
newUselessSet = BS.new

data Useless = A | B | C | D deriving (Show, Eq, Enum)

-- Enum types can provide own implementation if needed

instance HashIO Useless where
    hashIO A = return 10
    hashIO B = return 20
    hashIO C = return 30
    hashIO D = return 40
instance UniqueHashIO Useless where
instance ReversibleHashIO Useless where
    unHashIO 10 = return A
    unHashIO 20 = return B
    unHashIO 30 = return C 
    unHashIO 40 = return D


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
    putStr "ul fromList: \t"
    s <- newUselessSet
    BS.fromList [A,B,C,D] s
    a <- BS.get s D
    BS.set s D False
    b <- BS.get s D
    c <- BS.get s A
    check [a, not b, c]
