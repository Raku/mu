import qualified Judy.BitSet2 as BS
import Foreign.StablePtr
import Judy.Private (Value)

main = do
    putStrLn "# BitSet tests:"
    testSetList
--    sequence [testSimple, testGet, testGet2, testSwapBitSets, testSetList, testSetList']

check l = do
    if and l
        then putStrLn "ok"
        else putStrLn "bad"

testSimple = do
    putStr "simple: \t"
    s <- newIntSet
    BS.set s 3 True
    BS.set s 10 True
    BS.set s 30 True
    xs <- BS.toListIO s
    BS.clear s
    ys <- BS.toListIO s
    check [xs == [3,10,30], ys == []]


data Useless a = A a | B a a deriving (Show, Eq)

instance BS.Hashable a => BS.Hashable (Useless a) where
    hash _ = return 1

testGet2 = do
    putStr "testGet2: \t"
    let x = A 1
        y = B 42 59
    s <- BS.new :: IO (BS.BitSet (Useless Integer))
    BS.set s x True
    a <- BS.get s x
    b <- BS.get s (A 2)
    BS.set s y True
    c <- BS.get s y
    d <- BS.get s (B 42 59)
    check [a, not b, c, d]


newIntSet :: IO (BS.BitSet Int)
newIntSet = BS.new

testGet = do
    putStr "testGet: \t"
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


testSwapBitSets = do
    putStr "swapBitSets: \t"
    s1 <- newIntSet
    s2 <- newIntSet
    BS.set s1 1 True
    BS.set s2 2 True
    BS.swapBitSets s1 s2
    xs <- BS.toListIO s1
    ys <- BS.toListIO s2
    check [xs == [2], ys == [1]]


-- FIXME: GHC actually creates different StablePtrs for same data sometimes.
testSetList = do
    putStr "setList: \t"
    s <- newIntSet
    BS.setList [1..10] True s
    j <- newStablePtr 10
    j1 <- newStablePtr 10
    a <- BS.get s 10
    xs <- BS.toListIOp s
    BS.set s 10 True
    ys <- BS.toListIOp s
    BS.setList [2..10] False s
    b <- BS.get s 10
    c <- BS.get s 1
    putStr $ (show a) ++ "," ++ (show b) ++ "," ++ (show c) ++ " => "
    check [a, not b, c]
    putStrLn $ "\t" ++ (show xs)
    putStrLn $ "\t" ++ (show ys)
    putStrLn $ "\t" ++ (show (j == j1))


testSetList' = do
    putStr "setList': \t"
    s <- newIntSet
    BS.setList [1,2,3,4,5,6,7,8,9,10] True s
    a <- BS.get s 10
    BS.setList [2,3,4,5,6,7,8,9,10] False s
    b <- BS.get s 10
    c <- BS.get s 1
    putStr $ (show a) ++ "," ++ (show b) ++ "," ++ (show c) ++ " => "
    check [a, not b, c]
