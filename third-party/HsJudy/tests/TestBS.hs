import qualified Judy.BitSet as BS

main = do
    putStrLn "# BitSet tests:"
    sequence [testSimple, testGet, testSwapBitSets, testSetList, testCount]

check l = do
    if and l
        then putStrLn "ok"
        else putStrLn "bad"

testSimple = do
    putStr "simple: \t"
    s <- BS.new
    BS.set s 3 True
    BS.set s 10 True
    BS.set s 30 True
    xs <- BS.toListIO s
    BS.clear s
    ys <- BS.toListIO s
    check [xs == [3,10,30], ys == []]

testGet = do
    putStr "testGet: \t"
    s <- BS.new
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
    s1 <- BS.new
    s2 <- BS.new
    BS.set s1 1 True
    BS.set s2 2 True
    BS.swapBitSets s1 s2
    xs <- BS.toListIO s1
    ys <- BS.toListIO s2
    check [xs == [2], ys == [1]]

testSetList = do
    putStr "setList: \t"
    s <- BS.new
    BS.setList [1..10] True s
    a <- BS.get s 10
    BS.setList [2..10] False s
    b <- BS.get s 10
    c <- BS.get s 1
    check [a, not b, c]

testCount = do
    putStr "testCount: \t"
    s <- BS.new
    BS.setList [1..200] True s
    a <- BS.count s 1 200
    b <- BS.count s 200 1000
    c <- BS.count s 1 1
    BS.clear s
    d <- BS.count s 1 200
    check [a == 200, b == 1, c == 1, d == 0]

