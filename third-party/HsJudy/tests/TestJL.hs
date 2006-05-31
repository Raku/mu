
import Data.Typeable
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr
import System.IO.Unsafe

import Judy.Private


main = do
    putStrLn "# JudyL tests:"
    sequence [testSimple, testUpdate, testDelete, testCount, testList]

check l = do
    if and l
        then putStrLn "ok"
        else putStrLn "bad"


-- primitives
new :: IO (ForeignPtr JudyL)
new = do
    fp <- mallocForeignPtr
    addForeignPtrFinalizer judyL_free_ptr fp
    withForeignPtr fp $ flip poke nullPtr
    return $ fp

ins j wp v = withForeignPtr j $ \j -> do
    r <- judyLIns j wp judyError
    if r == pjerr
        then error "not enough memory"
        else poke r v >> return ()

get j wp = do
    jj <- withForeignPtr j peek
    r <- judyLGet jj wp judyError
    if r == nullPtr
        then return Nothing
        else peek r >>= return . Just

del j wp = withForeignPtr j $ \j -> do
    r <- judyLDel j wp judyError
    return $ r /= 0

count j i1 i2 = withForeignPtr j $ \j -> do
    jj <- peek j
    r <- judyLCount jj i1 i2 judyError
    return $ r

toListIO j = do
    jj <- withForeignPtr j peek
    alloca $ \wp -> do
        poke wp (-1)
        let f _ xs = do
                 v <- peek wp
                 r <- judyLPrev jj wp judyError
                 if r == nullPtr
                     then return xs
                     else do 
                         v <- peek wp
                         d <- peek r
                         f wp ((v, d):xs)
        r <- judyLLast jj wp judyError
        if r == nullPtr
            then return []
            else do
                v <- peek wp
                d <- peek r
                f wp [(v, d)]

-- tests

testSimple = do
    putStr "simple: \t"
    j <- new
    ins j 1 1
    a <- get j 1
    ins j 1 42
    b <- get j 2
    c <- get j 1
    check [a == Just 1, b == Nothing, c == Just 42]

testUpdate = do
    putStr "update: \t"
    j <- new
    ins j 1 1
    a <- get j 1
    ins j 1 42
    b <- get j 1
    ins j 1 1
    c <- get j 1
    check [a == Just 1, b == Just 42, c == Just 1]

testDelete = do
    putStr "delete: \t"
    j <- new
    a <- del j 1
    ins j 1 42
    b <- del j 1
    c <- del j 1
    check [not a, b, not c]

testCount = do
    putStr "count:  \t"
    j <- new
    a <- count j 1 10
    ins j 1 42
    b <- count j 1 10
    ins j 2 42
    ins j 3 42
    c <- count j 1 10
    d <- del j 2
    e <- count j 1 10
    f <- count j 3 10
    check [a == 0, b == 1, c == 3, d, e == 2, f == 1]

testList = do
    putStr "list:   \t"
    j <- new
    a <- toListIO j
    ins j 1 42
    ins j 2 42
    ins j 3 42
    b <- toListIO j
    c <- del j 2
    d <- toListIO j
    check [a == [], b == [(1,42),(2,42),(3,42)],
           c, d == [(1,42),(3,42)]]
