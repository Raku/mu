import Data.Typeable
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import System.IO.Unsafe

import Judy.Private

main = do
    putStrLn "# JudyHS tests:"
    sequence [testSimple, testUpdate, testDelete, testList]

check l = do
    if and l
        then putStrLn "ok"
        else putStrLn "bad"


-- primitives
new :: IO (ForeignPtr JudyHS)
new = do
    fp <- mallocForeignPtr
    addForeignPtrFinalizer judyHS_free_ptr fp
    withForeignPtr fp $ flip poke nullPtr
    return fp

newIter :: IO (ForeignPtr JudyHSIter)
newIter = do
    fp <- mallocForeignPtr
    addForeignPtrFinalizer judyHSIter_free_ptr fp
    withForeignPtr fp $ flip poke nullPtr
    return fp

ins j str v = withForeignPtr j $ \j -> do
    withCAStringLen str $ \(cp, len) -> do
        -- TODO: maybe there's a better way to convert Int -> Value?
        r <- judyHSIns j cp (fromIntegral len) judyError
        if r == pjerr
            then error "not enough memory =("
            else poke r v >> return () 

get j str = do
    jj <- withForeignPtr j peek
    withCAStringLen str $ \(cp, len) -> do
        r <- judyHSGet jj cp (fromIntegral len)
        if r == nullPtr
            then return Nothing
            else peek r >>= return . Just

del j str = withForeignPtr j $ \j -> do
    withCAStringLen str $ \(cp, len) -> do
        r <- judyHSDel j cp (fromIntegral len) judyError
        return $ r /= 0

toListIO j = do
    jj <- withForeignPtr j peek
    i <- newIter
    withForeignPtr i $ \ii -> alloca $ \cp -> alloca $ \len -> do
        let f act xs = do
                r <- act jj ii cp len judyError
                if r == nullPtr
                    then return xs
                    else do
                        l <- peek len
                        c <- peek cp
                        v <- peekCAStringLen (c, fromIntegral l)
                        d <- peek r
                        f judyHSIterNext ((v,d):xs)
        f judyHSIterFirst []

-- TODO: test swapping between two iterators.

mixList j1 j2 = do
    j1' <- withForeignPtr j1 peek
    j2' <- withForeignPtr j2 peek
    i1 <- newIter
    i2 <- newIter
    withForeignPtr i1 $ \i1' -> withForeignPtr i2 $ \i2' -> 
     alloca $ \cp -> alloca $ \len -> do
        let f (jj,ii) (jj',ii') act nact xs = do
               r <- act jj ii cp len judyError
               if r == nullPtr
                   then f (jj',ii') (jj,ii) nact judyHSIterNext xs
                   else do
                       l <- peek len
                       c <- peek cp
                       v <- peekCAStringLen (c, fromIntegral l)
                       d <- peek r
                       f (jj',ii') (jj,ii) nact judyHSIterNext ((v,d):xs)
        f (j1',i1') (j2',i2') judyHSIterFirst judyHSIterFirst []


-- tests

testSimple = do
    putStr "simple: \t"
    j <- new
    ins j "test" 1
    a <- get j "test"
    ins j "www" 42
    b <- get j "www"
    c <- get j "haha"
    check [a == Just 1, b == Just 42, c == Nothing]

testUpdate = do
    putStr "update: \t"
    j <- new
    ins j "test" 1
    a <- get j "test"
    ins j "test" 42
    b <- get j "test"
    ins j "test" 1
    c <- get j "test"
    check [a == Just 1, b == Just 42, c == Just 1]

testDelete = do
    putStr "delete: \t"
    j <- new
    a <- del j "haha haha haha"
    ins j "test" 42
    b <- del j "test"
    c <- del j "test"
    check [not a, b, not c]

testList = do
    putStr "list:   \t"
    j <- new
    a <- toListIO j
    ins j "test" 42
    ins j "hahaha" 42
    ins j "funk" 42
    b <- toListIO j
    c <- del j "hahaha"
    d <- toListIO j
    check [a == [], b == [("test",42),("hahaha",42),("funk",42)],
           c, d == [("test",42),("funk",42)]]



