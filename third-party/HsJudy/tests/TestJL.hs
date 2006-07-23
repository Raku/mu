import Test

import Data.Typeable
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr
import System.IO.Unsafe

import Judy.Private


main = no_plan $ do
    testSimple
    testUpdate
    testDelete
    testCount
    testList

{- Primitives for working with JudyL -}
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

{- Tests -}

testSimple = do
    say "Simple"
    j <- new
    ins j 1 1
    get j 1 .=> Just 1

    ins j 1 42
    get j 2 .=> Nothing
    get j 1 .=> Just 42

testUpdate = do
    say "Update"
    j <- new
    ins j 1 1
    get j 1 .=> Just 1

    ins j 1 42
    get j 1 .=> Just 42
    
    ins j 1 1
    get j 1 .=> Just 1

testDelete = do
    say "Delete"
    j <- new
    del j 1 .=> False

    ins j 1 42
    del j 1 .=> True
    del j 1 .=> False

testCount = do
    say "Count"
    j <- new
    count j 1 10 .=> 0
    
    ins j 1 42
    count j 1 10 .=> 1

    ins j 2 42
    ins j 3 42
    count j 1 10 .=> 3
    del j 2      .=> True
    count j 1 10 .=> 2
    count j 3 10 .=> 1

testList = do
    say "List"
    j <- new
    toListIO j .-= []
    
    ins j 1 42
    ins j 2 42
    ins j 3 42
    toListIO j .-= [(1,42), (2,42), (3,42)]

    del j 2 .=> True
    toListIO j .-= [(1,42),(3,42)]
