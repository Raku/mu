import Data.Typeable
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr
import Foreign.C.String
import System.IO.Unsafe

import Judy.Private


-- TODO: check whether shoudl use CString instead of CAString
-- CAString is being used now because apparently Judy is ignorant of Unicode

main = do
    putStrLn "# JudySL tests:"
    sequence [testSimple, testUpdate, testDelete, testNonASCII, testList]

check l = do
    if and l
        then putStrLn "ok"
        else putStrLn "bad"


-- primitives
new :: IO (ForeignPtr JudySL)
new = do
    fp <- mallocForeignPtr
    addForeignPtrFinalizer judySL_free_ptr fp
    withForeignPtr fp $ flip poke nullPtr
    return $ fp

ins j str v = withForeignPtr j $ \j -> do
    -- TODO: check if this is better than creating CString manually
    withCAString str $ \cstr -> do
        r <- judySLIns j cstr judyError
        poke r v
        return ()
        -- FIXME: check for error, depends on all that PJERR stuff =P

get j str = do
    jj <- withForeignPtr j peek
    withCAString str $ \cstr -> do
        r <- judySLGet jj cstr judyError
        if r == nullPtr
            then return Nothing
            else peek r >>= return . Just

del j str = withForeignPtr j $ \j -> do
    withCAString str $ \cstr -> do
        r <- judySLDel j cstr judyError
        return $ r /= 0

{-teste = do 
    allocaBytes 10 $ \cstr -> do
        j_null cstr
        a <- peekCAString cstr
        putStrLn a
        j_fill cstr (castCharToCChar 'a') 10
        b <- peekCAString cstr
        putStrLn b -}

toListIO j = do
    jj <- withForeignPtr j peek
    -- FIXME: get a better place for this constant
    allocaBytes 1000 $ \cstr -> do
        j_null cstr
        -- FIXME: ugly, unclear
        let f xs = do
                v <- peekCAString cstr
                r <- judySLNext jj cstr judyError
                g r xs
            g r xs =
              if r == nullPtr
                  then return xs
                  else do
                      v <- peekCAString cstr
                      d <- peek r
                      f ((v,d):xs)
        r <- judySLFirst jj cstr judyError
        g r []


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

testNonASCII = do
    putStr "non_ascii: \t"
    j <- new
    ins j "josé" 42
    a <- get j "josé"
    b <- toListIO j
    c <- del j "josé"
    d <- get j "josé"
    check [a == Just 42, b == [("josé", 42)], c, d == Nothing]

testList = do
    putStr "list:   \t"
    j <- new
    a <- toListIO j
    ins j "test" 42
    ins j "haha" 42
    ins j "funk" 42
    b <- toListIO j
    c <- del j "haha"
    d <- toListIO j
    check [a == [], b == [("test",42),("haha",42),("funk",42)],
           c, d == [("test",42),("funk",42)]]
