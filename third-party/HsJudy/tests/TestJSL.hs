import Test

import Data.Typeable
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr
import Foreign.C.String
import System.IO.Unsafe

import Judy.Private

main = no_plan $ do
    --testOutOfMemory
    testSimple
    testUpdate
    testDelete
    testNonASCII
    testList

{- Primitives for working with JudySL -}
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
        if r == pjerr
            then error "not enough memory =("
            else poke r v >> return () 

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
        let f act xs = do
                r <- act jj cstr judyError
                if r == nullPtr
                    then return xs
                    else do
                        v <- peekCAString cstr
                        d <- peek r
                        f judySLNext ((v,d):xs)
        f judySLFirst []


{- Tests -}

testSimple = do
    say "Simple"
    j <- new
    ins j "test" 1
    get j "test"   .=> Just 1
    ins j "www" 42
    get j "www"    .=> Just 42
    get j "haha"   .=> Nothing

testUpdate = do
    say "Update"
    j <- new
    ins j "test" 1
    get j "test"     .=> Just 1
    ins j "test" 42
    get j "test"     .=> Just 42
    ins j "test" 1
    get j "test"     .=> Just 1

testDelete = do
    say "Delete"
    j <- new
    del j "haha haha haha" .=> False
    ins j "test" 42
    del j "test"           .=> True
    del j "test"           .=> False

testNonASCII = do
    say "NonASCII"
    j <- new
    ins j "josé" 42
    get j "josé"    .=> Just 42
    toListIO j      .-= [("josé", 42)]
    del j "josé"    .=> True
    get j "josé"    .=> Nothing

testList = do
    say "List"
    j <- new
    toListIO j .-= []

    ins j "test" 42
    ins j "haha" 42
    ins j "funk" 42
    toListIO j .-= [("test",42),("haha",42),("funk",42)]

    del j "haha" .=> True
    toListIO j .-= [("test",42), ("funk",42)]


{-testOutOfMemory = do
    say "OutOfMemory"
    j <- new
    sequence_ $ map (\x -> ins j ("haha ahhaahahhahahahahhahahahahahhahahahahahha" ++ show x) x) [1..100000]
    l <- toListIO j
    putStrLn $ show l -}
