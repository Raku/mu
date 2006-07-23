import Test

import Data.Typeable
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import System.IO.Unsafe

import Judy.Private

import Data.List (sort)

main = no_plan $ do
    testSimple
    testUpdate
    testDelete
    testList
    testMixList

{- Primitives for working with JudyHS -}
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

mixList :: ForeignPtr JudyHS -> ForeignPtr JudyHS -> IO [(String, Value)]
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
                   then return xs
                   else do
                       l <- peek len
                       c <- peek cp
                       v <- peekCAStringLen (c, fromIntegral l)
                       d <- peek r
                       f (jj',ii') (jj,ii) nact judyHSIterNext ((v,d):xs)
        f (j1',i1') (j2',i2') judyHSIterFirst judyHSIterFirst []


{- Tests -}

testSimple = do
    say "Simple"
    j <- new
    ins j "test" 1
    get j "test" .=> Just 1

    ins j "www" 42
    get j "www"  .=> Just 42
    get j "haha" .=> Nothing

testUpdate = do
    say "Update"
    j <- new
    ins j "test" 1
    get j "test" .=> Just 1
    
    ins j "test" 42
    get j "test" .=> Just 42

    ins j "test" 1
    get j "test" .=> Just 1

testDelete = do
    say "Delete"
    j <- new
    del j "haha haha haha" .=> False
    
    ins j "test" 42
    del j "test" .=> True
    del j "test" .=> False

testList = do
    say "List"
    j <- new
    toListIO j .-= []

    ins j "test" 42
    ins j "hahaha" 42
    ins j "funk" 42
    toListIO j     .-= [("test",42), ("hahaha",42), ("funk",42)]
    del j "hahaha" .=> True
    toListIO j     .-= [("test",42), ("funk",42)]

testMixList = do
    say "MixList"
    j1 <- new
    j2 <- new
    mapM_ (\x -> ins j1 ("A" ++ (show x)) 1111) [1..10]
    mapM_ (\x -> ins j2 ("B" ++ (show x)) 2222) [1..10]
    a <- toListIO j1
    b <- toListIO j2
    c <- mixList j1 j2
    check [c == mix b a] -- The list is filled in inverse order at mixList
    where mix [] _ = []
          mix (x:xs) (y:ys) = x:y:(mix xs ys)
