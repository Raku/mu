module Judy.BitSet2 where

import Data.Typeable
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr
import Foreign.StablePtr
import System.IO.Unsafe

import Judy.Private
import Judy.Freeze


newtype BitSet a = BitSet (ForeignPtr Judy1)
    deriving (Eq, Ord, Typeable)

instance Show (BitSet a) where
    show (BitSet j) = "<BitSet " ++ show j ++ ">"

-- | O(1) - swap contents of two bitsets
swapBitSets :: BitSet a -> BitSet a -> IO ()
swapBitSets (BitSet bs1) (BitSet bs2) = do
    withForeignPtr bs1 $ \p1 ->  do
        withForeignPtr bs2 $ \p2 ->  do
            v1 <- peek p1
            v2 <- peek p2
            poke p1 v2
            poke p2 v1

-- | create a bitset
new :: IO (BitSet a)
new = do
    fp <- mallocForeignPtr
    addForeignPtrFinalizer judy1_free_ptr fp
    withForeignPtr fp $ flip poke nullPtr
    return $ BitSet fp

-- | set a bit and return its old state
set :: BitSet a -> a -> Bool -> IO Bool
set (BitSet j) v True = withForeignPtr j $ \j ->  do
    vp <- newStablePtr v
    r <- judy1Set j (ptrToWordPtr (castStablePtrToPtr vp)) judyError
    return $ r == 0
set (BitSet j) v False = withForeignPtr j $ \j -> do
    vp <- newStablePtr v
    r <- judy1Unset j (ptrToWordPtr (castStablePtrToPtr vp)) judyError
    return $ r /= 0


-- this inline was in Meacham original BitSet
-- {-# INLINE get #-}
get :: BitSet a -> a -> IO Bool
get (BitSet j) v = do
    jj <- withForeignPtr j peek
    vp <- newStablePtr v
    r <- judy1Test jj (ptrToWordPtr (castStablePtrToPtr vp)) judyError
    return $ r /= 0

clear :: BitSet a -> IO ()
clear (BitSet j) = withForeignPtr j $ \j -> judy1FreeArray j judyError >> return ()


toListIO :: BitSet a -> IO [a]
toListIO (BitSet j) = do
    jj <- withForeignPtr j peek
    alloca $ \vp -> do
        poke vp (-1)
        let f 0 xs = return xs
            f _ xs = do
                v <- peek vp
                v' <- deRefStablePtr $ castPtrToStablePtr $ wordPtrToPtr v
                r <- judy1Prev jj vp judyError
                f r (v':xs)
        r <- judy1Last jj vp judyError
        f r []

toListIOp :: BitSet a -> IO [Value]
toListIOp (BitSet j) = do
     jj <- withForeignPtr j peek
     alloca $ \vp -> do
         poke vp (-1)
         let f 0 xs = return xs
             f _ xs = do
                 v <- peek vp
                 r <- judy1Prev jj vp judyError
                 f r (v:xs)
         r <- judy1Last jj vp judyError
         f r []




{-setList :: [a] -> Bool -> BitSet a ->  IO ()
setList vs True (BitSet bs) = withForeignPtr bs $ \j -> mapM_ (\v -> do
                                                                 vp <- newStablePtr v
                                                                 judy1Set j (ptrToWordPtr (castStablePtrToPtr vp)) judyError
                                                              ) vs
setList vs False (BitSet bs) = withForeignPtr bs $ \j -> mapM_ (\v -> do
                                                                  vp <- newStablePtr v
                                                                  judy1Unset j (ptrToWordPtr (castStablePtrToPtr vp)) judyError
                                                               ) vs

-}

setList :: [a] -> Bool -> BitSet a -> IO ()
setList vs t j = mapM_ (\x -> set j x t) vs


{-
fromListIO :: [Value] -> IO BitSet
fromListIO ws = do
    bs <- new
    setList ws True bs
    return bs

-- Pure access routines

-- | create a frozen, immutable version of a bitset, the original mutable version is cleared.
freezeBitSet :: BitSet -> IO (Frozen BitSet)
freezeBitSet bs = do
    nbs <- new
    swapBitSets bs nbs
    return (Frozen nbs)

member :: Value -> Frozen BitSet -> Bool
member wp (Frozen bs) = unsafePerformIO $ get bs wp

fromList :: [Value] -> Frozen BitSet
fromList ws = Frozen $ unsafePerformIO $ do
    bs <- new
    setList ws True bs
    return bs

toList :: Frozen BitSet -> [Value]
toList = toListFrom 0


toListFrom :: Value -> Frozen BitSet -> [Value]
toListFrom iwp (Frozen (BitSet bs)) = unsafePerformIO $ do
        jj <- withForeignPtr bs peek
        (r,v) <- alloca $ \wp -> do
            poke wp iwp
            r <- judy1First jj wp judyError
            v <- peek wp
            return (r,v)
        let f 0 _ = []
            f _ v = v:unsafePerformIO (g v)
            g v = do
                (r,v) <- alloca $ \wp -> do
                    poke wp v
                    r <- judy1Next jj wp judyError
                    v <- peek wp
                    touchForeignPtr bs
                    return (r,v)
                return (f r v)
        return (f r v)


toRevList :: Frozen BitSet -> [Value]
toRevList = toRevListFrom (-1)

toRevListFrom :: Value -> Frozen BitSet -> [Value]
toRevListFrom iwp (Frozen (BitSet bs)) = unsafePerformIO $ do
    withForeignPtr bs $ \j -> do
        jj <- peek j
        (r,v) <- alloca $ \wp -> do
            poke wp iwp
            r <- judy1Last jj wp judyError
            v <- peek wp
            return (r,v)
        let f 0 _ = []
            f _ v = v:unsafePerformIO (g v)
            g v = do
                (r,v) <- alloca $ \wp -> do
                    poke wp v
                    r <- judy1Prev jj wp judyError
                    v <- peek wp
                    touchForeignPtr bs
                    return (r,v)
                return (f r v)
        return (f r v)


instance Freezable BitSet where
    freeze = freezeBitSet


-}
