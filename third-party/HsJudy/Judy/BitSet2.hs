module Judy.BitSet2 where

import Data.Typeable
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr
import Foreign.StablePtr
import System.IO.Unsafe
import Data.HashTable (hashString)

import Judy.Private
import Judy.Freeze


class Enum a => HashIO a where
    hashIO :: a -> IO Value
    -- Two step conversion, first from a -> Int then Int -> Value
    hashIO = return . toEnum . fromEnum
class HashIO a => UniqueHashIO a
class UniqueHashIO a => ReversibleHashIO a where
    unHashIO :: Value -> IO a
    -- Two step conversion, first from Value -> Int then Int -> a
    unHashIO = return . toEnum . fromEnum


instance HashIO Value where
    hashIO = return
instance UniqueHashIO Value
instance ReversibleHashIO Value where
    unHashIO = return


instance HashIO Int where
    hashIO = return . toEnum
instance UniqueHashIO Int
instance ReversibleHashIO Int where
    unHashIO = return . fromEnum

instance HashIO Integer where
    hashIO = return . fromIntegral . hashString . show


newtype HashIO a => BitSet a = BitSet { judy :: ForeignPtr Judy1 }
    deriving (Eq, Ord, Typeable)

instance Show (BitSet a) where
    show (BitSet bs) = "<BitSet " ++ show bs ++ ">"

-- | Swap contents of two sets.
swapBitSets :: BitSet a -> BitSet a -> IO ()
swapBitSets (BitSet bs1) (BitSet bs2) = do
    withForeignPtr bs1 $ \p1 ->  do
        withForeignPtr bs2 $ \p2 ->  do
            v1 <- peek p1
            v2 <- peek p2
            poke p1 v2
            poke p2 v1

-- | Create a set.
new :: HashIO a => IO (BitSet a)
new = do
    fp <- mallocForeignPtr
    addForeignPtrFinalizer judy1_free_ptr fp
    withForeignPtr fp $ flip poke nullPtr
    return $ BitSet fp

-- | Add a value to the set.
insert :: HashIO a => BitSet a -> a -> IO ()
insert (BitSet bs) v = withForeignPtr bs $ \bs' -> do
    v' <- hashIO v
    judy1Set bs' v' judyError
    return ()

-- | Delete a value in the set.
delete :: HashIO a => BitSet a -> a -> IO ()
delete (BitSet bs) v = withForeignPtr bs $ \bs' -> do
    v' <- hashIO v
    judy1Unset bs' v' judyError
    return ()

-- | Set value in or out the set and return its old value.
set :: HashIO a => BitSet a -> a -> Bool -> IO Bool
set (BitSet j) v True = withForeignPtr j $ \j ->  do
    vp <- hashIO v
    r <- judy1Set j vp judyError
    return $ r == 0
set (BitSet j) v False = withForeignPtr j $ \j -> do
    vp <- hashIO v
    r <- judy1Unset j vp judyError
    return $ r /= 0


-- this inline was in Meacham original BitSet
-- {-# INLINE get #-}
get :: HashIO a => BitSet a -> a -> IO Bool
get (BitSet j) v = do
    jj <- withForeignPtr j peek
    vp <- hashIO v
    r <- judy1Test jj vp judyError
    return $ r /= 0

-- | Is the value a member of the set? 
member :: HashIO a => BitSet a -> a -> IO Bool
member (BitSet bs) v = do
    bs' <- withForeignPtr bs peek
    v' <- hashIO v
    r <- judy1Test bs' v' judyError
    return $ r /= 0

-- | Is the set empty?
null :: BitSet a -> IO Bool
null (BitSet bs) = do
    bs' <- withForeignPtr bs peek
    return $ bs' == nullPtr

-- | Cardinality of the set.
size :: BitSet a -> IO Int
size (BitSet bs) = do
    bs' <- withForeignPtr bs peek
    r <- judy1Count bs' 0 (-1) judyError
    return $ fromEnum r

-- | Make the set empty.
clear :: HashIO a => BitSet a -> IO ()
clear (BitSet j) = withForeignPtr j $ \j -> judy1FreeArray j judyError >> return ()

-- | Convert the set to a list of elements.
toList :: ReversibleHashIO a => BitSet a -> IO [a]
toList (BitSet j) = do
    jj <- withForeignPtr j peek
    alloca $ \vp -> do
        poke vp (-1)
        let f 0 xs = return xs
            f _ xs = do
                v <- peek vp
                v' <- unHashIO v
                r <- judy1Prev jj vp judyError
                f r (v':xs)
        r <- judy1Last jj vp judyError
        f r []

-- | Create a set from a list of elements.
fromList :: HashIO a => [a] -> BitSet a -> IO ()
fromList vs bs = mapM_ (\v -> insert bs v) vs



-- FIXME: Is this other implementation faster than mapM_?
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
