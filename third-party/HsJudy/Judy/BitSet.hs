{-# OPTIONS -fallow-undecidable-instances -fallow-incoherent-instances #-}

module Judy.BitSet where

import Data.Typeable
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr
import System.IO.Unsafe

import Judy.Private
import Judy.Freeze
import Judy.HashIO


newtype HashIO a => BitSet a = BitSet { judy :: ForeignPtr Judy1 }
    deriving (Eq, Ord, Typeable)

instance Show (BitSet a) where
    show (BitSet bs) = "<BitSet " ++ show bs ++ ">"


-- | Swap contents of two sets.
swapBitSets :: BitSet a -> BitSet a -> IO ()
swapBitSets (BitSet j1) (BitSet j2) = do
    withForeignPtr j1 $ \p1 ->  do
        withForeignPtr j2 $ \p2 ->  do
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
insert :: HashIO a => a -> BitSet a -> IO ()
insert v (BitSet j) = withForeignPtr j $ \j' -> do
    v' <- hashIO v
    judy1Set j' v' judyError
    if v' == jerr
        then putStrLn "HsJudy: Not enough memory."
        else return ()

-- | Delete a value in the set.
delete :: HashIO a => a -> BitSet a -> IO ()
delete v (BitSet j) = withForeignPtr j $ \j' -> do
    v' <- hashIO v
    judy1Unset j' v' judyError
    if v' == jerr
        then error "HsJudy: Not enough memory."
        else return ()

-- | Set value in or out the set and return its old value.
set :: HashIO a => BitSet a -> a -> Bool -> IO Bool
set (BitSet j) v True = withForeignPtr j $ \j ->  do
    vp <- hashIO v
    r <- judy1Set j vp judyError
    if vp == jerr
        then error "HsJudy: Not enough memory."
        else return $ r == 0
set (BitSet j) v False = withForeignPtr j $ \j -> do
    vp <- hashIO v
    r <- judy1Unset j vp judyError
    if vp == jerr
        then error "HsJudy: Not enough memory."
        else return $ r /= 0

-- this inline was in Meacham original BitSet
-- {-# INLINE get #-}
get :: HashIO a => BitSet a -> a -> IO Bool
get (BitSet j) v = do
    jj <- withForeignPtr j peek
    vp <- hashIO v
    r <- judy1Test jj vp judyError
    return $ r /= 0

-- | Is the value a member of the set? 
member :: HashIO a => a -> BitSet a -> IO Bool
member v (BitSet j) = do
    j' <- withForeignPtr j peek
    v' <- hashIO v
    r <- judy1Test j' v' judyError
    return $ r /= 0

-- | Is the set empty?
null :: BitSet a -> IO Bool
null (BitSet j) = do
    j' <- withForeignPtr j peek
    return $ j' == nullPtr

-- | Cardinality of the set.
size :: BitSet a -> IO Int
size (BitSet j) = do
    j' <- withForeignPtr j peek
    r <- judy1Count j' 0 (-1) judyError
    return $ fromEnum r

-- | Make the set empty.
clear :: HashIO a => BitSet a -> IO ()
clear (BitSet j) = withForeignPtr j $ \j' -> judy1FreeArray j' judyError >> return ()

-- | Convert the set to a list of elements.
toList :: ReversibleHashIO a => BitSet a -> IO [a]
toList (BitSet j) = do
    j' <- withForeignPtr j peek
    alloca $ \vp -> do
        poke vp (-1)
        let f 0 xs = return xs
            f _ xs = do
                v <- peek vp
                v' <- unHashIO v
                r <- judy1Prev j' vp judyError
                f r (v':xs)
        r <- judy1Last j' vp judyError
        f r []

-- | Create a set from a list of elements.
-- FIXME: should I create the list here maybe?
fromList :: HashIO a => [a] -> BitSet a -> IO ()
fromList vs bs = mapM_ (\v -> insert v bs) vs



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





-- Pure access routines from original BitSet code

instance HashIO a => Freezable (BitSet a) where
    freeze = freezeBitSet

-- | Create a frozen, immutable version of a bitset, the original mutable version is cleared.
freezeBitSet :: HashIO a => BitSet a -> IO (Frozen (BitSet a))
freezeBitSet bs = do
    nbs <- new
    swapBitSets bs nbs
    return (Frozen nbs)

memberF :: HashIO a => a -> Frozen (BitSet a) -> Bool
memberF v (Frozen bs) = unsafePerformIO $ get bs v

fromListF :: HashIO a => [a] -> Frozen (BitSet a)
fromListF vs = Frozen $ unsafePerformIO $ do
    bs <- new
    fromList vs bs
    return bs

toListF :: ReversibleHashIO a => Frozen (BitSet a) -> [a]
toListF (Frozen (BitSet j)) = unsafePerformIO $ do
    j' <- withForeignPtr j peek
    alloca $ \vp -> do
        poke vp (-1)
        let f 0 xs = return xs
            f _ xs = do
                v <- peek vp
                v' <- unHashIO v
                r <- judy1Prev j' vp judyError
                f r (v':xs)
        r <- judy1Last j' vp judyError
        f r []


-- TODO: See if ListFrom and RevList are needed
-- compare my toListF with toListFrom (it have more unsafePerformIO's =P)

{-
toList :: Frozen (BitSet a) -> [Value]
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

-}

