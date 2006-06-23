{-# OPTIONS -fallow-undecidable-instances -fallow-incoherent-instances #-}

module Judy.Map2 (
    Map2 (..),
    keys, elems
) where

import Data.Typeable
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Foreign.StablePtr

import Judy.Private
import qualified Judy.CollectionsM as CM
import Judy.Refeable
import Judy.HashIO

instance (ReversibleHashIO k, Refeable a) => CM.MapM (Map2 k a) k a IO where
    new = new_
    delete k c = delete_ k c >> return ()
    member = member_
    lookup = lookup_
    alter = insert_
    fromList = fromList_
    toList = toList_

-- FIXME: Maybe when using own GC for stableptrs, refeable is viable as a key type

newtype (ReversibleHashIO k, Refeable a) => Map2 k a = Map2 { judy :: ForeignPtr JudyL }
    deriving (Eq, Ord, Typeable)

instance Show (Map2 k a) where
    show (Map2 j) = "<Map2 " ++ show j ++ ">"



-- copy/pasted FROM Judy/Map.hs -- some commented code arent translated yet

-- TODO: a "complete" finalizer (destroys StablePtrs): remember the case
-- when same StablePtr is being used by two keys, or that it maybe being
-- used by some other structure (you can't just free, need some refcounting
-- or use some newUniqueStablePtr, dunno yet)!

{-foreign import ccall "wrapper" mkFin :: (Ptr JudyHS -> IO ()) -> IO (FunPtr (Ptr JudyHS -> IO ()))

finalize :: Ptr JudyHS -> IO ()
finalize j = do
    v <- judyHSFreeArray j judyError
    putStrLn $ "\n (FINALIZER CALLED FOR "++ (show j) ++  ": " ++ (show v) ++ ") "
    return ()
-}
new_ :: IO (Map2 k a)
new_ = do
    fp <- mallocForeignPtr
    --putStr $ " (NEW on " ++ (show fp) ++ ") "
--    finalize' <- mkFin finalize
--    addForeignPtrFinalizer finalize' fp 
    withForeignPtr fp $ flip poke nullPtr
    return $ Map2 fp

insert_ :: (ReversibleHashIO k, Refeable a) => k -> a -> Map2 k a -> IO ()
insert_ k v (Map2 j) = withForeignPtr j $ \j' -> do
    k' <- hashIO k
    r <- judyLIns j' k' judyError
    if r == pjerr
        then error "HsJudy: Not enough memory."
        else do { v' <- toRef v; poke r v'; return () }

lookup_ :: (ReversibleHashIO k, Refeable a) => k -> Map2 k a -> IO (Maybe a)
lookup_ k (Map2 j) = do
    j' <- withForeignPtr j peek
    k' <- hashIO k
    r <- judyLGet j' k' judyError
    if r == nullPtr
        then return Nothing
        else do { v' <- peek r; v <- fromRef v'; return $ Just v }

member_ :: ReversibleHashIO k => k -> Map2 k a -> IO Bool
member_ k (Map2 j) = do
    j' <- withForeignPtr j peek
    k' <- hashIO k
    r <- judyLGet j' k' judyError
    return $ r /= nullPtr

del j wp = withForeignPtr j $ \j -> do
    r <- judyLDel j wp judyError
    return $ r /= 0

delete_ :: ReversibleHashIO k => k -> Map2 k a -> IO Bool
delete_ k (Map2 j) = withForeignPtr j $ \j' -> do
    k' <- hashIO k
    r <- judyLDel j' k' judyError
    return $ r /= 0

fromList_ :: (ReversibleHashIO k, Refeable a) => [(k,a)] -> IO (Map2 k a)
fromList_ xs = do
    m <- new_
    mapM_ (\(k,a) -> insert_ k a m) xs
    return m

--count j i1 i2 = withForeignPtr j $ \j -> do
--    jj <- peek j
--    r <- judyLCount jj i1 i2 judyError
--    return $ r


-- FIXME: DRY

toList_ :: (ReversibleHashIO k, Refeable a) => Map2 k a -> IO [(k,a)]
toList_ (Map2 j) = do
    jj <- withForeignPtr j peek
    alloca $ \vp -> do
        poke vp (-1)
        let f act xs = do
                r <- act jj vp judyError
                if r == nullPtr
                    then return xs
                    else do
                        k <- peek vp
                        k' <- unHashIO k
                        v <- peek r
                        v' <- fromRef v
                        f judyLPrev ((k', v'):xs)
        f judyLLast []


keys :: ReversibleHashIO k => Map2 k a -> IO [k]
keys (Map2 j) = do
    jj <- withForeignPtr j peek
    alloca $ \vp -> do
        poke vp (-1)
        let f act xs = do
                r <- act jj vp judyError
                if r == nullPtr
                    then return xs
                    else do
                        k <- peek vp
                        k' <- unHashIO k
                        f judyLPrev (k':xs)
        f judyLLast []

elems :: Refeable a => Map2 k a -> IO [a]
elems (Map2 j) = do
    jj <- withForeignPtr j peek
    alloca $ \vp -> do
        poke vp (-1)
        let f act xs = do
                r <- act jj vp judyError
                if r == nullPtr
                    then return xs
                    else do
                        v <- peek r
                        v' <- fromRef v
                        f judyLPrev (v':xs)
        f judyLLast []



