{-# OPTIONS -fallow-undecidable-instances -fallow-incoherent-instances #-}

module Judy.Map2 (
    Map2 (..),
    keys, elems, map, swapMaps, freeze, alter2
) where

import Data.Typeable
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Foreign.StablePtr
import Foreign
import Data.Maybe (fromJust)

import Judy.Private
import qualified Judy.CollectionsM as CM
import Judy.Refeable
import Judy.HashIO
import Judy.Freeze


import Prelude hiding (map)

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


instance (ReversibleHashIO k, Refeable a) => Freezable (Map2 k a) where
    freeze m = do
        m' <- new_
        swapMaps m' m
        return (Frozen m')

instance (ReversibleHashIO k, Refeable a) => CM.MapF (Frozen (Map2 k a)) k a where
    memberF k (Frozen m) = unsafePerformIO $ member_ k m
    lookupF k (Frozen m) = unsafePerformIO $ lookup_ k m
    fromListF l = Frozen $ unsafePerformIO $ fromList_ l
    toListF (Frozen m) = unsafePerformIO $ toList_ m



swapMaps :: Map2 k a -> Map2 k a -> IO ()
swapMaps (Map2 j1) (Map2 j2) = do
    withForeignPtr j1 $ \p1 -> withForeignPtr j2 $ \p2 -> do
        v1 <- peek p1
        v2 <- peek p2
        poke p1 v2
        poke p2 v1

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

alter2 :: (Eq a, ReversibleHashIO k, Refeable a) => (Maybe a -> Maybe a) -> k -> Map2 k a -> IO ()
alter2 f k m@(Map2 j) = do
    j' <- withForeignPtr j peek
    k' <- hashIO k
    r <- judyLGet j' k' judyError
    if r == nullPtr
        then if (f Nothing) == Nothing
                then return ()
                else insert_ k (fromJust (f Nothing)) m
        else do
            v' <- peek r
            v <- fromRef v'
            if (f (Just v)) == Nothing
                then do delete_ k m
                        return ()
                else do x <- toRef $ fromJust $ f (Just v)
                        poke r x

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

map_ :: (Ptr Value -> Ptr Value -> IO b) -> Map2 k a -> IO [b]
map_ f (Map2 j) = do
    jj <- withForeignPtr j peek
    alloca $ \vp -> do
        poke vp (-1)
        let loop act xs = do
            r <- act jj vp judyError
            if r == nullPtr
                then return xs
                else do x <- f r vp
                        loop judyLPrev (x:xs)
        loop judyLLast []

map :: (ReversibleHashIO k, Refeable a) => (k -> a -> b) -> Map2 k a -> IO [b]
map f = map_ $ \r vp -> do
    k <- peek vp
    k' <- unHashIO k
    v <- peek r
    v' <- fromRef v
    return $ f k' v'

toList_ :: (ReversibleHashIO k, Refeable a) => Map2 k a -> IO [(k,a)]
toList_ = map $ \k a -> (k,a)

keys :: ReversibleHashIO k => Map2 k a -> IO [k]
keys = map_ $ \_ vp -> do
    k <- peek vp
    unHashIO k

elems :: Refeable a => Map2 k a -> IO [a]
elems = map_ $ \r _ -> do
    v <- peek r
    fromRef v



