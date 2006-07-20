{-# OPTIONS -fallow-undecidable-instances -fallow-incoherent-instances #-}

module Judy.Map2 (
    Map2 (..),
    keys, elems, map, swapMaps, freeze, alter2
) where

import Data.Typeable
import Control.Monad (when)
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
import qualified Judy.MiniGC as GC

import Prelude hiding (map)

instance (ReversibleHashIO k, Refeable a) => CM.MapM (Map2 k a) k a IO where
    new = new_
    delete k c = delete_ k c >> return ()
    member = member_
    lookup = lookup_
    alter = insert_
    fromList = fromList_
    toList = toList_

data (ReversibleHashIO k, Refeable a) => Map2 k a = Map2 { judy :: ForeignPtr JudyL, gc :: GC.MiniGC }
    deriving (Eq, Ord, Typeable)

instance Show (Map2 k a) where
    show (Map2 j _) = "<Map2 " ++ show j ++ ">"


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



foreign import ccall "wrapper" mkFin :: (Ptr JudyL -> IO ()) -> IO (FunPtr (Ptr JudyL -> IO ()))

finalize :: GC.MiniGC -> Ptr JudyL -> IO ()
finalize gc j = do
    v <- judyLFreeArray j judyError
    j_ <- newForeignPtr_ j
    es <- rawElems (Map2 j_ GC.NoGC)
    mapM_ (GC.freeRef gc) es
    putStrLn $ "\n(FINALIZER CALLED FOR "++ (show j) ++  ": " ++ (show v) ++ ")\n"
    return ()

rawElems = map_ $ \r _ -> peek r

new_ :: IO (Map2 k a)
new_ = do
    fp <- mallocForeignPtr
    gc <- if needGC (undefined :: a) then GC.createMiniGC else return GC.NoGC
    finalize' <- mkFin $ finalize gc
    addForeignPtrFinalizer finalize' fp 
    -- addForeignPtrFinalizer judyL_free_ptr fp 
    withForeignPtr fp $ flip poke nullPtr
    return $ Map2 fp gc

insert_ :: (ReversibleHashIO k, Refeable a) => k -> a -> Map2 k a -> IO ()
insert_ k v (Map2 j gc) = withForeignPtr j $ \j' -> do
    k' <- hashIO k
    r <- judyLIns j' k' judyError
    if r == pjerr
        then error "HsJudy: Not enough memory."
        else do { v' <- toRef gc v; poke r v'; return () }

alter2 :: (Eq a, ReversibleHashIO k, Refeable a) => (Maybe a -> Maybe a) -> k -> Map2 k a -> IO ()
alter2 f k m@(Map2 j gc) = do
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
            let fv = f (Just v)
            if fv == Nothing
                then do delete_ k m 
                        return ()
                else if v /= (fromJust fv)
                         then do GC.freeRef gc v'
                                 x <- toRef gc (fromJust fv)
                                 poke r x
                         else return ()

lookup_ :: (ReversibleHashIO k, Refeable a) => k -> Map2 k a -> IO (Maybe a)
lookup_ k (Map2 j _) = do
    j' <- withForeignPtr j peek
    k' <- hashIO k
    r <- judyLGet j' k' judyError
    if r == nullPtr
        then return Nothing
        else do { v' <- peek r; v <- fromRef v'; return $ Just v }

member_ :: ReversibleHashIO k => k -> Map2 k a -> IO Bool
member_ k (Map2 j _) = do
    j' <- withForeignPtr j peek
    k' <- hashIO k
    r <- judyLGet j' k' judyError
    return $ r /= nullPtr

delete_ :: ReversibleHashIO k => k -> Map2 k a -> IO Bool
delete_ k (Map2 j gc) = withForeignPtr j $ \j' -> do
    j'' <- peek j'
    k' <- hashIO k
    when (needGC (undefined :: a)) $ do
        r <- judyLGet j'' k' judyError
        if r == nullPtr
            then return ()
            else do v' <- peek r
                    GC.freeRef gc v'
                    return ()
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
map_ f (Map2 j _) = do
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

swapMaps :: Map2 k a -> Map2 k a -> IO ()
swapMaps (Map2 j1 gc1) (Map2 j2 gc2) = do
    GC.swapGCs gc1 gc2
    withForeignPtr j1 $ \p1 -> withForeignPtr j2 $ \p2 -> do
        v1 <- peek p1
        v2 <- peek p2
        poke p1 v2
        poke p2 v1





