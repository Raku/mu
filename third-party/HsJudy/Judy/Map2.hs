{-# OPTIONS -fallow-undecidable-instances -fallow-incoherent-instances #-}

module Judy.Map2 (
    Map2 (..),
    swapMaps, freeze,
    toRevList,
    size,
    takeFirstElems, takeFirst,
    takeLastElems, takeLast
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

newtype (ReversibleHashIO k, Refeable a) => Map2 k a = Map2 { judy :: ForeignPtr JudyL }
    deriving (Eq, Ord, Typeable)

instance (ReversibleHashIO k, Refeable a) => CM.MapM (Map2 k a) k a IO where
    new = new_
    delete = delete_
    member = member_
    lookup = lookup_
    insert = insert_
    alter = alter_
    fromList = fromList_
    toList = toList_
    elems = elems_
    keys = keys_
    mapToList = mapToList_

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

instance Show (Map2 k a) where
    show (Map2 j) = "<Map2 " ++ show j ++ ">"



foreign import ccall "wrapper" mkFin :: (Ptr JudyL -> IO ()) -> IO (FunPtr (Ptr JudyL -> IO ()))

finalize :: Bool -> Ptr JudyL -> IO ()
finalize need j = do
    when need $ do
        j_ <- newForeignPtr_ j
        es <- rawElems (Map2 j_)
        mapM_ GC.freeRef es
    v <- judyLFreeArray j judyError
    --putStrLn $ "\n(FINALIZER CALLED FOR "++ (show j) ++  ": " ++ (show v) ++ ")\n"
    return ()

rawElems = internalMap $ \r _ -> peek r

dummy :: Refeable a => Map2 k a -> a
dummy = undefined

new_ :: Refeable a => IO (Map2 k a)
new_ = do
    fp <- mallocForeignPtr
    withForeignPtr fp $ flip poke nullPtr
    m <- return $ Map2 fp

    finalize' <- mkFin $ finalize $ needGC (dummy m)
    addForeignPtrFinalizer finalize' fp 
    return m

insert_ :: (ReversibleHashIO k, Refeable a) => k -> a -> Map2 k a -> IO ()
insert_ k v (Map2 j) = withForeignPtr j $ \j' -> do
    k' <- hashIO k
    r <- judyLIns j' k' judyError
    if r == pjerr
        then error "HsJudy: Not enough memory."
        else do { v' <- toRef v; poke r v'; return () }

alter_ :: (Eq a, ReversibleHashIO k, Refeable a) => (Maybe a -> Maybe a) -> k -> Map2 k a -> IO (Maybe a)
alter_ f k m@(Map2 j) = do
    j' <- withForeignPtr j peek
    k' <- hashIO k
    r <- judyLGet j' k' judyError
    if r == nullPtr
        then if (f Nothing) == Nothing
                then return Nothing
                else insert_ k (fromJust (f Nothing)) m >> return (f Nothing)
        else do
            v' <- peek r
            v <- fromRef v'
            let fv = f (Just v)
            if fv == Nothing
                then do delete_ k m 
                        return Nothing           -- FIXME check delete output
                else if v /= (fromJust fv)
                         then do when (needGC (fromJust fv)) $ GC.freeRef v'
                                 x <- toRef (fromJust fv)
                                 poke r x
                                 return fv
                         else return fv

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
delete_ k m@(Map2 j) = withForeignPtr j $ \j' -> do
    j'' <- peek j'
    k' <- hashIO k
    when (needGC (dummy m)) $ do
        r <- judyLGet j'' k' judyError
        if r == nullPtr
            then return ()
            else do v' <- peek r
                    GC.freeRef v'
                    return ()
    r <- judyLDel j' k' judyError
    return $ r /= 0

size :: Map2 k a -> IO Int
size (Map2 j) = withForeignPtr j $ \j' -> do
    jj <- peek j'
    r <- judyLCount jj 0 (-1) judyError
    return $ fromEnum r



fromList_ :: (ReversibleHashIO k, Refeable a) => [(k,a)] -> IO (Map2 k a)
fromList_ xs = do
    m <- new_
    mapM_ (\(k,a) -> insert_ k a m) xs
    return m

internalMap' :: (Ptr Value -> Ptr Value -> IO b) -> Map2 k a -> IO [b]
internalMap' f (Map2 j) = do
    jj <- withForeignPtr j peek
    alloca $ \vp -> do
        poke vp (0 :: Value)
        let loop act xs = do
            r <- act jj vp judyError
            if r == nullPtr
                then return xs
                else do x <- f r vp
                        loop judyLNext (x:xs)
        loop judyLFirst []

withLast :: (Ptr Value -> Ptr Value -> IO b) -> Int -> Map2 k a -> IO [b]
withLast f n (Map2 j) = do
    jj <- withForeignPtr j peek
    alloca $ \vp -> do
        poke vp (-1)
        let loop _ xs 0 = return xs
            loop act xs n' = do
            r <- act jj vp judyError
            if r == nullPtr
                then return xs
                else do x <- f r vp
                        loop judyLPrev (x:xs) (n'-1)
        loop judyLLast [] n

-- FIXME: use a less obscure syntax =P
takeLast :: (ReversibleHashIO k, Refeable a) => Int -> Map2 k a -> IO [(k,a)]
takeLast n m = do
    withLast (\r vp -> do { k <- peek vp >>= unHashIO; v <- peek r >>= fromRef; return (k,v) }) n m

takeLastElems :: Refeable a => Int -> Map2 k a -> IO [a]
takeLastElems n m = do
    withLast (\r _ -> peek r >>= fromRef) n m




withFirst :: (Ptr Value -> Ptr Value -> IO b) -> Int -> Map2 k a -> IO [b]
withFirst f n (Map2 j) = do
    jj <- withForeignPtr j peek
    alloca $ \vp -> do
        poke vp (0 :: Value)
        let loop _ xs 0 = return xs
            loop act xs n' = do
            r <- act jj vp judyError
            if r == nullPtr
                then return xs
                else do x <- f r vp
                        loop judyLNext (x:xs) (n'-1)
        loop judyLFirst [] n

-- FIXME: For n < size, is better use this approach, but for
-- n ~= size would be better to use LPrev and LLast and dont reverse.


-- FIXME: use a less obscure syntax =P
takeFirst :: (ReversibleHashIO k, Refeable a) => Int -> Map2 k a -> IO [(k,a)]
takeFirst n m = do
    l <- withFirst (\r vp -> do { k <- peek vp >>= unHashIO; v <- peek r >>= fromRef; return (k,v) }) n m
    return $ reverse l

takeFirstElems :: Refeable a => Int -> Map2 k a -> IO [a]
takeFirstElems n m = do
    l <- withFirst (\r _ -> peek r >>= fromRef) n m
    return $ reverse l

internalMap :: (Ptr Value -> Ptr Value -> IO b) -> Map2 k a -> IO [b]
internalMap f (Map2 j) = do
    jj <- withForeignPtr j peek
    alloca $ \vp -> do
        poke vp (-1)
        let loop act xs = do
            r <- act jj vp judyError
            if r == nullPtr
                then return xs
                else do x <- f r vp
                        loop judyLPrev (x:xs)
        loop judyLLast [] -- Because of list concat we go backwards
                          -- to get ordered list right.

mapToList_ :: (ReversibleHashIO k, Refeable a) => (k -> a -> b) -> Map2 k a -> IO [b]
mapToList_ f = internalMap $ \r vp -> do
    k <- peek vp
    k' <- unHashIO k
    v <- peek r
    v' <- fromRef v
    return $ f k' v'

mapToRevList_ :: (ReversibleHashIO k, Refeable a) => (k -> a -> b) -> Map2 k a -> IO [b]
mapToRevList_ f = internalMap' $ \r vp -> do
    k <- peek vp
    k' <- unHashIO k
    v <- peek r
    v' <- fromRef v
    return $ f k' v'

toList_ :: (ReversibleHashIO k, Refeable a) => Map2 k a -> IO [(k,a)]
toList_ = mapToList_ $ \k a -> (k,a)

toRevList :: (ReversibleHashIO k, Refeable a) => Map2 k a -> IO [(k,a)]
toRevList = mapToRevList_ $ \k a -> (k,a)

keys_ :: ReversibleHashIO k => Map2 k a -> IO [k]
keys_ = internalMap $ \_ vp -> do
    k <- peek vp
    unHashIO k

elems_ :: Refeable a => Map2 k a -> IO [a]
elems_ = internalMap $ \r _ -> do
    v <- peek r
    fromRef v

swapMaps :: Map2 k a -> Map2 k a -> IO ()
swapMaps (Map2 j1) (Map2 j2) = do
    withForeignPtr j1 $ \p1 -> withForeignPtr j2 $ \p2 -> do
        v1 <- peek p1
        v2 <- peek p2
        poke p1 v2
        poke p2 v1
