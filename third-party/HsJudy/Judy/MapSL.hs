{-# OPTIONS -fallow-undecidable-instances -fallow-incoherent-instances #-}

module Judy.MapSL (
    MapSL (..),
    swapMaps, freeze
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
import Judy.Map (Stringable (..))
import Judy.Freeze
import qualified Judy.MiniGC as GC

import Prelude hiding (map)

newtype (Stringable k, Refeable a) => MapSL k a = MapSL { judy :: ForeignPtr JudySL }
    deriving (Eq, Ord, Typeable)

instance (Stringable k, Refeable a) => CM.MapM (MapSL k a) k a IO where
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

instance (Stringable k, Refeable a) => Freezable (MapSL k a) where
    freeze m = do
        m' <- new_
        swapMaps m' m
        return (Frozen m')

instance (Stringable k, Refeable a) => CM.MapF (Frozen (MapSL k a)) k a where
    memberF k (Frozen m) = unsafePerformIO $ member_ k m
    lookupF k (Frozen m) = unsafePerformIO $ lookup_ k m
    fromListF l = Frozen $ unsafePerformIO $ fromList_ l
    toListF (Frozen m) = unsafePerformIO $ toList_ m

instance Show (MapSL k a) where
    show (MapSL j) = "<MapSL " ++ show j ++ ">"

foreign import ccall "wrapper" mkFin :: (Ptr JudySL -> IO ()) -> IO (FunPtr (Ptr JudySL -> IO ()))

finalize :: Bool -> Ptr JudySL -> IO ()
finalize need j = do
    --putStrLn $ show $ need
    when need $ do
        j_ <- newForeignPtr_ j
        es <- rawElems (MapSL j_)
        mapM_ GC.freeRef es
    v <- judySLFreeArray j judyError
    --putStrLn $ "\n(FINALIZER CALLED FOR "++ (show j) ++  ": " ++ (show v) ++ ")\n"
    return ()

rawElems = internalMap $ \r _ -> peek r

dummy :: Refeable a => MapSL k a -> a
dummy = undefined

new_ :: Refeable a => IO (MapSL k a)
new_ = do
    fp <- mallocForeignPtr
    withForeignPtr fp $ flip poke nullPtr
    m <- return $ MapSL fp

    -- putStrLn $ show $ needGC $ dummy m
    finalize' <- mkFin $ finalize $ needGC $ dummy m
    addForeignPtrFinalizer finalize' fp 
    return m

insert_ :: (Stringable k, Refeable a) => k -> a -> MapSL k a -> IO ()
insert_ k v (MapSL j) = withForeignPtr j $ \j' -> do
    useAsCS k $ \k' -> do
        r <- judySLIns j' k' judyError
        if r == pjerr
            then error "HsJudy: Not enough memory."
            else do { v' <- toRef v; poke r v'; return () }

alter_ :: (Eq a, Stringable k, Refeable a) => (Maybe a -> Maybe a) -> k -> MapSL k a -> IO (Maybe a)
alter_ f k m@(MapSL j) = do
    j' <- withForeignPtr j peek
    useAsCS k $ \k' -> do
        r <- judySLGet j' k' judyError
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
                            return Nothing
                    else if v /= (fromJust fv)
                             then do when (needGC (fromJust fv)) $ GC.freeRef v'
                                     x <- toRef (fromJust fv)
                                     poke r x
                                     return fv
                             else return fv

lookup_ :: (Stringable k, Refeable a) => k -> MapSL k a -> IO (Maybe a)
lookup_ k (MapSL j) = do
    j' <- withForeignPtr j peek
    useAsCS k $ \k' -> do
        r <- judySLGet j' k' judyError
        if r == nullPtr
            then return Nothing
            else do { v' <- peek r; v <- fromRef v'; return $ Just v }

member_ :: Stringable k => k -> MapSL k a -> IO Bool
member_ k (MapSL j) = do
    j' <- withForeignPtr j peek
    useAsCS k $ \k' -> do
        r <- judySLGet j' k' judyError
        return $ r /= nullPtr

delete_ :: (Stringable k, Refeable a) => k -> MapSL k a -> IO Bool
delete_ k m@(MapSL j) = withForeignPtr j $ \j' -> do
    j'' <- peek j'
    useAsCS k $ \k' -> do
        when (needGC (dummy m)) $ do
            r <- judySLGet j'' k' judyError
            if r == nullPtr
                then return () 
                else do v' <- peek r
                        GC.freeRef v'
                        return ()
        r <- judySLDel j' k' judyError
        return $ r /= 0


fromList_ :: (Stringable k, Refeable a) => [(k,a)] -> IO (MapSL k a)
fromList_ xs = do
    m <- new_
    mapM_ (\(k,a) -> insert_ k a m) xs
    return m

internalMap :: (Ptr Value -> CString -> IO b) -> MapSL k a -> IO [b]
internalMap f (MapSL j) = do
    jj <- withForeignPtr j peek
    alloca $ \vp -> do
        poke vp (-1)
        let loop act xs = do
            r <- act jj vp judyError
            if r == nullPtr
                then return xs
                else do x <- f r vp
                        loop judySLPrev (x:xs)
        loop judySLLast []

mapToList_ :: (Stringable k, Refeable a) => (k -> a -> b) -> MapSL k a -> IO [b]
mapToList_ f = internalMap $ \r vp -> do
    k <- copyCS vp
    v <- peek r
    v' <- fromRef v
    return $ f k v'

toList_ :: (Stringable k, Refeable a) => MapSL k a -> IO [(k,a)]
toList_ = mapToList_ $ \k a -> (k,a)


keys_ :: Stringable k => MapSL k a -> IO [k]
keys_ = internalMap $ \_ vp -> do
    k <- copyCS vp
    return k

elems_ :: Refeable a => MapSL k a -> IO [a]
elems_ = internalMap $ \r _ -> do
    v <- peek r
    fromRef v

swapMaps :: MapSL k a -> MapSL k a -> IO ()
swapMaps (MapSL j1) (MapSL j2) = do
    withForeignPtr j1 $ \p1 -> withForeignPtr j2 $ \p2 -> do
        v1 <- peek p1
        v2 <- peek p2
        poke p1 v2
        poke p2 v1
