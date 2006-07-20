{-# OPTIONS -fallow-undecidable-instances -fallow-incoherent-instances #-}

module Judy.MapSL (
    MapSL (..),
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
import Judy.Map (Stringable (..))
import Judy.Freeze
import qualified Judy.MiniGC as GC

import Prelude hiding (map)

instance (Stringable k, Refeable a) => CM.MapM (MapSL k a) k a IO where
    new = new_
    delete k c = delete_ k c >> return ()
    member = member_
    lookup = lookup_
    alter = insert_
    fromList = fromList_
    toList = toList_

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


newtype (Stringable k, Refeable a) => MapSL k a = MapSL { judy :: ForeignPtr JudySL }
    deriving (Eq, Ord, Typeable)

instance Show (MapSL k a) where
    show (MapSL j) = "<MapSL " ++ show j ++ ">"


foreign import ccall "wrapper" mkFin :: (Ptr JudySL -> IO ()) -> IO (FunPtr (Ptr JudySL -> IO ()))

finalize :: Bool -> Ptr JudySL -> IO ()
finalize need j = do
    when need $ do
        j_ <- newForeignPtr_ j
        es <- rawElems (MapSL j_)
        mapM_ GC.freeRef es
    v <- judySLFreeArray j judyError
    putStrLn $ "\n(FINALIZER CALLED FOR "++ (show j) ++  ": " ++ (show v) ++ ")\n"
    return ()

rawElems = map_ $ \r _ -> peek r

new_ :: IO (MapSL k a)
new_ = do
    fp <- mallocForeignPtr
    finalize' <- mkFin $ finalize (needGC (undefined :: a))
    addForeignPtrFinalizer finalize' fp 
    withForeignPtr fp $ flip poke nullPtr
    return $ MapSL fp

insert_ :: (Stringable k, Refeable a) => k -> a -> MapSL k a -> IO ()
insert_ k v (MapSL j) = withForeignPtr j $ \j' -> do
    useAsCS k $ \k' -> do
        r <- judySLIns j' k' judyError
        if r == pjerr
            then error "HsJudy: Not enough memory."
            else do { v' <- toRef v; poke r v'; return () }

alter2 :: (Eq a, Stringable k, Refeable a) => (Maybe a -> Maybe a) -> k -> MapSL k a -> IO ()
alter2 f k m@(MapSL j) = do
    j' <- withForeignPtr j peek
    useAsCS k $ \k' -> do
        r <- judySLGet j' k' judyError
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
                             then do when (needGC (undefined :: a)) $ GC.freeRef v'
                                     x <- toRef (fromJust fv)
                                     poke r x
                             else return ()

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

delete_ :: Stringable k => k -> MapSL k a -> IO Bool
delete_ k (MapSL j) = withForeignPtr j $ \j' -> do
    j'' <- peek j'
    useAsCS k $ \k' -> do
        when (needGC (undefined :: a)) $ do
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

map_ :: (Ptr Value -> CString -> IO b) -> MapSL k a -> IO [b]
map_ f (MapSL j) = do
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

map :: (Stringable k, Refeable a) => (k -> a -> b) -> MapSL k a -> IO [b]
map f = map_ $ \r vp -> do
    k <- copyCS vp
    v <- peek r
    v' <- fromRef v
    return $ f k v'

toList_ :: (Stringable k, Refeable a) => MapSL k a -> IO [(k,a)]
toList_ = map $ \k a -> (k,a)


keys :: Stringable k => MapSL k a -> IO [k]
keys = map_ $ \_ vp -> do
    k <- copyCS vp
    return k

elems :: Refeable a => MapSL k a -> IO [a]
elems = map_ $ \r _ -> do
    v <- peek r
    fromRef v

swapMaps :: MapSL k a -> MapSL k a -> IO ()
swapMaps (MapSL j1) (MapSL j2) = do
    withForeignPtr j1 $ \p1 -> withForeignPtr j2 $ \p2 -> do
        v1 <- peek p1
        v2 <- peek p2
        poke p1 v2
        poke p2 v1
