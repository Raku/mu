{-# OPTIONS -fallow-undecidable-instances -fallow-incoherent-instances #-}

module Judy.Map (
    Stringable (..),
    Map (..),

    -- FIXME: need to move to MapM api
    elems, keys, map, swapMaps, freeze, alter2
) where

import Data.Typeable
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Foreign
import Data.Maybe (fromJust)


import Judy.Private
import qualified Judy.CollectionsM as CM
import Judy.Refeable
import Judy.Freeze

import Prelude hiding (map)

-- Refeable is now in Judy/Refeable.hs

instance (Stringable k, Refeable a) => CM.MapM (Map k a) k a IO where
    new = new_
    delete k c = delete_ k c >> return ()
    member = member_
    lookup = lookup_
    alter = insert_
    fromList = fromList_
    toList = toList_


instance (Stringable k, Refeable a) => Freezable (Map k a) where
    freeze m = do
        m' <- new_
        swapMaps m' m
        return (Frozen m')

instance (Stringable k, Refeable a) => CM.MapF (Frozen (Map k a)) k a where
    memberF k (Frozen m) = unsafePerformIO $ member_ k m
    lookupF k (Frozen m) = unsafePerformIO $ lookup_ k m
    fromListF l = Frozen $ unsafePerformIO $ fromList_ l
    toListF (Frozen m) = unsafePerformIO $ toList_ m

-- TODO: Work on Storable to let any Storable type be
-- "stringable" i.e., used as Key.

-- TODO: Support using ByteStrings, seems to be trivial with Stringable, just instantiate?

class Stringable k where
    toString :: k -> String
    fromString :: String -> k

instance Stringable String where
    toString = id
    fromString = id



--instance Stringable Int where
--    toString = show
--    fromString = read

-- FIXME: really necessary/useful restrict types here?
newtype (Stringable k, Refeable a) => Map k a = Map { judy :: ForeignPtr JudyHS }
    deriving (Eq, Ord, Typeable)

instance Show (Map k a) where
    show (Map j) = "<Map " ++ show j ++ ">"





-- TODO: a "complete" finalizer (destroys StablePtrs): remember the case
-- when same StablePtr is being used by two keys, or that it maybe being
-- used by some other structure (you can't just free, need some refcounting
-- or use some newUniqueStablePtr, dunno yet)!

-- Plan: make a mini-GC of my own.

foreign import ccall "wrapper" mkFin :: (Ptr JudyHS -> IO ()) -> IO (FunPtr (Ptr JudyHS -> IO ()))

finalize :: Ptr JudyHS -> IO ()
finalize j = do
    v <- judyHSFreeArray j judyError
    --putStrLn $ "\n(FINALIZER CALLED FOR "++ (show j) ++  ": " ++ (show v) ++ ")\n"
    return ()

new_ :: IO (Map k a)
new_ = do
    fp <- mallocForeignPtr
--    putStr $ " (NEW on " ++ (show fp) ++ ") "
--    finalize' <- mkFin finalize
--    addForeignPtrFinalizer finalize' fp 
    addForeignPtrFinalizer judyHS_free_ptr fp 
    withForeignPtr fp $ flip poke nullPtr
    return $ Map fp

insert_ :: (Stringable k, Refeable a) => k -> a -> Map k a -> IO ()
insert_ k v (Map j) = withForeignPtr j $ \j' -> do
    withCAStringLen (toString k) $ \(cp, len) -> do
        -- TODO: maybe there's a better way to convert Int -> Value
        r <- judyHSIns j' cp (fromIntegral len) judyError
        if r == pjerr
            then error "HsJudy: Not enough memory."
            else do
                v' <- toRef v
                poke r v'
                return ()

alter2 :: (Eq a, Stringable k, Refeable a) => (Maybe a -> Maybe a) -> k -> Map k a -> IO ()
alter2 f k m@(Map j) = do
    j' <- withForeignPtr j peek
    withCAStringLen (toString k) $ \(cp, len) -> do
        r <- judyHSGet j' cp (fromIntegral len)
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
                    else do
                            x <- toRef $ fromJust $ f (Just v)
                            poke r x

lookup_ :: (Stringable k, Refeable a) => k -> Map k a -> IO (Maybe a)
lookup_ k (Map j) = do
    j' <- withForeignPtr j peek
    withCAStringLen (toString k) $ \(cp, len) -> do
        r <- judyHSGet j' cp (fromIntegral len)
        if r == nullPtr
            then return Nothing
            else do 
                v' <- peek r
                v <- fromRef v'
                return $ Just v

member_ :: Stringable k => k -> Map k a -> IO Bool
member_ k (Map j) = do
    j' <- withForeignPtr j peek
    withCAStringLen (toString k) $ \(cp, len) -> do
        r <- judyHSGet j' cp (fromIntegral len)
        return $ r /= nullPtr

delete_ :: Stringable k => k -> Map k a -> IO Bool
delete_ k (Map j) = withForeignPtr j $ \j' -> do
    withCAStringLen (toString k) $ \(cp, len) -> do
        r <- judyHSDel j' cp (fromIntegral len) judyError
        -- TODO: must free the stableptr
        return $ r /= 0

-- FIXME: may use MapIter type to enforce some safety in its use?
newtype MapIter = MapIter { iter :: ForeignPtr JudyHSIter }
    deriving (Eq, Ord, Typeable)

instance Show MapIter where
    show (MapIter i) = "<Iter "++ show i ++ ">"


newIter :: IO (MapIter)
newIter = do
    fp <- mallocForeignPtr
--    addForeignPtrFinalizer judyHSIter_free_ptr fp
    withForeignPtr fp $ flip poke nullPtr
    return $ MapIter fp

fromList_ :: (Stringable k, Refeable a) => [(k,a)] -> IO (Map k a)
fromList_ xs = do
    m <- new_
    mapM_ (\(k,a) -> insert_ k a m) xs
    return m

map_ :: (Ptr Value -> Ptr CString -> Ptr Value -> IO b) -> Map k a -> IO [b]
map_ f (Map j) = do
    jj <- withForeignPtr j peek
    (MapIter i) <- newIter
    withForeignPtr i $ \ii -> alloca $ \cp -> alloca $ \len -> do
        poke len 0
        jp_null cp
        let loop act xs = do
            r <- act jj ii cp len judyError
            if r == nullPtr
                then return xs
                else do x <- f r cp len
                        loop judyHSIterNext (x:xs)
        loop judyHSIterFirst []

map :: (Stringable k, Refeable a) => (k -> a -> b) -> Map k a -> IO [b]
map f = map_ $ \r cp len -> do
    l <- peek len
    c <- peek cp
    v <- peekCAStringLen (c, fromIntegral l)
    d <- peek r
    d' <- fromRef d
    return $ f (fromString v) d'

toList_ :: (Stringable k, Refeable a) => Map k a -> IO [(k,a)]
toList_ = map $ \k a -> (k, a)

elems :: Refeable a => Map k a -> IO [a]
elems = map_ $ \r _ _ -> do
    d <- peek r
    fromRef d

keys :: Stringable k => Map k a -> IO [k]
keys = map_ $ \_ cp len -> do
    l <- peek len
    c <- peek cp
    v <- peekCAStringLen (c, fromIntegral l)
    return $ fromString v


swapMaps :: Map k a -> Map k a -> IO ()
swapMaps (Map j1) (Map j2) = do
    withForeignPtr j1 $ \p1 -> withForeignPtr j2 $ \p2 -> do
        v1 <- peek p1
        v2 <- peek p2
        poke p1 v2
        poke p2 v1
