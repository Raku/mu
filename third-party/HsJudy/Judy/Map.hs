{-# OPTIONS -fallow-undecidable-instances -fallow-incoherent-instances #-}

module Judy.Map (
    Stringable (..),
    Map (..),

    -- FIXME: need to move to MapM api
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
import Foreign
import Data.Maybe (fromJust)
import qualified Data.ByteString as B

import Judy.Private
import qualified Judy.CollectionsM as CM
import Judy.Refeable
import Judy.Freeze
import qualified Judy.MiniGC as GC

import Prelude hiding (map)

-- Refeable is now in Judy/Refeable.hs

{- Stringable stuff -} 
-- TODO: Work on Storable to let any Storable type be
-- "stringable" i.e., used as Key.

-- TODO: Support using ByteStrings, seems to be trivial with Stringable, just instantiate?

class Stringable k where
    toString :: k -> String
    fromString :: String -> k

    useAsCS :: k -> (CString -> IO a) -> IO a
    useAsCS k = withCAString (toString k)
    useAsCSLen :: k -> (CStringLen -> IO a) -> IO a
    useAsCSLen k = withCAStringLen (toString k)

    copyCS :: CString -> IO k
    copyCS c = peekCAString c >>= return . fromString
    copyCSLen :: CStringLen -> IO k
    copyCSLen c = peekCAStringLen c >>= return . fromString

instance Stringable String where
    toString = id
    fromString = id

instance Stringable B.ByteString where
    toString = undefined
    fromString = undefined

    useAsCS = B.useAsCString 
    useAsCSLen = B.useAsCStringLen

    copyCS = B.copyCString
    copyCSLen = B.copyCStringLen


--instance Stringable Int where
--    toString = show
--    fromString = read
{- End of Stringable stuff -}

-- FIXME: really necessary/useful restrict types here?
newtype (Stringable k, Refeable a) => Map k a = Map { judy :: ForeignPtr JudyHS }
    deriving (Eq, Ord, Typeable)

instance (Stringable k, Refeable a) => CM.MapM (Map k a) k a IO where
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

instance Show (Map k a) where
    show (Map j) = "<Map " ++ show j ++ ">"

foreign import ccall "wrapper" mkFin :: (Ptr JudyHS -> IO ()) -> IO (FunPtr (Ptr JudyHS -> IO ()))

finalize :: Bool -> Ptr JudyHS -> IO ()
finalize need j = do
    when need $ do
        j_ <- newForeignPtr_ j
        es <- rawElems (Map j_)
        mapM_ GC.freeRef es
    v <- judyHSFreeArray j judyError
    putStrLn $ "\n(FINALIZER CALLED FOR "++ (show j) ++  ": " ++ (show v) ++ ")\n"
    return ()

rawElems = internalMap $ \r _ _ -> peek r

new_ :: IO (Map k a)
new_ = do
    fp <- mallocForeignPtr
    finalize' <- mkFin $ finalize (needGC (undefined :: a))
    addForeignPtrFinalizer finalize' fp 
    withForeignPtr fp $ flip poke nullPtr
    return $ Map fp

insert_ :: (Stringable k, Refeable a) => k -> a -> Map k a -> IO ()
insert_ k v (Map j) = withForeignPtr j $ \j' -> do
    useAsCSLen k $ \(cp, len) -> do
        -- TODO: maybe there's a better way to convert Int -> Value
        r <- judyHSIns j' cp (fromIntegral len) judyError
        if r == pjerr
            then error "HsJudy: Not enough memory."
            else do
                v' <- toRef v
                poke r v'
                return ()

alter_ :: (Eq a, Stringable k, Refeable a) => (Maybe a -> Maybe a) -> k -> Map k a -> IO (Maybe a)
alter_ f k m@(Map j) = do
    j' <- withForeignPtr j peek
    useAsCSLen k $ \(cp, len) -> do
        r <- judyHSGet j' cp (fromIntegral len)
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
                             then do when (needGC (undefined :: a)) $ GC.freeRef v'
                                     x <- toRef (fromJust fv)
                                     poke r x
                                     return fv
                             else return fv

lookup_ :: (Stringable k, Refeable a) => k -> Map k a -> IO (Maybe a)
lookup_ k (Map j) = do
    j' <- withForeignPtr j peek
    useAsCSLen k $ \(cp, len) -> do
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
    useAsCSLen k $ \(cp, len) -> do
        r <- judyHSGet j' cp (fromIntegral len)
        return $ r /= nullPtr

delete_ :: Stringable k => k -> Map k a -> IO Bool
delete_ k (Map j) = withForeignPtr j $ \j' -> do
    j'' <- peek j'
    useAsCSLen k $ \(cp, len) -> do
        when (needGC (undefined :: a)) $ do
            r <- judyHSGet j'' cp (fromIntegral len)
            if r == nullPtr
                then return ()
                else do v' <- peek r
                        GC.freeRef v'
                        return ()
        r <- judyHSDel j' cp (fromIntegral len) judyError
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

internalMap :: (Ptr Value -> Ptr CString -> Ptr Value -> IO b) -> Map k a -> IO [b]
internalMap f (Map j) = do
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

mapToList_ :: (Stringable k, Refeable a) => (k -> a -> b) -> Map k a -> IO [b]
mapToList_ f = internalMap $ \r cp len -> do
    l <- peek len
    c <- peek cp
    v <- copyCSLen (c, fromIntegral l)
    d <- peek r
    d' <- fromRef d
    return $ f v d'

toList_ :: (Stringable k, Refeable a) => Map k a -> IO [(k,a)]
toList_ = mapToList_ $ \k a -> (k, a)

elems_ :: Refeable a => Map k a -> IO [a]
elems_ = internalMap $ \r _ _ -> do
    d <- peek r
    fromRef d

keys_ :: Stringable k => Map k a -> IO [k]
keys_ = internalMap $ \_ cp len -> do
    l <- peek len
    c <- peek cp
    v <- copyCSLen (c, fromIntegral l)
    return v


swapMaps :: Map k a -> Map k a -> IO ()
swapMaps (Map j1) (Map j2) = do
    withForeignPtr j1 $ \p1 -> withForeignPtr j2 $ \p2 -> do
        v1 <- peek p1
        v2 <- peek p2
        poke p1 v2
        poke p2 v1
