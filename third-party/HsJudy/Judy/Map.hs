module Judy.Map (
    Stringable (..),
    Map (..),
    new, insert, Judy.Map.lookup, member, delete,
    elems, keys, toList, fromList
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

-- TODO: use ByteString
-- TODO: instantiate CollectionM, MapM

-- TODO: a "complete" finalizer (destroys StablePtrs): remember the case
-- when same StablePtr is being used by two keys, or that it maybe being
-- used by some other structure (you can't just free, need some refcounting
-- or use some newUniqueStablePtr, dunno yet)!

-- TODO: Work on Storable to let any Storable type be
-- "stringable" i.e., used as Key.
-- FIXME: odd!
class Stringable k where
    toString :: k -> String
    fromString :: String -> k

instance Stringable String where
    toString = id
    fromString = id

--instance Stringable Int where
--    toString = show
--    fromString = read

newtype Stringable k => Map k a = Map { judy :: ForeignPtr JudyHS }
    deriving (Eq, Ord, Typeable)

instance Show (Map k a) where
    show (Map j) = "<Map " ++ show j ++ ">"

new :: IO (Map k a)
new = do
    fp <- mallocForeignPtr
    -- TODO: addForeignPtrFinalizer
    withForeignPtr fp $ flip poke nullPtr
    return $ Map fp

insert :: Stringable k => k -> a -> Map k a -> IO ()
insert k v (Map j) = withForeignPtr j $ \j' -> do
    withCAStringLen (toString k) $ \(cp, len) -> do
        -- TODO: maybe there's a better way to convert Int -> Value
        r <- judyHSIns j' cp (fromIntegral len) judyError
        if r == pjerr
            then error "HsJudy: Not enough memory."
            else do
                v' <- newStablePtr v
                poke r (ptrToWordPtr (castStablePtrToPtr v'))
                return ()

lookup :: Stringable k => k -> Map k a -> IO (Maybe a)
lookup k (Map j) = do
    j' <- withForeignPtr j peek
    withCAStringLen (toString k) $ \(cp, len) -> do
        r <- judyHSGet j' cp (fromIntegral len)
        if r == nullPtr
            then return Nothing
            else do 
                v' <- peek r
                v <- deRefStablePtr (castPtrToStablePtr (wordPtrToPtr v'))
                return $ Just v

member :: Stringable k => k -> Map k a -> IO Bool
member k (Map j) = do
    j' <- withForeignPtr j peek
    withCAStringLen (toString k) $ \(cp, len) -> do
        r <- judyHSGet j' cp (fromIntegral len)
        return $ r /= nullPtr

delete :: Stringable k => k -> Map k a -> IO Bool
delete k (Map j) = withForeignPtr j $ \j -> do
    withCAStringLen (toString k) $ \(cp, len) -> do
        r <- judyHSDel j cp (fromIntegral len) judyError
        -- TODO: must free the stableptr
        return $ r /= 0

-- FIXME: may use MapIter type to enforce some safety in its use?
newtype MapIter = MapIter { iter :: ForeignPtr JudyHSIter }
    deriving (Eq, Ord, Typeable)

instance Show MapIter where
    show (MapIter i) = "<Iter "++ show i ++ ">"


-- FIXME: elems and keys => DRY
newIter :: IO (MapIter)
newIter = do
    fp <- mallocForeignPtr
    addForeignPtrFinalizer judyHSIter_free_ptr fp
    withForeignPtr fp $ flip poke nullPtr
    return $ MapIter fp 

fromList :: Stringable k => [(k,a)] -> IO (Map k a)
fromList xs = do
    m <- new
    mapM_ (\(k,a) -> insert k a m) xs
    return m

-- FIXME: DRY

toList :: Stringable k => Map k a -> IO [(k,a)]
toList (Map j) = do
    jj <- withForeignPtr j peek
    (MapIter i) <- newIter
    withForeignPtr i $ \ii -> alloca $ \cp -> alloca $ \len -> do
        let f act xs = do
                r <- act jj ii cp len judyError
                if r == nullPtr
                    then return xs
                    else do
                        l <- peek len
                        c <- peek cp
                        v <- peekCAStringLen (c, fromIntegral l)
                        d <- peek r
                        d' <- deRefStablePtr (castPtrToStablePtr (wordPtrToPtr d))
                        f judyHSIterNext ((fromString v, d'):xs)
        f judyHSIterFirst []


elems :: Map k a -> IO [a]
elems (Map j) = do
    jj <- withForeignPtr j peek
    (MapIter i) <- newIter
    withForeignPtr i $ \ii -> alloca $ \cp -> alloca $ \len -> do
        let f act xs = do
                r <- act jj ii cp len judyError
                if r == nullPtr
                    then return xs
                    else do
                        d <- peek r
                        d' <- deRefStablePtr (castPtrToStablePtr (wordPtrToPtr d))
                        f judyHSIterNext (d':xs)
        f judyHSIterFirst []

keys :: Stringable k => Map k a -> IO [k]
keys (Map j) = do
    jj <- withForeignPtr j peek
    (MapIter i) <- newIter
    withForeignPtr i $ \ii -> alloca $ \cp -> alloca $ \len -> do
        let f act xs = do
                r <- act jj ii cp len judyError
                if r == nullPtr
                    then return xs
                    else do
                        l <- peek len
                        c <- peek cp
                        v <- peekCAStringLen (c, fromIntegral l)
                        f judyHSIterNext ((fromString v):xs)
        f judyHSIterFirst []
