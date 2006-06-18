{-# OPTIONS -fallow-undecidable-instances -fallow-incoherent-instances #-}

module Judy.Map (
    Stringable (..),
    Refeable (..),
    Map (..),
--    {-new,-} insert, --Judy.Map.lookup, --member, -- delete,
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
import qualified Judy.CollectionsM as CM


instance (Stringable k, Refeable a) => CM.MapM (Map k a) k a IO where
    new = new_
    delete k c = delete_ k c >> return ()
    member = member_
    lookup = lookup_
    alter = insert_


-- TODO: use ByteString
-- TODO: instantiate CollectionM, MapM

-- TODO: Work on Storable to let any Storable type be
-- "stringable" i.e., used as Key.
-- FIXME: odd!
class Stringable k where
    toString :: k -> String
    fromString :: String -> k

instance Stringable String where
    toString = id
    fromString = id

class Refeable a where
    toRef :: a -> IO Value
    fromRef :: Value -> IO a
    -- freeRef will be used in finalizer
    -- freeRef :: a -> IO ()


-- FIXME: It results in an illegal instruction if I drop the Dummy instance.
-- Maybe something arch related, dunno. =P

class Dummy a
instance Dummy a

instance Dummy a => Refeable a where
    toRef a = do
        a' <- newStablePtr a
        return (ptrToWordPtr (castStablePtrToPtr a'))
    fromRef v = do
        a <- deRefStablePtr (castPtrToStablePtr (wordPtrToPtr v))
        return a

-- Don't need to StablePtr "simple" things like Int
instance Refeable Int where
    toRef i = return $ toEnum i
    fromRef v = return $ fromEnum v

 

--instance Stringable Int where
--    toString = show
--    fromString = read

newtype Stringable k => Map k a = Map { judy :: ForeignPtr JudyHS }
    deriving (Eq, Ord, Typeable)

instance Show (Map k a) where
    show (Map j) = "<Map " ++ show j ++ ">"


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
new_ :: IO (Map k a)
new_ = do
    fp <- mallocForeignPtr
    putStr $ " (NEW on " ++ (show fp) ++ ") "
--    finalize' <- mkFin finalize
--    addForeignPtrFinalizer finalize' fp 
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
delete_ k (Map j) = withForeignPtr j $ \j -> do
    withCAStringLen (toString k) $ \(cp, len) -> do
        r <- judyHSDel j cp (fromIntegral len) judyError
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
    addForeignPtrFinalizer judyHSIter_free_ptr fp
    withForeignPtr fp $ flip poke nullPtr
    return $ MapIter fp

fromList :: (Stringable k, Refeable a) => [(k,a)] -> IO (Map k a)
fromList xs = do
    m <- new_
    mapM_ (\(k,a) -> insert_ k a m) xs
    return m

-- FIXME: DRY

toList :: (Stringable k, Refeable a) => Map k a -> IO [(k,a)]
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
                        d' <- fromRef d
                        f judyHSIterNext ((fromString v, d'):xs)
        f judyHSIterFirst []


elems :: Refeable a => Map k a -> IO [a]
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
                        d' <- fromRef d
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
