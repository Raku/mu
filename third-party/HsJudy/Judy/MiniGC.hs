module Judy.MiniGC (
    MiniGC(..), createMiniGC, newRef, freeRef, swapGCs
) where

import Data.Typeable
import Data.Maybe (fromJust)

import Foreign
import Foreign.Ptr
import Foreign.StablePtr


import Judy.Private

data MiniGC = MiniGC Map | NoGC deriving (Eq, Ord, Typeable)

table (MiniGC j) = j

createMiniGC :: IO MiniGC
createMiniGC = do
    t <- newMap
    -- FIXME: need finalizer for this guy too? =P
    return $ MiniGC t

newRef NoGC _ = undefined
newRef gc a = do
    --putStr "(new)"
    v <- newStablePtr a
    let v' = ptrToWordPtr $ castStablePtrToPtr v
    alter2 f v' (table gc)
    return v'
   where f Nothing = Just 1
         f (Just n) = Just (n+1)

freeRef NoGC v = return ()
freeRef gc v = do
    --putStr "(free? "
    alter2 f v (table gc)
    x <- member v (table gc)
    if x
        then return () --do { putStr "no!)"; return () }
        else freeStablePtr $ castPtrToStablePtr $ wordPtrToPtr v
        --else do { putStr "yes)"; freeStablePtr $ castPtrToStablePtr $ wordPtrToPtr v }
   where f Nothing = Nothing
         f (Just 1) = Nothing
         f (Just n) = Just (n-1)

swapGCs :: MiniGC -> MiniGC -> IO ()
swapGCs (MiniGC (Map j1)) (MiniGC (Map j2)) = do
    withForeignPtr j1 $ \p1 -> withForeignPtr j2 $ \p2 -> do
        v1 <- peek p1
        v2 <- peek p2
        poke p1 v2
        poke p2 v1

{- Special implementation of (Map Value Int) over JudyL for use in GC -}

-- FIXME: clean up a bit

data Map = Map { judy :: ForeignPtr JudyL } deriving (Eq, Ord, Typeable)

instance Show Map where
    show (Map j) = "<hsjudy gc internal map " ++ show j ++ ">"

newMap :: IO Map
newMap = do
    fp <- mallocForeignPtr
    addForeignPtrFinalizer judyL_free_ptr fp 
    withForeignPtr fp $ flip poke nullPtr
    return $ Map fp

insert :: Value -> Int -> Map -> IO ()
insert k v (Map j) = withForeignPtr j $ \j' -> do
    r <- judyLIns j' k judyError
    if r == pjerr
        then error "HsJudy: Not enough memory."
        else poke r (toEnum v)

alter2 :: (Maybe Int -> Maybe Int) -> Value -> Map -> IO ()
alter2 f k m@(Map j) = do
    j' <- withForeignPtr j peek
    r <- judyLGet j' k judyError
    if r == nullPtr
        then if (f Nothing) == Nothing
                then return ()
                else insert k (fromJust (f Nothing)) m
        else do
            v' <- peek r
            let v = (fromEnum v')
            let fv = (f (Just v))
            if fv == Nothing
                then delete k m >> return ()
                else poke r $ toEnum $ fromJust fv

lookup :: Value -> Map -> IO (Maybe Int)
lookup k (Map j) = do
    j' <- withForeignPtr j peek
    r <- judyLGet j' k judyError
    if r == nullPtr
        then return Nothing
        else do { v' <- peek r; return $ Just $ fromEnum v' }

member :: Value -> Map -> IO Bool
member k (Map j) = do
    j' <- withForeignPtr j peek
    r <- judyLGet j' k judyError
    return $ r /= nullPtr

delete :: Value -> Map -> IO Bool
delete k (Map j) = withForeignPtr j $ \j' -> do
    r <- judyLDel j' k judyError
    return $ r /= 0
