module Judy.MiniGC (
    judyGC, newRef, freeRef
) where

import Data.Typeable
import Data.Maybe (fromJust)

import Foreign
import Foreign.Ptr
import Foreign.StablePtr

import Judy.Private

{-# NOINLINE judyGC #-}
judyGC = unsafePerformIO newGCMap

newRef a = do
    --putStr "(new)"
    v <- newStablePtr a
    let v' = ptrToWordPtr $ castStablePtrToPtr v
    alter f v' judyGC
    return v'
   where f Nothing = Just 1
         f (Just n) = Just (n+1)

freeRef v = do
    --putStr "(free? "
    alter f v judyGC
    x <- member v judyGC
    if x
        then return () --do { putStr "no!)"; return () }
        else freeStablePtr $ castPtrToStablePtr $ wordPtrToPtr v
        --else do { putStr "yes)"; freeStablePtr $ castPtrToStablePtr $ wordPtrToPtr v }
   where f Nothing = Nothing
         f (Just 1) = Nothing
         f (Just n) = Just (n-1)

{- Special implementation of (GCMap Value Int) over JudyL for use in GC -}

-- FIXME: clean up a bit

newtype GCMap = GCMap { judy :: ForeignPtr JudyL } deriving (Eq, Ord, Typeable)

instance Show GCMap where
    show (GCMap j) = "<hsjudy gc internal map " ++ show j ++ ">"

newGCMap :: IO GCMap
newGCMap = do
    fp <- mallocForeignPtr
    addForeignPtrFinalizer judyL_free_ptr fp 
    withForeignPtr fp $ flip poke nullPtr
    return $ GCMap fp

insert :: Value -> Int -> GCMap -> IO ()
insert k v (GCMap j) = withForeignPtr j $ \j' -> do
    r <- judyLIns j' k judyError
    if r == pjerr
        then error "HsJudy: Not enough memory."
        else poke r (toEnum v)

alter :: (Maybe Int -> Maybe Int) -> Value -> GCMap -> IO ()
alter f k m@(GCMap j) = do
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

lookup :: Value -> GCMap -> IO (Maybe Int)
lookup k (GCMap j) = do
    j' <- withForeignPtr j peek
    r <- judyLGet j' k judyError
    if r == nullPtr
        then return Nothing
        else do { v' <- peek r; return $ Just $ fromEnum v' }

member :: Value -> GCMap -> IO Bool
member k (GCMap j) = do
    j' <- withForeignPtr j peek
    r <- judyLGet j' k judyError
    return $ r /= nullPtr

delete :: Value -> GCMap -> IO Bool
delete k (GCMap j) = withForeignPtr j $ \j' -> do
    r <- judyLDel j' k judyError
    return $ r /= 0
