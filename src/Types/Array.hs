{-# OPTIONS_GHC -fglasgow-exts #-}

module Types.Array where

import {-# SOURCE #-} AST
import Internals

type Index = Int

class Class a where
    fetch       :: a -> Eval VArray
    fetch av = do
        size <- fetchSize av
        forM [0..size-1] $ \idx -> do
            sv <- fetchElem av idx
            readIVar sv
    store       :: a -> VArray -> Eval ()
    store av list = do
        forM_ ([0..] `zip` list) $ \(idx, val) -> do
            sv <- fetchElem av idx
            writeIVar sv val
        storeSize av (length list)
    fetchElem   :: a -> Index -> Eval (IVar VScalar) -- autovivify
    fetchElem av key = do
        return $ proxyScalar (fetchVal av key) (storeVal av key)
    storeElem   :: a -> Index -> IVar VScalar -> Eval () -- binding
    storeElem av idx sv = do
        val <- readIVar sv
        storeVal av idx val
    fetchVal    :: a -> Index -> Eval Val
    fetchVal av key = do
        rv <- existsElem av key
        if rv then readIVar =<< fetchElem av key
              else return undef
    storeVal    :: a -> Index -> Val -> Eval ()
    storeVal av key val = do
        sv <- fetchElem av key
        writeIVar sv val
    fetchSize   :: a -> Eval Index
    fetchSize av = do
        vals <- fetch av
        return $ length vals
    storeSize   :: a -> Index -> Eval ()
    storeSize av sz = do
        size <- fetchSize av
        case size `compare` sz of
            GT -> mapM_ (const $ pop av) [size .. sz-1]
            EQ -> return () -- no need to do anything
            LT -> mapM_ (\idx -> storeElem av idx =<< newScalar undef) [size .. sz-1]
    extendSize  :: a -> Index -> Eval ()
    extendSize av sz = do
        size <- fetchSize av
        when (size < sz) $ do
            mapM_ (\idx -> storeElem av idx =<< newScalar undef) [size .. sz-1]
    deleteElem  :: a -> Index -> Eval ()
    deleteElem av idx = do
        size <- fetchSize av
        case (size - 1) `compare` idx of
            GT -> return ()                             -- no such index
            EQ -> storeSize av (size - 1)               -- truncate
            LT -> storeElem av idx =<< newScalar undef  -- set to undef
    existsElem  :: a -> Index -> Eval VBool
    existsElem av idx = do
        size <- fetchSize av
        return $ size > idx
    clear       :: a -> Eval ()
    clear av = storeSize av 0
    push        :: a -> [Val] -> Eval ()
    push av vals = do
        size <- fetchSize av
        forM_ ([size..] `zip` vals) $ \(idx, val) -> do
            storeElem av idx =<< newScalar val
    pop         :: a -> Eval Val
    pop av = do
        size <- fetchSize av
        if size == 0
            then return undef
            else do
                sv <- fetchElem av $ size - 1
                storeSize av $ size - 1
                readIVar sv
    shift       :: a -> Eval Val
    shift av = do
        vals <- splice av 0 1 []
        return $ last (undef:vals)
    unshift     :: a -> [Val] -> Eval ()
    unshift av vals = do
        splice av 0 0 vals
        return ()
    splice      :: a -> Index -> Index -> [Val] -> Eval [Val]
    splice av off len vals = do
        size <- fetchSize av
        let off' = if off < 0 then off + size else off
            len' = if len < 0 then len + size - off else len
        result <- mapM (fetchElem av) [off' .. off' + len' - 1]
        let off = if off > size then size else off
            len = if off + len > size then size - off else len
            cnt = length vals
        case cnt `compare` len of
            GT -> do
                -- Move items up to make room
                let delta = cnt - len
                extendSize av (size + delta)
                (`mapM_` reverse [off + len .. size - 1]) $ \idx -> do
                    val <- fetchElem av idx
                    storeElem av (idx + delta) val
            LT -> do
                let delta = len - cnt
                (`mapM_` [off + len .. size - 1]) $ \idx -> do
                    val <- fetchElem av idx
                    storeElem av (idx - delta) val
                storeSize av (size - delta)
            _ -> return ()
        forM_ ([0..] `zip` vals) $ \(idx, val) -> do
            storeElem av (off + idx) =<< newScalar val
        mapM readIVar result
