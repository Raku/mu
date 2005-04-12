{-# OPTIONS_GHC -fglasgow-exts #-}

module Types.Hash where

import {-# SOURCE #-} AST
import Internals
import Types

type Index = VStr

class (Typeable a) => Class a where
    iType :: a -> VStr
    iType _ = "Hash"
    fetch       :: a -> Eval VHash
    fetch hv = do
        keys <- fetchKeys hv
        vals <- mapM (fetchVal hv) keys
        return $ keys `zip` vals
    store       :: a -> VHash -> Eval ()
    store hv vals = do
        clear hv
        forM_ vals $ \(key, val) -> do
            storeVal hv key val
    fetchElem   :: a -> Index -> Eval (IVar VScalar) -- autovivify
    fetchElem hv key = do
        return $ proxyScalar (fetchVal hv key) (storeVal hv key)
    storeElem   :: a -> Index -> IVar VScalar -> Eval () -- binding
    storeElem hv idx sv = do
        val <- readIVar sv
        storeVal hv idx val
    fetchVal    :: a -> Index -> Eval Val
    fetchVal hv key = do
        rv <- existsElem hv key
        if rv then readIVar =<< fetchElem hv key
              else return undef
    storeVal    :: a -> Index -> Val -> Eval ()
    storeVal hv key val = do
        sv <- fetchElem hv key
        writeIVar sv val
    fetchKeys   :: a -> Eval [Index]
    fetchKeys hv = do
        pairs <- fetch hv
        return $ map fst pairs
    deleteElem  :: a -> Index -> Eval ()
    existsElem  :: a -> Index -> Eval VBool
    existsElem hv idx = do
        keys <- fetchKeys hv
        return $ idx `elem` keys
    clear       :: a -> Eval ()
    clear hv = do
        keys <- fetchKeys hv
        mapM_ (deleteElem hv) keys
    isEmpty     :: a -> Eval VBool
    isEmpty hv = do
        keys <- fetchKeys hv
        return $ null keys 
