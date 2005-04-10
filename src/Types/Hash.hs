module Types.Hash where

import {-# SOURCE #-} AST
import Internals

type Index = VStr

class Class a where
    fetch       :: a -> Eval VHash
    fetch hv = do
        keys <- fetchKeys hv
        forM keys $ \key -> do
            sv  <- fetchElem hv key
            val <- readIVar sv
            return (key, val)
    store       :: a -> VHash -> Eval ()
    store hv vals = do
        clear hv
        forM_ vals $ \(key, val) -> do
            sv <- newScalar val
            storeElem hv key sv
    fetchElem   :: a -> Index -> Eval (IVar VScalar)
    storeElem   :: a -> Index -> IVar VScalar -> Eval ()
    fetchKeys   :: a -> Eval [Index]
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
