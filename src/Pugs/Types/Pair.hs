{-# OPTIONS_GHC -fglasgow-exts #-}

module Pugs.Types.Pair where

import {-# SOURCE #-} Pugs.AST
import Pugs.Internals
import Pugs.Types

class (Typeable a) => Class a where
    iType :: a -> Type
    iType = const $ mkType "Pair"
    fetch :: a -> Eval VPair
    fetch pv = do
        key <- fetchKey pv
        val <- fetchVal pv
        return (key, val)
    fetchKey  :: a -> Eval VScalar
    fetchVal  :: a -> Eval VScalar
    fetchVal pv = do
        readIVar =<< fetchElem pv
    storeVal  :: a -> Val -> Eval ()
    storeVal pv val = do
        sv <- fetchElem pv
        writeIVar sv val
    fetchElem :: a -> Eval (IVar VScalar)
    fetchElem pv = do
        return $ proxyScalar (fetchVal pv) (storeVal pv)
