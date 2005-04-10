{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans #-}

{-
    Implementation Types.

    Three Rings for the Elven-kings under the sky,
    Seven for the Dwarf-lords in their halls of stone,
    Nine for Mortal Men doomed to die,
    One for the Dark Lord on his dark throne
    In the Land of Mordor where the Shadows lie.
-}

module Types where

import {-# SOURCE #-} AST
import Internals
import qualified Types.Array  as Array
import qualified Types.Handle as Handle
import qualified Types.Hash   as Hash
import qualified Types.Scalar as Scalar
import qualified Types.Code   as Code
import qualified Types.Rule   as Rule
import qualified Data.HashTable as HTable

type IArray  = IORef [IVar VScalar]
type IArraySlice = [IVar VScalar]
type IHash   = HTable.HashTable VStr (IVar VScalar)
type IScalar = IORef Val
type ICode   = IORef VCode
data IHashEnv -- phantom types! fun!

-- these implementation allows no destructions
type IRule   = VRule
type IHandle = VHandle -- XXX maybe IORef?

instance Show IArray  where show _ = "{array}"
instance Show IHash   where show _ = "{hash}"
-- instance Show IHandle where show _ = "{handle}"
-- instance Show IScalar where show _ = "{scalar}"
-- instance Show ICode   where show _ = "{code}"
-- instance Show IRule   where show _ = "{rule}"

-- GADTs, here we come!
data VRef where
    MkRef   :: IVar a -> VRef

