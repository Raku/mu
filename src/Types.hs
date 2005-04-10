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
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap

type IArray  = IORef [IScalar]
type IHash   = IORef (Map VStr IScalar)
type IScalar = IORef Val
type IScalarConst = Val
type IHandle  = IORef Handle
data IHashEnv = IHashEnv deriving Show

-- these implementation allows no destructions
type ICode   = VCode
type IRule   = VRule

instance Show IArray  where show _ = "{array}"
instance Show IHash   where show _ = "{hash}"
instance Show IHandle where show _ = "{handle}"
-- instance Show IScalar where show _ = "{scalar}"
-- instance Show ICode   where show _ = "{code}"
-- instance Show IRule   where show _ = "{rule}"

-- GADTs, here we come!
data VRef where
    MkRef   :: IVar a -> VRef

instance Scalar.Class IScalar where
    fetch = liftIO . readIORef
    store = (liftIO .) . writeIORef

-- Constants may be valid scalar as well
instance Scalar.Class IScalarConst where
    fetch = return
    store sv v = retError "Can't modify constant item" (Syn "=" [Val sv, Val v])

instance Hash.Class IHash where
instance Hash.Class IHashEnv where

instance Array.Class IArray where

instance Handle.Class IHandle where

instance Rule.Class IRule where

