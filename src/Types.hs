{-# OPTIONS_GHC -fglasgow-exts #-}

{-
    Implementation Types.

    Three Rings for the Elven-kings under the sky,
    Seven for the Dwarf-lords in their halls of stone,
    Nine for Mortal Men doomed to die,
    One for the Dark Lord on his dark throne
    In the Land of Mordor where the Shadows lie.
-}

module Types where
import AST
import Internals
import Data.Map     as Map
import Data.IntMap  as IntMap
import Types.Array  as Array
import Types.Handle as Handle
import Types.Hash   as Hash
import Types.Scalar as Scalar

type IArray  = IORef (IntMap IScalar)
type IHash   = IORef (Map VStr IScalar)
type IScalar = IORef Val

instance Show IArray  where show _ = "{array}"
instance Show IHash   where show _ = "{hash}"
-- instance Show IScalar where show _ = "{scalar}"

-- GADTs, here we come!
data IVar where
    IScalar :: ScalarClass a => a -> IVar
    IArray  :: ArrayClass  a => a -> IVar
    IHash   :: HashClass   a => a -> IVar
    IHandle :: HandleClass a => a -> IVar
    IConst  :: Val -> IVar

instance ScalarClass IScalar where
instance HashClass IHash where

