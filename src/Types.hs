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
import Data.Map     as Map
import Data.IntMap  as IntMap
import Types.Array  as Array
import Types.Handle as Handle
import Types.Hash   as Hash
import Types.Scalar as Scalar
import Types.Code   as Code
import Types.Rule   as Rule

type IArray  = IORef (IntMap IScalar)
type IHash   = IORef (Map VStr IScalar)
type IScalar = IORef Val
type IScalarConst = Val
type IHandle  = IORef Handle
data IHashEnv = IHashEnv deriving Show

-- these implementation allows no destructions
type ICode   = VSub
type IRule   = VRule

instance Show IArray  where show _ = "{array}"
instance Show IHash   where show _ = "{hash}"
instance Show IHandle where show _ = "{handle}"
-- instance Show IScalar where show _ = "{scalar}"
-- instance Show ICode   where show _ = "{code}"
-- instance Show IRule   where show _ = "{rule}"

-- GADTs, here we come!
data IVar where
    IScalar :: ScalarClass a => a -> IVar
    IArray  :: ArrayClass  a => a -> IVar
    IHash   :: HashClass   a => a -> IVar
    ICode   :: CodeClass   a => a -> IVar
    IHandle :: HandleClass a => a -> IVar
    IRule   :: RuleClass   a => a -> IVar

varClass (IScalar _) = "Scalar"
varClass (IArray _)  = "Array"
varClass (IHash _)   = "Hash"
varClass (ICode _)   = "Code"
varClass (IHandle _) = "Handle"
varClass (IRule _)   = "Rule"

instance ScalarClass IScalar where
    fetch = liftIO . readIORef
    store = (liftIO .) . writeIORef

-- Constants may be valid scalar as well
instance ScalarClass IScalarConst where
    fetch = return
    store sv v = retError "Can't modify constant item" (Syn "=" [Val sv, Val v])

instance HashClass IHash where
instance HashClass IHashEnv where

instance ArrayClass IArray where

instance HandleClass IHandle where

instance RuleClass IRule where


instance Eq IVar where
    (==) = const $ const True
instance Ord IVar where
    compare _ _ = EQ
instance Show IVar where
    show v = "<" ++ varClass v ++ ">"

