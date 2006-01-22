{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans -funbox-strict-fields -cpp #-}

module PIL.Native.Types (
    Native(..), NativeObj(..),
    NativeBit, NativeInt, NativeNum, NativeError, NativeStr, NativeSeq, NativeMap,

    NativeSub(..), NativeLangExpression(..), NativeLangSym, NativeLangMethod,
    NativeLangCallType(..),

    ObjectId, ObjectPrim,
    
    SeqOf, MapOf, Pad,

    module Pugs.AST.SIO,
) where
import Pugs.AST.SIO
import Data.Typeable
import Control.Exception
import Control.Concurrent.STM

#ifndef HADDOCK
import {-# SOURCE #-} PIL.Repr
#endif

import qualified Data.Map as Map
import qualified Data.Seq as Seq
import qualified Data.FastPackedString as Str

{-| 

PIL.Native.Types

This module defines the core runtime types in Haskell. 

See Also: 

  PIL.Native.Coerce
  PIL.Native.Prims

-}

-- | Natives are "unboxed" runtime-allocated values.
data Native
    = NError    !NativeError
--------------------------------- Singulars
    | NBit      !NativeBit
    | NInt      !NativeInt
    | NNum      !NativeNum
    | NSub      !NativeSub
--------------------------------- Plurals
    | NStr      !NativeStr
    | NSeq      !NativeSeq
    | NMap      !NativeMap
--------------------------------- Objects
    | NObj      !NativeObj
    deriving (Show, Eq, Ord, Typeable)

type ObjectId    = NativeInt

instance Show NativeObj where
    show o = "<obj:#" ++ show (o_id o) ++ "|cls:#" ++ show (o_id (o_class o)) ++ ">"
instance Eq NativeObj where
    x == y = (o_id x) == (o_id y)
instance Ord NativeObj where
    compare x y = compare (o_id x) (o_id y)

type ObjectPrim = (NativeSeq -> STM Native)

data NativeObj = MkObject
    { o_id      :: ObjectId
    , o_class   :: NativeObj
    , o_repr    :: Repr
    }
    deriving (Typeable)

type Pad = NativeMap

data NativeSub = MkSub
    { s_params :: !(SeqOf NativeLangSym)
    , s_exps   :: !(SeqOf NativeLangExpression)
    , s_pad    :: !Pad
    } 
    deriving (Show, Eq, Ord, Typeable)

type NativeLangSym = NativeStr
type NativeLangMethod = NativeStr

data NativeLangCallType
    = CPrim | CPublic | CPrivate
    deriving (Show, Eq, Ord, Typeable)

data NativeLangExpression
    = ELit  !Native
    | EVar  !NativeLangSym
    | ECall { c_type :: !NativeLangCallType
            , c_obj  :: !NativeLangExpression
            , c_meth :: !NativeLangMethod
            , c_args :: !(SeqOf NativeLangExpression)
            }
    | ESaveContinuation -- XXX - save current continuation
    deriving (Show, Eq, Ord, Typeable)

type NativeBit = Bool
type NativeInt = Int
type NativeNum = Float
type NativeError = Exception
type NativeStr = Str.FastString
type NativeSeq = SeqOf Native
type NativeMap = Map.Map NativeStr Native

type MapOf = Map.Map NativeStr
type SeqOf = Seq.Seq

instance Ord NativeError where
    compare x y = compare (show x) (show y)

