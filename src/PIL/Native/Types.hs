{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans -funbox-strict-fields #-}

module PIL.Native.Types (
    Native(..), NativeObj(..),
    NativeBit, NativeInt, NativeNum, NativeError, NativeStr, NativeSeq, NativeMap,

    NativeSub(..), NativeLangExpression(..), NativeLangSym, NativeLangMethod,

    ObjectId, ObjectAttrs,
    
    SeqOf, MapOf, Pad,

    module Pugs.AST.SIO,
) where
import Pugs.AST.SIO
import Data.Typeable
import Control.Exception
import Control.Concurrent.STM
import qualified Data.Map as NMap
import qualified Data.Seq as NSeq
import qualified Data.FastPackedString as NStr

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
type ObjectAttrs = TVar NativeMap

instance Show ObjectAttrs where
    show _ = "<opaque>"

instance Ord NativeObj where
    compare x y = compare (o_id x) (o_id y)

data NativeObj = MkObject
    { o_id      :: !ObjectId
    , o_class   :: NativeObj -- ::Class is self-recursive, so can't be strict here
    , o_attrs   :: !ObjectAttrs
    }
    deriving (Show, Eq, Typeable)

type Pad = NativeMap

data NativeSub = MkSub
    { s_params :: !(SeqOf NativeLangSym)
    , s_exps   :: !(SeqOf NativeLangExpression)
    , s_pad    :: !Pad
    } 
    deriving (Show, Eq, Ord, Typeable)

type NativeLangSym = NativeStr
type NativeLangMethod = NativeStr

data NativeLangExpression
    = ELit  !Native
    | EVar  !NativeLangSym
    | ECall { c_obj  :: !NativeLangExpression
            , c_meth :: !NativeLangMethod
            , c_args :: !(SeqOf NativeLangExpression)
            }
    deriving (Show, Eq, Ord, Typeable)

type NativeBit = Bool
type NativeInt = Int
type NativeNum = Float
type NativeError = Exception
type NativeStr = NStr.FastString
type NativeSeq = SeqOf Native
type NativeMap = NMap.Map NativeStr Native

type MapOf = NMap.Map NativeStr
type SeqOf = NSeq.Seq

instance Ord NativeError where
    compare x y = compare (show x) (show y)

