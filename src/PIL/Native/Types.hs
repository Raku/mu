{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans #-}

module PIL.Native.Types (
    Native(..),
    NativeBit, NativeInt, NativeNum, NativeError, NativeStr, NativeSeq, NativeMap,

    NativeBlock(..), NativeLangExpression(..), NativeLangSym, NativeLangMethod,
    
    SeqOf, MapOf,
) where
import Data.Typeable
import Control.Exception
import Data.FunctorM
import qualified Data.Map as NMap
import qualified Data.Array.Diff as NSeq
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
    | NBlock    !NativeBlock
--------------------------------- Plurals
    | NStr      !NativeStr
    | NSeq      !NativeSeq
    | NMap      !NativeMap
    deriving (Show, Eq, Ord, Typeable)

data NativeBlock = MkBlock
    { nb_params :: !(SeqOf NativeLangSym)
    , nb_body   :: !(SeqOf NativeLangExpression)
    } 
    deriving (Show, Eq, Ord, Typeable)

type NativeLangSym = NativeStr
type NativeLangMethod = NativeStr

data NativeLangExpression
    = NL_Lit  !Native
    | NL_Var  !NativeLangSym
    | NL_Call { nl_obj  :: !NativeLangExpression
              , nl_meth :: !NativeLangMethod
              , nl_args :: !(SeqOf NativeLangExpression)
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
type SeqOf = NSeq.DiffArray NativeInt

instance Ord NativeError where
    compare x y = compare (show x) (show y)

instance Eq a => Eq (SeqOf a) where
    a == a'   =   NSeq.assocs a == NSeq.assocs a'

instance Ord a => Ord (SeqOf a) where
    a <= a'   =   NSeq.assocs a <= NSeq.assocs a'

instance FunctorM SeqOf where
    fmapM f a = sequence [ f e >>= return . (,) i | (i,e) <- NSeq.assocs a] >>= return . NSeq.array b
        where
        b = NSeq.bounds a

