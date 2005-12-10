{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans #-}

module PIL.Native.Types (
    Native(..),
    NativeBit, NativeInt, NativeNum, NativeError, NativeStr, NativeSeq, NativeMap,

    NativeBlock(..), NativeLangExpression(..), NativeLangSym, NativeLangMethod, ArrayOf,
) where
import Data.Typeable
import Control.Exception
import qualified Data.Map as NMap
import qualified Data.Array.Diff as NSeq
import qualified Data.FastPackedString as NStr

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
    { nb_params :: !(ArrayOf NativeLangSym)
    , nb_body   :: !(ArrayOf NativeLangExpression)
    } 
    deriving (Show, Eq, Ord, Typeable)

type NativeLangSym = NativeStr
type NativeLangMethod = NativeStr

data NativeLangExpression
    = NL_Lit  !Native
    | NL_Var  !NativeLangSym
    | NL_Call { nl_obj  :: !NativeLangExpression
              , nl_meth :: !NativeLangMethod
              , nl_args :: !(ArrayOf NativeLangExpression)
              }
    deriving (Show, Eq, Ord, Typeable)


type NativeBit = Bool
type NativeInt = Int
type NativeNum = Float
type NativeError = Exception
type NativeStr = NStr.FastString
type NativeSeq = ArrayOf Native
type NativeMap = NMap.Map NativeStr Native
type ArrayOf = NSeq.DiffArray NativeInt

instance Ord NativeError where
    compare x y = compare (show x) (show y)

instance Eq a => Eq (ArrayOf a) where
    a == a'   =   NSeq.assocs a == NSeq.assocs a'

instance Ord a => Ord (ArrayOf a) where
    a <= a'   =   NSeq.assocs a <= NSeq.assocs a'
