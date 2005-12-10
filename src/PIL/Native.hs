{-# OPTIONS_GHC -Wall -cpp -fglasgow-exts -fno-warn-orphans #-}

module PIL.Native where
import qualified Data.Map as NMap
import qualified Data.Array.Diff as NSeq
import qualified Data.FastPackedString as NStr
import Data.Typeable
import Control.Exception

#define _(x) (NStr.pack(x))

-- | Natives are "unboxed" runtime-allocated values.
data Native
    = NError    !NativeError
    | NBit      !NativeBit
    | NInt      !NativeInt
    | NNum      !NativeNum
--------------------------------- Aggregates
    | NStr      !NativeStr
    | NSeq      !NativeSeq
    | NMap      !NativeMap
--------------------------------- Block
    | NBlock    !NativeBlock
    | NOpaque   !NativeOpaque
    deriving (Show, Eq, Ord, Typeable)

data NativeBlock = MkBlock
    { params :: !(ArrayOf NativeBlockSym)
    , body   :: !(ArrayOf NativeBlockExpression)
    } 
    deriving (Show, Eq, Ord, Typeable)

data NativeOpaque = MkOpaque
    { ident  :: !NativeInt
    , klass  :: !NativeOpaque
    , attrs  :: !NativeMap
    }
    deriving (Show, Eq, Ord, Typeable)

type NativeBlockSym = NativeStr
type NativeBlockMsg = NativeStr

data NativeBlockExpression
    = NB_Lit  { val  :: Native }
    | NB_Var  { sym  :: NativeBlockSym }
    | NB_Send { obj  :: NativeBlockExpression
              , msg  :: NativeBlockMsg
              , args :: ArrayOf NativeBlockExpression
              }
    deriving (Show, Eq, Ord, Typeable)

-- Common API for plural types
class (IsNative a, IsNative key, IsNative val) => IsPlural a key val | a -> key, a -> val where 
    isEmpty     :: a -> NativeBit
    size        :: a -> NativeInt
    empty       :: a
    elems       :: a -> [val]
    assocs      :: a -> [(key, val)]

instance IsPlural NativeStr NativeInt NativeStr where
    isEmpty = NStr.null
    size    = NStr.length
    empty   = NStr.empty
    elems   = NStr.elems
    assocs  = zip [0..] . elems

instance IsPlural NativeMap NativeStr Native where
    isEmpty = NMap.null
    size    = NMap.size
    empty   = NMap.empty
    elems   = NMap.elems
    assocs  = NMap.assocs

instance IsPlural NativeSeq NativeInt Native where
    isEmpty x = (size x == 0)
    size x    = snd (NSeq.bounds x)
    empty     = NSeq.array (0,0) []
    elems     = NSeq.elems
    assocs    = NSeq.assocs

class Show a => IsNative a where 
    toNative   :: a -> Native
    fromNative :: Native -> a
    toString   :: a -> String
    toString   = show

instance IsNative Native where
    toNative   = id
    fromNative = id
    toString (NError x)   = toString x
    toString (NBit x)     = toString x
    toString (NInt x)     = toString x
    toString (NNum x)     = toString x
    toString (NStr x)     = toString x
    toString (NSeq x)     = toString x
    toString (NMap x)     = toString x
    toString (NBlock _)   = castFail

castFail :: (Typeable a) => a
castFail = error "Cannot case code to anything (XXX This message needs work)"

instance IsNative NativeBit where
    toNative = NBit
    fromNative (NError {})  = 0         -- Errors are undefs are false
    fromNative (NBit x)     = x
    fromNative (NInt x)     = (x /= 0)
    fromNative (NNum x)     = (x /= 0)
    fromNative (NStr x)     = case size x of
        0   -> 0
        1   -> (NStr.head x /= '0')
        _   -> 1
    fromNative (NSeq x)     = isEmpty x
    fromNative (NMap x)     = isEmpty x
    fromNative (NBlock _)   = 1         -- Code are always true

instance IsNative NativeInt where
    toNative = NInt
    fromNative (NError {})  = 0
    fromNative (NBit x)     = fromEnum x
    fromNative (NInt x)     = x
    fromNative (NNum x)     = fromEnum x
    fromNative (NStr x)     = read (toString x)
    fromNative (NSeq x)     = size x
    fromNative (NMap x)     = size x
    fromNative (NBlock _)   = castFail

instance IsNative NativeStr where
    toNative = NStr
    toString = NStr.unpackFromUTF8
    fromNative (NError {})  = empty
    fromNative (NBit x)     = if x then _("1") else _("0")
    fromNative (NInt x)     = _(toString x)
    fromNative (NNum x)     = _(toString x)
    fromNative (NStr x)     = x
    fromNative (NSeq x)     = NStr.unwords $ map fromNative (elems x)
    fromNative (NMap x)     = NStr.unlines $ map fromPair (assocs x)
        where
        fromPair (k, v) = NStr.append k (NStr.cons '\t' (fromNative v))
    fromNative (NBlock _)   = castFail

instance IsNative NativeNum where
    toNative = NNum
    fromNative (NError {})  = 0
    fromNative (NBit x)     = if x then 1 else 0
    fromNative (NInt x)     = toEnum x
    fromNative (NNum x)     = x
    fromNative (NStr x)     = read (toString x)
    fromNative (NSeq x)     = toEnum (size x)
    fromNative (NMap x)     = toEnum (size x)
    fromNative (NBlock _)   = castFail

instance IsNative NativeMap where
    toNative = NMap

instance IsNative NativeSeq where
    toNative = NSeq

instance IsNative NativeError where
    toNative = NError

type NativeBit = Bool
type NativeInt = Int
type NativeNum = Float
type NativeError = Exception
type NativeStr = NStr.FastString
type NativeSeq = ArrayOf Native
type NativeMap = NMap.Map NativeStr Native

type ArrayOf = NSeq.DiffArray NativeInt

instance Num NativeBit where
    False + False = False
    _     + _     = True
    x     - False = x
    _     - True  = False
    True  * True  = True
    _     * _     = False
    negate        = id
    abs           = id
    signum        = id
    fromInteger 0 = False
    fromInteger _ = True

instance Ord NativeError where
    compare x y = compare (show x) (show y)

instance Eq a => Eq (ArrayOf a) where
    a == a'   =   NSeq.assocs a == NSeq.assocs a'

instance Ord a => Ord (ArrayOf a) where
    a <= a'   =   NSeq.assocs a <= NSeq.assocs a'

