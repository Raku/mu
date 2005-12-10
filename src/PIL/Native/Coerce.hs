{-# OPTIONS_GHC -fglasgow-exts #-}

module PIL.Native.Coerce where
import PIL.Native.Types
import Data.Typeable
import Data.Dynamic
import Control.Exception
import qualified Data.Map as NMap
import qualified Data.Array.Diff as NSeq
import qualified Data.FastPackedString as NStr

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
    toString x            = castFail x

instance IsNative NativeBit where
    toNative = NBit
    fromNative (NError {})  = False      -- Errors are undefs are false
    fromNative (NBit x)     = x
    fromNative (NInt x)     = (x /= 0)
    fromNative (NNum x)     = (x /= 0)
    fromNative (NStr x)     = case size x of
        0   -> False
        1   -> (NStr.head x /= '0')
        _   -> True
    fromNative (NSeq x)     = isEmpty x
    fromNative (NMap x)     = isEmpty x
    fromNative (NBlock _)   = True       -- Code are always true

instance IsNative NativeInt where
    toNative = NInt
    fromNative (NError {})  = 0
    fromNative (NBit x)     = fromEnum x
    fromNative (NInt x)     = x
    fromNative (NNum x)     = fromEnum x
    fromNative (NStr x)     = read (toString x)
    fromNative (NSeq x)     = size x
    fromNative (NMap x)     = size x
    fromNative x            = castFail x

instance IsNative NativeStr where
    toNative = NStr
    toString = NStr.unpackFromUTF8
    fromNative (NError {})  = empty
    fromNative (NBit x)     = if x then NStr.pack "1" else NStr.pack "0"
    fromNative (NInt x)     = NStr.pack $ toString x
    fromNative (NNum x)     = NStr.pack $ toString x
    fromNative (NStr x)     = x
    fromNative (NSeq x)     = NStr.unwords $ map fromNative (elems x)
    fromNative (NMap x)     = NStr.unlines $ map fromPair (assocs x)
        where
        fromPair (k, v) = NStr.append k (NStr.cons '\t' (fromNative v))
    fromNative x            = castFail x

instance IsNative NativeNum where
    toNative = NNum
    fromNative (NError {})  = 0
    fromNative (NBit x)     = if x then 1 else 0
    fromNative (NInt x)     = toEnum x
    fromNative (NNum x)     = x
    fromNative (NStr x)     = read (toString x)
    fromNative (NSeq x)     = toEnum (size x)
    fromNative (NMap x)     = toEnum (size x)
    fromNative x            = castFail x

instance IsNative NativeMap where
    toNative = NMap
    fromNative (NError {})  = empty
    fromNative (NMap x)     = x
    fromNative x            = castFail x

instance IsNative NativeSeq where
    toNative = NSeq
    fromNative (NError {})  = empty
    fromNative (NSeq x)     = x
    fromNative x            = castFail x

instance IsNative NativeError where
    toNative = NError
    fromNative (NError x)   = x
    fromNative x            = err x

castFail :: a -> b
castFail _ = error "cast fail"

err :: (Typeable a) => a -> NativeError
err = DynException . toDyn
