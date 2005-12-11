{-# OPTIONS_GHC -fglasgow-exts #-}

module PIL.Native.Coerce where
import PIL.Native.Types
import Control.Arrow
import Control.Exception
import Data.Dynamic
import Data.Typeable
import qualified Data.Map as NMap
import qualified Data.Array.Diff as NSeq
import qualified Data.FastPackedString as NStr

nil :: Native
nil = toNative mkNil

mkNil :: NativeError
mkNil = NonTermination

mkErr :: (Typeable a) => a -> NativeError
mkErr = DynException . toDyn

mkSeq :: (NSeq.IArray a b) => [b] -> a Int b
mkSeq xs = NSeq.listArray (0, length xs - 1) xs

mkMap :: [(String, a)] -> MapOf a
mkMap = NMap.fromList . map (\(k, v) -> (mkStr k, v))

mkStr :: String -> NativeStr
mkStr = NStr.pack

mkBlock :: [String] -> [NativeLangExpression] -> NativeBlock
mkBlock params exps = MkBlock
    { nb_params = mkSeq (map mkStr params)
    , nb_body   = mkSeq exps
    }

mkCall :: NativeLangExpression -> String -> [NativeLangExpression] -> NativeLangExpression
mkCall obj meth args = NL_Call
    { nl_obj  = obj
    , nl_meth = mkStr meth
    , nl_args = mkSeq args
    }

class IsPlural a key val | a -> key, a -> val where 
    isEmpty     :: a -> NativeBit
    size        :: a -> NativeInt
    empty       :: a
    indices     :: a -> [key]
    elems       :: a -> [val]
    append      :: a -> a -> a
    assocs      :: a -> [(key, val)]
    fetch       :: a -> key -> val
    update      :: a -> key -> val -> a

instance IsPlural NativeStr NativeInt NativeStr where
    isEmpty = NStr.null
    size    = NStr.length
    empty   = NStr.empty
    indices = \x -> [0 .. (NStr.length x - 1)]
    elems   = NStr.elems
    append  = NStr.append
    assocs  = zip [0..] . elems

instance IsPlural NativeMap NativeStr Native where
    isEmpty = NMap.null
    size    = NMap.size
    empty   = NMap.empty
    indices = NMap.keys
    elems   = NMap.elems
    append  = NMap.union
    assocs  = NMap.assocs

instance IsPlural (SeqOf a) NativeInt a where
    isEmpty x  = (size x == 0)
    size x     = snd (NSeq.bounds x)
    empty      = NSeq.array (0, -1) []
    indices    = NSeq.indices
    elems      = NSeq.elems
    append x y = NSeq.listArray (0, size x + size y - 1) (elems x ++ elems y)
    assocs     = NSeq.assocs

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
    fromNative (NBit x)     = if x then mkStr "1" else mkStr "0"
    fromNative (NInt x)     = mkStr $ toString x
    fromNative (NNum x)     = mkStr $ toString x
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

instance IsNative NativeBlock where
    toNative = NBlock
    fromNative (NBlock x)   = x
    fromNative x            = castFail x

instance IsNative NativeError where
    toNative = NError
    fromNative (NError x)   = x
    fromNative x            = mkErr x

instance IsNative Integer where
    toNative = toNative . fromEnum
    fromNative = toEnum . fromNative

instance IsNative Double where
    toNative = (toNative :: NativeNum -> Native) . uncurry encodeFloat . decodeFloat
    fromNative = uncurry encodeFloat . decodeFloat . (fromNative :: Native -> NativeNum)

instance IsNative (Either Integer Double) where
    toNative = either toNative toNative
    fromNative (NNum x) = (Right . uncurry encodeFloat . decodeFloat) x
    fromNative n        = (Left . fromNative) n

instance IsNative String where
    toNative = toNative . mkStr
    fromNative = NStr.unpackFromUTF8 . fromNative

instance IsNative [Native] where
    toNative = NSeq . mkSeq
    fromNative = NSeq.elems . (fromNative :: Native -> NativeSeq)

instance IsNative [(Native, Native)] where
    toNative = NMap . NMap.fromList . map ((fromNative :: Native -> NativeStr) *** id) 
    fromNative = NMap.assocs . NMap.mapKeys (toNative :: NativeStr -> Native) . fromNative

instance IsNative [NativeStr] where
    toNative = (toNative :: NativeSeq -> Native) . mkSeq . map toNative
    fromNative = map fromNative . fromNative

castFail :: a -> b
castFail _ = error "cast fail"

