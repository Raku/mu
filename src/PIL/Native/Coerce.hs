{-# OPTIONS_GHC -fglasgow-exts #-}

module PIL.Native.Coerce where
import PIL.Native.Types
import Control.Arrow
import Control.Exception
import Data.Dynamic
import Data.Typeable
import qualified Data.Map as NMap
import qualified Data.Seq as NSeq
import qualified Data.FastPackedString as NStr

{-| 

PIL.Native.Coerce

This module defines the functions for use with the types defined 
in PIL.Native.Types. 

See Also: 

  PIL.Native.Types
  PIL.Native.Prims

-}

nil :: Native
nil = toNative mkNil

is_nil :: Native -> Bool
is_nil (NError {})  = True
is_nil _            = False

emptySeq :: Native
emptySeq = toNative (empty :: NativeSeq)

emptyMap :: Native
emptyMap = toNative (empty :: NativeMap)

mkNil :: NativeError
mkNil = NonTermination

mkErr :: (Typeable a) => a -> NativeError
mkErr = DynException . toDyn

mkSeq :: [b] -> SeqOf b
mkSeq = NSeq.fromList

mkMap :: [(String, a)] -> MapOf a
mkMap = NMap.fromList . map (\(k, v) -> (mkStr k, v))

mkStr :: String -> NativeStr
mkStr = NStr.pack

mkSub :: [String] -> [NativeLangExpression] -> NativeSub
mkSub params exps = MkSub
    { s_params = mkSeq (map mkStr params)
    , s_exps   = mkSeq exps
    , s_pad    = empty
    }

mkCall :: NativeLangExpression -> String -> [NativeLangExpression] -> NativeLangExpression
mkCall obj meth args = ECall
    { c_obj  = obj
    , c_meth = mkStr meth
    , c_args = mkSeq args
    }

class IsPlural a key val | a -> key, a -> val where 
    isEmpty     :: a -> NativeBit
    size        :: a -> NativeInt
    reversed    :: a -> a
    exists      :: a -> key -> Bool
    empty       :: a
    indices     :: a -> [key]
    elems       :: a -> [val]
    append      :: a -> a -> a
    push        :: a -> SeqOf val -> a
    assocs      :: a -> [(key, val)]
    fromAssocs  :: [(key, val)] -> a
    splice      :: a -> Int -> a
    fetch       :: a -> key -> Maybe val
    insert      :: a -> key -> val -> a
    (!)         :: a -> key -> val
    (!) x k = maybe (error "index out of bounds") id $ fetch x k

instance IsPlural NativeStr NativeInt NativeStr where
    isEmpty    = NStr.null
    size       = NStr.length
    empty      = NStr.empty
    exists (NStr.PS _ _ l) n = (n >= 0) && (n < l)
    indices    = \x -> [0 .. (NStr.length x - 1)]
    elems      = NStr.elems
    append     = NStr.append
    reversed   = NStr.reverse
    push       = \x xs -> NStr.concat (x:NSeq.toList xs)
    assocs     = zip [0..] . elems
    fromAssocs = NStr.concat . map snd -- XXX wrong
    splice     = flip NStr.drop
    fetch (NStr.PS p s l) n
        | n < 0     = fail "negative index"
        | n >= l    = fail "index out of bounds"
        | otherwise = return $ NStr.PS p (s + n) 1
    insert     = error "XXX str.insert"

instance Ord k => IsPlural (NMap.Map k v) k v where
    isEmpty    = NMap.null
    size       = NMap.size
    empty      = NMap.empty
    indices    = NMap.keys
    elems      = NMap.elems
    exists     = flip NMap.member
    append     = NMap.union
    push       = error "It doesn't make sense to push into a hash"
    splice     = error "It doesn't make sense to splice from a hash"
    reversed   = error "It doesn't make sense to reverse from a hash"
    assocs     = NMap.assocs
    fromAssocs = NMap.fromList
    fetch      = flip NMap.lookup
    insert     = \o k v -> NMap.insert k v o
    (!)        = (NMap.!)

instance IsPlural (SeqOf a) NativeInt a where
    isEmpty      = NSeq.null
    size         = NSeq.length
    empty        = NSeq.empty
    exists x n   = (n >= 0) && (n < size x)
    indices      = \x -> [0 .. size x - 1]
    elems        = NSeq.toList
    append       = (NSeq.><)
    push         = append
    reversed     = NSeq.reverse
    splice       = flip NSeq.drop
    assocs       = ([0..] `zip`) . elems
    fromAssocs   = NSeq.fromList . map snd -- XXX wrong
    fetch x k    | k >= size x = Nothing
                 | otherwise   = Just (NSeq.index x k)
    insert x k v | k == size x = (NSeq.|>) x v
                 | otherwise   = NSeq.update k v x
    (!)          = NSeq.index

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
    fromNative (NSub _)     = True
    fromNative (NObj _)     = True

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

{-
instance IsNative NativeSeq where
    toNative = NSeq
    fromNative (NError {})  = empty
    fromNative (NSeq x)     = x
    fromNative x            = castFail x
-}

instance IsNative NativeSub where
    toNative = NSub
    fromNative (NSub x)     = x
    fromNative x            = castFail x

instance IsNative NativeObj where
    toNative = NObj
    fromNative (NObj x)     = x
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
    fromNative = NSeq.toList . (fromNative :: Native -> NativeSeq)

instance IsNative [(Native, Native)] where
    toNative = NMap . NMap.fromList . map ((fromNative :: Native -> NativeStr) *** id) 
    fromNative = NMap.assocs . NMap.mapKeys (toNative :: NativeStr -> Native) . fromNative

instance IsNative [NativeStr] where
    toNative = (toNative :: NativeSeq -> Native) . mkSeq . map toNative
    fromNative = map fromNative . fromNative

instance (IsNative a) => IsNative (Maybe a) where
    toNative Nothing  = nil
    toNative (Just x) = toNative x
    fromNative (NError {}) = Nothing
    fromNative x           = Just (fromNative x)

instance (IsNative a) => IsNative (SeqOf a) where
    toNative = NSeq . fmap toNative
    fromNative (NError {})  = empty
    fromNative (NSeq x)     = fmap fromNative x
    fromNative x            = castFail x

instance IsNative [NativeObj] where
    toNative = NSeq . mkSeq . map toNative
    fromNative (NError {})  = []
    fromNative (NSeq x)     = elems $ fmap fromNative x
    fromNative x            = castFail x

castFail :: a -> b
castFail _ = error "cast fail"

failWith :: (Monad m, IsNative a) => String -> a -> m b
failWith msg s = fail $ msg ++ ": " ++ toString s
