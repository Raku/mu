{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans -fno-full-laziness -fno-cse -fno-warn-deprecations -fallow-undecidable-instances -fallow-overlapping-instances -funbox-strict-fields -fparr -cpp #-}

#ifndef HADDOCK
module Pugs.Internals.Cast (
    (:>:)(..),
    (:<:)(..),
    addressOf,
    showAddressOf,
    fromTypeable,
    _cast
) where

import Data.Dynamic hiding (cast)
import GHC.Exts (unsafeCoerce#, Word(W#), Word#)
import Data.ByteString (ByteString)
import Data.Sequence (Seq)
import Numeric (showHex)
import Data.Foldable (toList)
import GHC.PArr (fromP, toP, mapP)
import qualified Data.Sequence as Seq
import qualified Data.Typeable as Typeable
import qualified UTF8


--
-- Nominal subtyping relationship with widening cast.
-- 
-- The function "cast" is injective: for distinct values of "b",
-- it must produce distinct values of "a".
--
-- Also, it must work for all values of type "b".
-- 
class ((:>:) a) b where
    {-# SPECIALISE cast :: a -> a #-}
    {-# SPECIALISE cast :: ByteString -> ByteString #-}
    {-# SPECIALISE cast :: String -> ByteString #-}
    {-# SPECIALISE cast :: ByteString -> String #-}
    {-# SPECIALISE cast :: String -> String #-}
    cast :: b -> a

class ((:<:) a) b where
    castBack :: a -> b

instance (b :<: a) => (:>:) a b where
    cast = castBack

{-# INLINE _cast #-}
{-# SPECIALISE _cast :: String -> String #-}
{-# SPECIALISE _cast :: String -> ByteString #-}
_cast :: (a :>: String) => String -> a
_cast = cast

instance (:<:) a a where castBack = id

instance ((:>:) [a]) (Seq a) where cast = toList
instance ((:<:) [a]) (Seq a) where castBack = Seq.fromList
instance ((:>:) [a]) [:a:] where cast = fromP
instance ((:<:) [a]) [:a:] where castBack = toP

-- "map cast" can be written as "cast"
instance (a :>: b) => ((:>:) [a]) [b] where cast = map cast
instance (a :>: b) => ((:>:) [:a:]) [:b:] where cast = mapP cast

fromTypeable :: forall m a b. (Monad m, Typeable a, Typeable b) => a -> m b
fromTypeable x = case Typeable.cast x of
    Just y -> return y
    _      -> fail $ "Cannot cast from " ++ (show $ typeOf x) ++ " to " ++ (show $ typeOf (undefined :: b))

{-# INLINE addressOf #-}
addressOf :: a -> Word
addressOf x = W# (unsafeCoerce# x)

{-# INLINE showAddressOf #-}
showAddressOf :: String -> a -> String
showAddressOf typ x = addr `seq` ('<' : typ ++ ":0x" ++ showHex addr ">")
    where
    addr = addressOf x

instance ((:>:) String) ByteString where
    cast = UTF8.unpack
instance ((:<:) String) ByteString where
    castBack = UTF8.pack

instance ((:<:) ByteString) String where
    castBack = UTF8.unpack

#endif
