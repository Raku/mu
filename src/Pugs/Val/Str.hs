{-# OPTIONS_GHC -fglasgow-exts #-}
module Pugs.Val.Str where
import Pugs.Internals
import qualified Data.ByteString.Char8 as Char8

instance ((:>:) String) ByteString where cast = Char8.unpack
instance ((:<:) String) ByteString where castBack = Char8.pack

newtype PureStr = MkStr ByteString
    deriving (Typeable, Show, Eq, Ord, Data, (:>:) String, (:<:) String, (:>:) ByteString, (:<:) ByteString)

