{-# OPTIONS_GHC -fglasgow-exts -fallow-overlapping-instances #-}
module Pugs.Val.Str where
import Pugs.Internals
import qualified Data.ByteString.Char8 as Char8

instance ((:>:) String) ByteString where cast = Char8.unpack
instance ((:<:) String) ByteString where castBack = Char8.pack

newtype PureStr = MkStr ByteString
    deriving (Typeable, Show, Eq, Ord, Data, (:>:) String, (:<:) String, (:>:) ByteString, (:<:) ByteString)

{- regretfully, I couldn't find a way to put this here w/o circularity.
instance ICoercible P PureStr where
    asStr  = cast
-}

parseNum :: PureStr -> Double
parseNum (MkStr s) = fromIntegral i
    where (i, _) = maybe (0, undefined) id $ Char8.readInt s
