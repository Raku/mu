{-# OPTIONS_GHC -fglasgow-exts -fallow-overlapping-instances #-}
module Pugs.Val.Bit where
import Pugs.Internals
import qualified Data.ByteString.Char8 as Char8

instance ((:>:) PureBit) Bool where cast = MkBit
instance ((:<:) PureBit) Bool where castBack (MkBit i) = i

newtype PureBit = MkBit Bool
    deriving (Typeable, Show, Eq, Ord, Data, (:>:) Bool, (:<:) Bool)

{- regretfully, I couldn't find a way to put this here w/o circularity.
instance ICoercible P PureBit where
    asBit  = return . cast
-}


