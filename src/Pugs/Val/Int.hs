{-# OPTIONS_GHC -fglasgow-exts -fallow-overlapping-instances #-}
module Pugs.Val.Int where
import Pugs.Internals
import qualified Data.ByteString.Char8 as Char8

instance ((:>:) PureInt) Integer where cast = MkInt
instance ((:<:) PureInt) Integer where castBack (MkInt i) = i

newtype PureInt = MkInt Integer
    deriving (Typeable, Show, Eq, Ord, Data, (:>:) Integer, (:<:) Integer)

{- regretfully, I couldn't find a way to put this here w/o circularity.
instance ICoercible P PureInt where
    asInt  = return . cast
-}

