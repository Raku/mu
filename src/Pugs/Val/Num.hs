{-# OPTIONS_GHC -fglasgow-exts -fallow-overlapping-instances #-}
module Pugs.Val.Num where
import Pugs.Internals
import qualified Data.ByteString.Char8 as Char8

instance ((:>:) PureNum) Double where cast = MkNum
instance ((:<:) PureNum) Double where castBack (MkNum i) = i

newtype PureNum = MkNum Double
    deriving (Typeable, Show, Eq, Ord, Data, (:>:) Double, (:<:) Double)

{- regretfully, I couldn't find a way to put this here w/o circularity.
instance ICoercible P PureNum where
    asNum  = return . cast
-}

