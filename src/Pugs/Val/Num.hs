{-# OPTIONS_GHC -fglasgow-exts -fallow-overlapping-instances #-}
module Pugs.Val.Num where
import Pugs.Internals
import qualified Data.ByteString.Char8 as Char8

instance ((:>:) PureNum) Double where cast = NDouble
instance ((:<:) PureNum) Double where
    castBack (NDouble   x) = x
    castBack (NRational x) = fromRational x

instance ((:>:) PureNum) Rational where cast = NRational
instance ((:<:) PureNum) Rational where
    castBack (NDouble   x) = toRational x
    castBack (NRational x) = x

instance ((:>:) PureNum) Int where cast = NRational . fromIntegral

data PureNum
    = NDouble   !Double              -- change to "!NativeDouble"
    | NRational !Rational
    deriving (Typeable, Show, Eq, Ord, Data)

{- regretfully, I couldn't find a way to put this here w/o circularity.
instance ICoercible P PureNum where
    asNum  = return . cast
-}

