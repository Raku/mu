{-# OPTIONS_GHC -fglasgow-exts -fallow-overlapping-instances #-}
module Pugs.Val.Int where
import Pugs.Internals
import qualified Data.ByteString.Char8 as Char8

instance ((:>:) PureInt) Integer where cast = IFinite
instance ((:<:) PureInt) Integer where
    castBack (IFinite i) = i
    castBack INotANumber = error "NaN"
    castBack (IInfinite SPositive) = error "+Infinity"
    castBack (IInfinite SNegative) = error "-Infinity"

-- ugh, duplicate from above? lose, lose.
instance ((:>:) PureInt) Int where cast = IFinite . toInteger 
instance ((:<:) PureInt) Int where
    castBack (IFinite i) = fromInteger i
    castBack INotANumber = error "NaN"
    castBack (IInfinite SPositive) = error "+Infinity"
    castBack (IInfinite SNegative) = error "-Infinity"

data Sign
    = SPositive
    | SNegative
    deriving (Show, Eq, Ord, Data, Typeable)

data PureInt
    = IFinite      !Integer
    | IInfinite    !Sign
    | INotANumber
    deriving (Typeable, Show, Eq, Ord, Data)

instance (:>:) Integer PureInt where
instance (:<:) Integer PureInt where


{- regretfully, I couldn't find a way to put this here w/o circularity.
instance ICoercible P PureInt where
    asInt  = return . cast
-}

