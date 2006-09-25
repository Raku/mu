{-# OPTIONS_GHC -fglasgow-exts -fallow-overlapping-instances -funbox-strict-fields #-}

module Pugs.Val.Base where

import qualified Data.ByteString.Char8 as Char8

import Pugs.Internals
import {-# SOURCE #-} Pugs.Val


-- Goal: associate each builtin type with prim methods (that handles native monotypes)
--       its metaclass then get those as the initial method slots
--       still have to maintain a list of builtin bootstrap classes somewhere
--       but everything else can be reflected via Coercible
--
-- Plan: Each prim type has a static fixed meta class

class (Monad m, Functor m, Typeable a) => ICoercible m a | a -> m where
    asBit    :: a -> m PureBit
    asBit _ = return $ cast True
    asInt    :: a -> m PureInt
    asInt x = fail $ "coerce fail: " ++ (show $ typeOf x) ++ " to PureInt"
    asNum    :: a -> m PureNum
    asNum x = fail $ "coerce fail: " ++ (show $ typeOf x) ++ " to PureNum"
    asStr    :: a -> m PureStr
    asStr _ = return (cast "<opaque>") -- XXX wrong
    -- "$item = VAL"
    asItem   :: a -> Maybe (m Val)
    asItem _ = Nothing -- default = do nothing (for Scalar this would return its content)
    -- "@list = VAL"
    asList   :: a -> Maybe (m PureList)
    asList _ = Nothing -- default = do nothing (for Scalar this would return its content wrapped in a 1-seq)
    asNative :: a -> m ValNative
    asNative = fmap (NBuf . cast) . asStr

type PureList       = Seq Val -- Seq (Either PureSeq PureRange) -- XXX - *very bogus*

-- PureBit

instance ((:>:) PureBit) Bool where cast = MkBit
instance ((:<:) PureBit) Bool where castBack (MkBit i) = i

newtype PureBit = MkBit Bool
    deriving (Typeable, Show, Eq, Ord, Data, (:>:) Bool, (:<:) Bool)

instance ICoercible P PureBit where asBit = return . cast

-- PureInt

instance ((:>:) PureInt) Integer where cast = IFinite
instance ((:<:) PureInt) Integer where
    castBack (IFinite i) = i
    castBack INotANumber = error "NaN"
    castBack (IInfinite SPositive) = error "+Infinity"
    castBack (IInfinite SNegative) = error "-Infinity"

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

instance ICoercible P PureInt where
    asInt = return . cast
    asStr INotANumber           = cast "NaN"
    asStr (IInfinite SPositive) = cast "Inf"
    asStr (IInfinite SNegative) = cast "-Inf"
    asStr (IFinite n)           = cast (show n)
    asNum INotANumber           = return $ cast ( (0/0) :: Double)
    asNum (IInfinite SPositive) = return $ cast ( (1/0) :: Double)
    asNum (IInfinite SNegative) = return $ cast ((-1/0) :: Double)
    asNum (IFinite   n)         = return $ cast ((fromIntegral n) :: Double)

-- PureNum

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

instance ICoercible P PureNum where asNum = return . cast

-- PureStr

newtype PureStr = MkStr ByteString deriving
    ( Typeable, Show, Eq, Ord, Data
    , (:>:) ID, (:<:) ID
    , (:>:) String, (:<:) String
    , (:>:) ByteString, (:<:) ByteString
    )

parseInt :: PureStr -> Int
parseInt (MkStr s) = maybe 0 fst (Char8.readInt s)

instance ICoercible P PureStr where
    asBit (MkStr s)
        | Char8.null s = return $ cast False
        | otherwise  = return $ cast (Char8.head s /= '0') -- 0x30
    asStr = cast
    asNum = cast . parseInt -- XXX - wrong
    asInt = cast . parseInt

