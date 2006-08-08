{-# OPTIONS_GHC -fglasgow-exts -fallow-overlapping-instances #-}
module Pugs.Val.Str where
import Pugs.Internals
import {-# SOURCE #-} Pugs.Val
import qualified Data.ByteString.Char8 as Char8

instance ((:>:) String) ByteString where cast = Char8.unpack
instance ((:<:) String) ByteString where castBack = Char8.pack

newtype PureStr = MkStr ByteString
    deriving (Typeable, Show, Eq, Ord, Data, (:>:) String, (:<:) String, (:>:) ByteString, (:<:) ByteString)

instance ICoercible P PureStr where
    asStr  = return . cast

class (Monad m, Functor m, Eq a, Data a, Typeable a) => ICoercible m a | a -> m where
    asBit    :: a -> m PureBit
    asBit _ = return True
    asInt    :: a -> m PureInt
    asInt x = fail $ "coerce fail: " ++ (show $ typeOf x) ++ " to PureInt"
    asNum    :: a -> m PureNum
    asNum x = fail $ "coerce fail: " ++ (show $ typeOf x) ++ " to PureNum"
    asStr    :: a -> m PureStr
    asStr x = return (cast "<opaque>") -- XXX wrong
    -- "$item = VAL"
    asItem   :: a -> Maybe (m Val)
    asItem _ = Nothing -- default = do nothing (for Scalar this would return its content)
    -- "@list = VAL"
    asList   :: a -> Maybe (m PureList)
    asList _ = Nothing -- default = do nothing (for Scalar this would return its content wrapped in a 1-seq)
    asNative :: a -> m ValNative
    asNative = fmap (NBuf . cast) . asStr

