module Judy.Freeze(Frozen(),Freezable(..)) where

import Judy.Private

class Freezable a where
    freeze :: a -> IO (Frozen a)
