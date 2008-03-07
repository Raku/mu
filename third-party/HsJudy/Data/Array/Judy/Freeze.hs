module Data.Array.Judy.Freeze(Frozen(),Freezable(..)) where

import Data.Array.Judy.Private

class Freezable a where
    freeze :: a -> IO (Frozen a)
