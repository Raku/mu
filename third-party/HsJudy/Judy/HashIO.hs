{-# OPTIONS -fallow-undecidable-instances -fallow-incoherent-instances -fallow-overlapping-instances #-}

module Judy.HashIO (
    HashIO (..),
    UniqueHashIO, -- (..),
    ReversibleHashIO (..)
) where

import Data.HashTable (hashString)

import Judy.Private

class HashIO a where
    -- Two step conversion, first from a -> Int then Int -> Value
    hashIO :: a -> IO Value
class HashIO a => UniqueHashIO a
class UniqueHashIO a => ReversibleHashIO a where
    -- Two step conversion, first from Value -> Int then Int -> a
    unHashIO :: Value -> IO a


instance Enum a => UniqueHashIO a where
instance Enum a => HashIO a where
    hashIO = return . toEnum . fromEnum
instance Enum a => ReversibleHashIO a where
    unHashIO = return . toEnum . fromEnum


instance HashIO Value where
    hashIO = return
instance UniqueHashIO Value
instance ReversibleHashIO Value where
    unHashIO = return


instance HashIO Integer where
    hashIO = return . fromIntegral . hashString . show

