{-# OPTIONS_GHC -fglasgow-exts #-}

module PIL.Internals (
    Arbitrary(..),
    gen1, gen2, assert,
    newSTRef, readSTRef, writeSTRef, modifySTRef, runST, ST, STRef,
) where
import Control.Monad.ST
import Data.STRef
import Test.QuickCheck
import Control.Exception

gen1 :: (Arbitrary a) => (a -> b) -> Gen b
gen1 = (`fmap` arbitrary)

gen2 :: (Arbitrary a, Arbitrary b) => (a -> b -> c) -> Gen c
gen2 f = do
    x <- arbitrary
    y <- arbitrary
    return $ f x y
