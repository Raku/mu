{-# OPTIONS_GHC -fglasgow-exts #-}

module PIL.Tie (
    Scalar, Array, Hash,
    emptyHash, invokeTie, TieMethod(..),
) where
import PIL.Internals
import Data.Map as Map

-- Invoke a tied function
invokeTie :: a -> b -> ST s ()
invokeTie _ _ = return ()

data TieMethod = FETCH | STORE | UNTIE

data Scalar = MkScalar Value
    deriving (Show, Eq)

data Array = MkArray [Value]
    deriving (Show, Eq)

data Hash = MkHash (Map Key Value)
    deriving (Show, Eq)

type Key = Value
type Value = Int

emptyHash = MkHash Map.empty

----------------------------------------------------------------
-- QuickCheck instances

-- instance Arbitrary Box where
--     arbitrary = oneof [ gen2 Scalar, gen2 Array, gen2 Hash ]
--     coarbitrary = assert False undefined

instance Arbitrary Scalar where
    arbitrary = gen1 MkScalar
    coarbitrary = assert False undefined
instance Arbitrary Array where
    arbitrary = gen1 MkArray
    coarbitrary = assert False undefined
instance Arbitrary Hash where
    arbitrary = gen1 MkHash
    coarbitrary = assert False undefined
instance (Ord a, Arbitrary a, Arbitrary b) => Arbitrary (Map a b) where
    arbitrary = fmap Map.fromList arbitrary
    coarbitrary = assert False undefined

