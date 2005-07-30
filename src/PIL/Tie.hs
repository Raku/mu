{-# OPTIONS_GHC -fglasgow-exts #-}

module PIL.Tie (
    Scalar, Array, Hash,
    TiedScalar, TiedArray, TiedHash,
) where
import PIL.Internals

data TiedScalar = MkTiedScalar
    { fetchS :: Eval Scalar
    , storeS :: Scalar -> Eval ()
    }

-- Stub interface; more to come
data TiedArray = MkTiedArray
    { fetchA :: Eval Array
    , storeA :: Array -> Eval ()
    }

-- Stub interface; more to come
data TiedHash = MkTiedHash
    { fetchH :: Eval Hash
    , storeH :: Hash -> Eval ()
    }

data Scalar = MkScalar Value
    deriving (Show, Eq)

data Array = MkArray [Value]
    deriving (Show, Eq)

data Hash = MkHash [(Key, Value)]
    deriving (Show, Eq)

type Key = Value
type Value = Int

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

