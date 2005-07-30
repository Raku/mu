{-# OPTIONS_GHC -fglasgow-exts #-}

module PIL.Tie (
    Scalar, Array, Hash,
    TiedScalar, TiedArray, TiedHash,
    emptyHash, tieHash,
) where
import PIL.Internals

data TiedScalar = MkTiedScalar
    { fetchS :: forall s. ST s Scalar
    , storeS :: forall s. Scalar -> ST s ()
    }

-- Stub interface; more to come
data TiedArray = MkTiedArray
    { fetchA :: forall s. ST s Array
    , storeA :: forall s. Array -> ST s ()
    }

-- Stub interface; more to come
data TiedHash = MkTiedHash
    { fetchH :: forall s. ST s Hash
    , storeH :: forall s. Hash -> ST s ()
    }

data Scalar = MkScalar Value
    deriving (Show, Eq)

data Array = MkArray [Value]
    deriving (Show, Eq)

data Hash = MkHash [(Key, Value)]
    deriving (Show, Eq)

type Key = Value
type Value = Int

emptyHash = MkHash []
tieHash h = MkTiedHash (return h) (const $ return ())

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

