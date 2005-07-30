{-# OPTIONS_GHC -fglasgow-exts #-}

module PIL.Tie (
    Scalar, Array, Hash,
    Tie, TiedScalar, TiedArray, TiedHash,
) where
import PIL.Internals

data Tie a b
    = Tied (a b)
    | Untied (TVar b)

data TiedScalar a = MkTiedScalar
    { fetchS :: Eval a
    , storeS :: a -> Eval ()
    }

-- Stub interface; more to come
data TiedArray a = MkTiedArray
    { fetchA :: Eval a
    , storeA :: a -> Eval ()
    }

-- Stub interface; more to come
data TiedHash a = MkTiedHash
    { fetchH :: Eval a
    , storeH :: a -> Eval ()
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

