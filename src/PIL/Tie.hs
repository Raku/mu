{-# OPTIONS_GHC -fglasgow-exts #-}

module PIL.Tie (
    Scalar, Array, Hash,
    emptyHash, invokeTie, TieMethod(..), Tieable(..),
    Container, Name(..), Type(..), Box(..), Object, newObject,
    cmap, bmap, tmap, newMutBox,
) where
import PIL.Internals
import Data.Map as Map

newtype Name = MkName { unName :: String }
    deriving (Eq, Ord, Show, Typeable)
newtype Type = MkType { unType :: String }
    deriving (Eq, Ord, Show, Typeable)

data Container
    = Scalar (Cell Scalar)  -- Scalar container
    | Array (Cell Array)    -- Array container
    | Hash (Cell Hash)      -- Hash container

{-|
'Cell' is either mutable (rebindable) or immutable, decided at compile time.

Tieable is orthogonal to mutableness; a constant tied container can still be
subject to @untie()@ and @tie()@.
-}
data Cell a
    = Con { conBox  :: Box a,        tieable :: MaybeTied }
    | Mut { mutBox  :: TVar (Box a), tieable :: MaybeTied }

type MaybeTied = Maybe (TVar Tieable)
type Object = Dynamic

newObject :: (Typeable a) => a -> Dynamic
newObject = toDyn

instance Ord Object where
    compare _ _ = EQ
instance Eq Object where
    _ == _ = True

newMutBox :: (BoxClass a) => a -> Maybe Tieable -> STM Container
newMutBox val pkg = do
    id  <- newId
    box <- newTVar $ MkBox id val
    tie <- case pkg of
        Nothing -> return Nothing
        Just x  -> fmap Just (newTVar x)
    return . mkContainer $ Mut box tie

newConBox :: (BoxClass a) => a -> Maybe Tieable -> STM Container
newConBox val pkg = do
    id  <- newId
    tie <- case pkg of
        Nothing -> return Nothing
        Just x  -> fmap Just (newTVar x)
    return . mkContainer $ Con (MkBox id val) tie

class BoxClass a where
    mkContainer :: Cell a -> Container

instance BoxClass Hash where
    mkContainer = Hash

instance BoxClass Array where
    mkContainer = Array

instance BoxClass Scalar where
    mkContainer = Scalar

data Box a = MkBox { boxId :: Id, boxVal :: a }
    deriving (Eq, Ord, Show, Typeable)

{-|
The type of tie-table must agree with the storage type.  Such a table
may be empty, as denoted by the nullary constructor 'Untied'.  Each of
the three storage types comes with its own tie-table layout.
-}
data Tieable = Untied | Tied Object
    deriving (Eq, Ord, Show, Typeable)


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

cmap :: (forall a. Cell a -> b) -> Container -> b
cmap f c = case c of
    Scalar x -> f x
    Array x  -> f x
    Hash x   -> f x

bmap :: (forall a. Box a -> STM b) -> Cell a -> STM b
bmap f c = case c of
    Con con _ -> f con
    Mut mut _ -> f =<< readTVar mut

tmap :: (MaybeTied -> STM b) -> Cell a -> STM b
tmap f c = case c of
    Con _ t -> f t
    Mut _ t -> f t


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

