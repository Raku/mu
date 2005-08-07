{-# OPTIONS_GHC -fglasgow-exts #-}

module PIL.Container (
    Scalar, Array, Hash,
--  emptyHash, invokeTie, TieMethod(..),
    Tieable(..), Container, Name(..),
--  cmap, bmap, tmap, newMutBox,
) where
import PIL.Internals
import Data.Typeable
-- import PIL.MetaModel
import Data.Map as Map

newtype Name = MkName { unName :: String }
    deriving (Eq, Ord, Show, Typeable)

data Container
    = Scalar (TVar (Cell Scalar))  -- Scalar container
    | Array (TVar (Cell Array))    -- Array container
    | Hash (TVar (Cell Hash))      -- Hash container
    deriving (Typeable)

data Scalar = MkScalar Value
    deriving (Show, Eq, Typeable)

data Array = MkArray [Value]
    deriving (Show, Eq, Typeable)

data Hash = MkHash (Map Key Value)
    deriving (Show, Eq, Typeable)

type Key = Value
type Value = Int


{-|
'Cell' is either mutable (rebindable) or immutable, decided at compile time.

Tieable is orthogonal to mutableness; a constant tied container can still be
subject to @untie()@ and @tie()@.
-}
data (Typeable a) => Cell a
    = Con { cellId :: Id, cellCon :: a,      cellTie :: MaybeTied }
    | Mut { cellId :: Id, cellMut :: TVar a, cellTie :: MaybeTied }
    deriving (Typeable)

type MaybeTied = Maybe (TVar Tieable)


{-|
The type of tie-table must agree with the storage type.  Such a table
may be empty, as denoted by the nullary constructor 'Untied'.  Each of
the three storage types comes with its own tie-table layout.
-}
data Tieable = Untied | Tied Dynamic
    deriving (Eq, Ord, Show, Typeable)


{-

newMutBox :: (CellClass a) => a -> Maybe Tieable -> STM Container
newMutBox val pkg = do
    id  <- newId
    var <- newTVar val
    tie <- case pkg of
        Nothing -> return Nothing
        Just x  -> fmap Just (newTVar x)
    return . mkContainer $ Mut id var tie

newConBox :: (CellClass a) => a -> Maybe Tieable -> STM Container
newConBox val pkg = do
    id  <- newId
    tie <- case pkg of
        Nothing -> return Nothing
        Just x  -> fmap Just (newTVar x)
    return . mkContainer $ Con id val tie

class (Typeable a) => CellClass a where
    mkContainer :: Cell a -> Container

instance CellClass Hash where
    mkContainer = Hash

instance CellClass Array where
    mkContainer = Array

instance CellClass Scalar where
    mkContainer = Scalar

-- Invoke a tied function
invokeTie :: a -> b -> ST s ()
invokeTie _ _ = return ()

data TieMethod = FETCH | STORE | UNTIE

emptyHash = MkHash Map.empty

cmap :: (forall a. Typeable a => Cell a -> b) -> Container -> b
cmap f c = case c of
    Scalar x -> f x
    Array x  -> f x
    Hash x   -> f x

tmap :: Typeable a => (MaybeTied -> STM b) -> Cell a -> STM b
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


-- mkSym :: String -> Sym
-- showSym :: Sym -> String

-- | Sample Container: @%\*ENV@ is rw is HashEnv

{-
hashEnv :: STM Container
hashEnv = newMutBox emptyHash (Just (Tied (newObject "Hash::Env")))

hashNew :: STM Container
hashNew = do
    id  <- newId
    box <- newTVar $ MkBox id emptyHash
    return . Hash $ Mut box Nothing
-}

readId :: Container -> STM Id
readId = cmap (return cellId)

{-|
Compare two containers for Id equivalence.  If the container types differ, this
will never return True.
-}
(=:=) :: Container -> Container -> STM Bool
x =:= y = do
    ix <- readId x
    iy <- readId y
    return (ix == iy)

-}

instance Ord Dynamic where
    compare _ _ = EQ
instance Eq Dynamic where
    _ == _ = True

instance Show Container where
    show = error "cmap" -- cmap (show . typeOf)
instance Ord Container where
    compare _ _ = EQ
instance Eq Container where
    _ == _ = True

