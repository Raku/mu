{-# OPTIONS_GHC -fglasgow-exts #-}

module PIL.Container (
    Scalar, Array, Hash,
    Tieable(..), Container, Name(..),
    cmap, newMut,
) where
import PIL.Internals
import Data.Typeable
-- import PIL.MetaModel
import Data.Map as Map

newtype Name = MkName { unName :: String }
    deriving (Eq, Ord, Show, Typeable)

data Container
    = ScalarCell (TVar (Cell Scalar))  -- Scalar container
    | ArrayCell (TVar (Cell Array))    -- Array container
    | HashCell (TVar (Cell Hash))      -- Hash container
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

class (Typeable a) => CellClass a where
    mkContainer :: Cell a -> STM Container

instance CellClass Hash where
    mkContainer = fmap HashCell . newTVar

instance CellClass Array where
    mkContainer = fmap ArrayCell . newTVar

instance CellClass Scalar where
    mkContainer = fmap ScalarCell . newTVar

newMut :: (%i::Id, CellClass a) => a -> Maybe Tieable -> STM Container
newMut val pkg = do
    var <- newTVar val
    tie <- case pkg of
        Nothing -> return Nothing
        Just x  -> fmap Just (newTVar x)
    mkContainer $ Mut %i var tie

newCon :: (%i::Id, CellClass a) => a -> Maybe Tieable -> STM Container
newCon val pkg = do
    tie <- case pkg of
        Nothing -> return Nothing
        Just x  -> fmap Just (newTVar x)
    mkContainer $ Con %i val tie

-- Invoke a tied function
invokeTie :: a -> b -> ST s ()
invokeTie _ _ = return ()

data TieMethod = FETCH | STORE | UNTIE

emptyHash :: Hash
emptyHash = MkHash Map.empty

cmap :: forall b. (Typeable a => Cell a -> STM b) -> Container -> STM b
cmap f t = case t of
    ScalarCell x -> doMap x
    ArrayCell x  -> doMap x
    HashCell x   -> doMap x
    where
    doMap :: Typeable a => (TVar (Cell a)) -> STM b
    doMap = (f =<<) . readTVar

-- | Sample Container: @%\*ENV@ is rw is HashEnv

hashEnv :: (%i::Id) => STM Container
hashEnv = newCon emptyHash tiedEnv
    where
    tiedEnv = Just (Tied (error "Hash::Env"))

hashNew :: (%i::Id) => STM Container
hashNew = newMut emptyHash Nothing

readId :: Container -> STM Id
readId = cmap (return . cellId)

----------------------------------------------------------------
-- QuickCheck instances

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

{-|
Compare two containers for Id equivalence.  If the container types differ, this
will never return True.
-}
(=:=) :: Container -> Container -> STM Bool
x =:= y = do
    ix <- readId x
    iy <- readId y
    return (ix == iy)

instance Ord Dynamic where
    compare _ _ = EQ
instance Eq Dynamic where
    _ == _ = True

instance Show Container where
    show (ScalarCell _) = "<scalar>"
    show (ArrayCell _)  = "<array>"
    show (HashCell _)   = "<hash>"
instance Ord Container where
    compare _ _ = EQ
instance Eq Container where
    _ == _ = True

