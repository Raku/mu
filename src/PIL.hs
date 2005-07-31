{-# OPTIONS_GHC -fglasgow-exts -cpp #-}

module PIL where
import PIL.Tie
import PIL.Internals
import GHC.Exts (Splittable(..))

{-|
'Container' comes in two flavours: Nontieable ('NCon') and Tieable
('TCon').  Once chosen, there is no way in runtime to revert this decision.
-}
data Container s a
    = NCon (STRef s (NBox a))
    | TCon (STRef s (TBox a))

{-|
A Non-tieable container is comprised of an Id and a storage of that type, which
can only be @Scalar@, @Array@ or @Hash@.  Again, there is no way to cast a
Scalar container into a Hash container at runtime.
-}
type NBox a = (Id, a)

{-|
A Tieable container also contains an Id and a storage, but also adds a
tie-table that intercepts various operations for its type.
-}
type TBox a = (Id, a, Tieable a)

{-|
The type of tie-table must agree with the storage type.  Such a table
may be empty, as denoted by the nullary constructor "Untied".  Each of
the three storage types comes with its own tie-table layout.
-}
#ifdef HADDOCK
data Tieable a = Untied | TieScalar TiedScalar | TieArray TiedArray | TieHash TiedHash
#else
data Tieable a where
    Untied     :: Tieable a
    TieScalar  :: TiedScalar -> Tieable Scalar
    TieArray   :: TiedArray  -> Tieable Array
    TieHash    :: TiedHash   -> Tieable Hash
#endif

-- | Sample TCon: @%\*ENV@
hashEnv :: ST s (Container s Hash)
hashEnv = fmap TCon $ newSTRef (-1, emptyHash, TieHash (tieHash emptyHash))

-- | Sample NCon: @%foo@
hashNew :: (%i :: Id) => ST s (Container s Hash)
hashNew = fmap NCon $ newSTRef (%i, emptyHash)

-- | Bind container @x@ to @y@
bind :: Container s a     -- ^ The @$x@ in @$x := $y@
     -> Container s a     -- ^ The @$y@ in @$x := $y@
     -> ST s ()
{-|
To bind a container to another, we first check to see if they are of the
same tieableness.  If so, we simply overwrite the target one's Id,
storage and tie-table (if any).
-}
bind (TCon x) (TCon y) = writeSTRef x =<< readSTRef y
bind (NCon x) (NCon y) = writeSTRef x =<< readSTRef y

{-|
To bind an non-tieable container to a tieable one, we implicitly remove
any current ties on the target, although it can be retied later:
-}
bind (TCon x) (NCon y) = do
    (id, val) <- readSTRef y
    writeSTRef x (id, val, Untied)
{-|
To bind a tieable container to a tied one, we first check if it is
actually tied.  If yes, we throw a runtime exception.  If not, we
proceed as if both were non-tieable.
-}
bind (NCon x) (TCon y) = do
    (id, val, tied) <- readSTRef y
    case tied of
        Untied -> writeSTRef x (id, val)
        _      -> fail "Cannot bind a tied container to a non-tieable one"

{-|
Compare two containers for Id equivalence.  If the container types differ, this
will never return True.
-}
(=:=) :: Container s a -> Container s b -> ST s Bool
x =:= y = do
    x_id <- readId x
    y_id <- readId y
    return (x_id == y_id)

-- | Read the Id field from a container
readId :: Container s a -> ST s Id
readId (NCon x) = fmap fst $ readSTRef x
readId (TCon x) = fmap (\(id, _, _) -> id) $ readSTRef x

-- | Untie a container
untie :: Container s a -> ST s ()
-- | Untie an non-tieable container is a no-op:
untie (NCon x) = return ()
-- | For a tieable container, we first invokes the "UNTIE" handler, then set
--   its "tied" slot to Untied:
untie (TCon x) = do
    (id, val, tied) <- readSTRef x
    case tied of
        Untied  -> return ()
        _       -> do
            tied `invokeTie` UNTIE
            writeSTRef x (id, val, Untied)

newtype Id = MkId Int
    deriving (Num, Eq, Ord, Show, Splittable)

instance Splittable Int where
    split x = (x, x+1)

-- | This should be fine: @untie(%ENV); %foo := %ENV@
testOk :: (%i::Id) => ST s ()
testOk = do
    x <- hashNew
    y <- hashEnv
    untie y
    bind x y

-- | This should fail: @%foo := %ENV@
testFail :: (%i::Id) => ST s ()
testFail = do
    x <- hashNew
    y <- hashEnv
    bind x y

testEquiv :: (%i::Id) => ST s (Container s a) -> ST s (Container s b) -> ST s Bool
testEquiv x y = do
    x' <- x
    y' <- y
    (x' =:= y')

testBind :: (%i::Id) => ST s (Container s a) -> ST s (Container s a) -> ST s ()
testBind x y = do
    x' <- x
    y' <- y
    bind x' y'

-- Extremely small language

data Exp
    = Bind LV Exp
    | Untie LV
    deriving (Show, Eq, Ord)

data LV
    = HashENV
    | HashNew
    deriving (Show, Eq, Ord)

type GenContainer a = forall s. ST s (Container s a)

class Evalable a b | a -> b where
    eval :: (%i :: Id) => a -> (forall s. ST s (Container s b))

instance Evalable Exp Hash where
    eval (Untie x) = do
        x' <- eval x
        untie x'
        return x'

instance Evalable LV Hash where
    eval HashNew = hashNew
    eval HashENV = hashEnv

instance Arbitrary LV where
    arbitrary = oneof (map return [HashENV, HashNew])
    coarbitrary = assert False undefined

prop_untie :: LV -> Bool
prop_untie x = try_ok (Untie x)

try_ok :: Evalable a b => a -> Bool
try_ok x = runST f
    where
    f :: forall s. ST s Bool
    f = do
        let %i = 0
        eval x
        return True

tests :: IO ()
tests = do
    let %i = 0
    putStrLn "==> Anything can be untied"
    test prop_untie
    putStrLn "==> %ENV =:= %ENV;"
    print $ runST (testEquiv hashEnv hashEnv)
    putStrLn "==> %ENV =:= %foo;"
    print $ runST (testEquiv hashEnv hashNew)
    putStrLn "==> %foo =:= %bar;"
    print $ runST (testEquiv hashNew hashNew)
    putStrLn "==> %foo := %bar;"
    print $ runST (testBind hashNew hashNew)
    putStrLn "==> %ENV := %ENV;"
    print $ runST (testBind hashEnv hashEnv)
    putStrLn "==> untie(%ENV); %foo := %ENV;"
    print $ runST (testBind hashNew $ do { env <- hashEnv; untie env; return env })
    putStrLn "==> %foo := %ENV;"
    print $ runST (testBind hashNew hashEnv)

