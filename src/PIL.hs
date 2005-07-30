{-# OPTIONS_GHC -fglasgow-exts -cpp #-}

module PIL where
import PIL.Tie
import PIL.Internals

{-|
'Container' comes in two flavours: Nontieable ('NCon') and Tieable
('TCon').  Once chosen, there is no way in runtime to revert this decision.
-}
data Container s a
    = NCon (STRef s (NBox a))
    | TCon (STRef s (TBox a))

type BoxId = Int

{-|
A Non-tieable container is comprised of an Id and a storage of that type, which
can only be @Scalar@, @Array@ or @Hash@.  Again, there is no way to cast a
Scalar container into a Hash container at runtime.
-}
type NBox a = (BoxId, a)

{-|
A Tieable container also contains an Id and a storage, but also adds a
tie-table that intercepts various operations for its type.
-}
type TBox a = (BoxId, a, Tieable a)

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
hashEnv = fmap TCon $ newSTRef (0, emptyHash, TieHash (tieHash emptyHash))

-- | Sample NCon: @%foo@
hashFoo :: ST s (Container s Hash)
hashFoo = fmap NCon $ newSTRef (1, emptyHash)

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
readId :: Container s a -> ST s BoxId
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

-- | This should be fine: @untie(%ENV); %foo := %ENV@
testOk :: ST s ()
testOk = do
    x <- hashFoo
    y <- hashEnv
    untie y
    bind x y

-- | This should fail: @%foo := %ENV@
testFail :: ST s ()
testFail = do
    x <- hashFoo
    y <- hashEnv
    bind x y

testEquiv :: ST s (Container s a) -> ST s (Container s b) -> ST s Bool
testEquiv x y = do
    x' <- x
    y' <- y
    (x' =:= y')

tests = do
    putStrLn "==> %ENV =:= %ENV;"
    print $ runST (testEquiv hashEnv hashEnv)
    putStrLn "==> %ENV =:= %foo;"
    print $ runST (testEquiv hashEnv hashFoo)
    putStrLn "==> untie(%ENV); my %foo := %ENV;"
    print $ runST testOk
    putStrLn "==> my %foo := %ENV;"
    print $ runST testFail

