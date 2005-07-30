{-# OPTIONS_GHC -fglasgow-exts -cpp #-}

module PIL where
import PIL.Tie
import PIL.Internals

-- | 'Container' comes in two flavours: Untieable ('UCon') and Tieable
--   ('TCon').  Once chosen, there is no way in runtime to revert this decision.
data Container s a
    = UCon (UBox s a)
    | TCon (TBox s a)

-- | 'UBox' is a typed, mutable reference, comprised of an Id and a
--   storage of that type, which can only be @Scalar@, @Array@ or @Hash@.
--   Again, there is no way to cast a 'UBox' into another type at runtime.

type BoxId = Int
type UBox s a = STRef s (BoxId, a)

-- | 'TBox' is like 'UBox', but with an additional field that may
--   contain a dispatch table that intercepts various operations.
type TBox s a = STRef s (BoxId, a, Tieable a)

#ifndef HADDOCK
data Tieable a where
    Untied     :: Tieable a
    TieScalar  :: TiedScalar -> Tieable Scalar
    TieArray   :: TiedArray  -> Tieable Array
    TieHash    :: TiedHash   -> Tieable Hash
#endif

-- | Sample TCon: @%\*ENV@
hashEnv :: ST s (Container s Hash)
hashEnv = fmap TCon $ newSTRef (0, emptyHash, TieHash (tieHash emptyHash))

-- | Sample UCon: @%foo@
hashFoo :: ST s (Container s Hash)
hashFoo = fmap UCon $ newSTRef (0, emptyHash)

-- | Bind container @x@ to @y@
bind :: Container s a     -- ^ The @$x@ in @$x := $y@
     -> Container s a     -- ^ The @$y@ in @$x := $y@
     -> ST s ()
bind (UCon x) (UCon y) = writeSTRef x =<< readSTRef y
bind (TCon x) (TCon y) = writeSTRef x =<< readSTRef y
bind (UCon x) (TCon y) = do
    (id, val, tied) <- readSTRef y
    case tied of
        Untied -> writeSTRef x (id, val)
        _      -> fail "Cannot bind a tied container into a non-tieable one"
bind (TCon x) (UCon y) = do
    (id, val) <- readSTRef y
    writeSTRef x (id, val, Untied)

-- | Untie a container
untie :: Container s a -> ST s ()
untie (UCon x) = return ()
untie (TCon x) = modifySTRef x (\(id, val, _) -> (id, val, Untied))

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

tests = do
    putStrLn "==> untie(%ENV); my %foo := %ENV;"
    print $ runST testOk
    putStrLn "==> my %foo := %ENV;"
    print $ runST testFail

