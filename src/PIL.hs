{-# OPTIONS_GHC -fglasgow-exts -cpp #-}

module PIL where
import PIL.Tie
import PIL.Internals
import Prelude hiding (Num)

-- Pad maps symbols to containers
newtype Pad = MkPad (Map Sym Container)

-- $?CALLER::CALLER::SUB
data Sym = MkSym
    { symSigil      :: Sigil       -- $
    , symTwigil     :: Twigil      -- ?
    , symPackage    :: [Name]      -- [CALLER, CALLER]
    , symName       :: Name        -- SUB
    }
    deriving (Eq, Ord, Show, Typeable)

data Sigil
    = SigilScalar   -- $
    | SigilArray    -- @
    | SigilHash     -- %
    | SigilCode     -- &
    | SigilPackage  -- ::
    deriving (Eq, Ord, Show, Enum, Typeable)

data Twigil
    = TwigilNone
    | TwigilCompiled -- ?
    | TwigilGlobal   -- *
    | TwigilFile     -- =
    | TwigilImplicit -- ^
    | TwigilPublic   -- .
    | TwigilPrivate  -- :
    deriving (Eq, Ord, Show, Enum, Typeable)

-- mkSym :: String -> Sym
-- showSym :: Sym -> String

-- | Sample Container: @%\*ENV@ is rw is HashEnv
hashEnv :: STM Container
hashEnv = newMutBox emptyHash (Just (Tied (newObject "Hash::Env")))

{-
hashNew :: STM Container
hashNew = do
    id  <- newId
    box <- newTVar $ MkBox id emptyHash
    return . Hash $ Mut box Nothing
-}

readId :: Container -> STM Id
readId = cmap (bmap (return . boxId))

{-|
Compare two containers for Id equivalence.  If the container types differ, this
will never return True.
-}
(=:=) :: Container -> Container -> STM Bool
x =:= y = do
    ix <- readId x
    iy <- readId y
    return (ix == iy)

untie :: Container -> STM Val
untie = (>> return Void) . cmap (tmap $ maybe undefined (`writeTVar` Untied))

-- | Assign container @x@ to @y@
assign :: Container   -- ^ The @$x@ in @$x = $y@
       -> Container   -- ^ The @$y@ in @$x = $y@
       -> STM Val
assign = undefined

-- | Bind container @x@ to @y@
bind :: Container   -- ^ The @$x@ in @$x := $y@
     -> Container   -- ^ The @$y@ in @$x := $y@
     -> STM Val
bind = undefined

-- | Any PIL expression can only evaluate to one of three value results.
data Val
    = Void
    | Single Single
    | Plural [Single]
--  | Control Control
    deriving (Eq, Ord, Show, Typeable)

-- | 'Item' is either one of the five intrisic types, or an object.
data Single
    = Object Object
    -- Intrinsic types, according to S02.
    | Int Int | Num Num | Str Str | Ref Ref | Bit Bit
    -- These four below are apocryphal (not part of spec).
    | Pair Pair | Junc Junc | Type Type | Code Code 
    deriving (Eq, Ord, Show, Typeable)

-- | 'Ref' always points to a container; values are promoted to constant containers.
newtype Ref = MkRef { unRef :: Container }
    deriving (Eq, Ord, Show, Typeable)
data Pair = MkPair { pairKey :: Single, pairVal :: Container }
    deriving (Eq, Ord, Show, Typeable)

type Bit = Bool
type Str = String
type Num = Double
type Code = (Val -> Val) -- XXX
data Control = MkControl -- XXX
    deriving (Eq, Ord, Show, Typeable)
data Junc = MkJunc -- XXX
    deriving (Eq, Ord, Show, Typeable)

instance Show Container where
    show _ = "<container>"
instance Ord Container where
    compare _ _ = EQ
instance Eq Container where
    _ == _ = True


#ifdef ASD

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

-- | This should be fine: @untie(%ENV); %foo := %ENV@
testOk :: (%i::Id) => STM ()
testOk = do
    x <- hashNew
    y <- hashEnv
    untie y
    bind x y

-- | This should fail: @%foo := %ENV@
testFail :: (%i::Id) => STM ()
testFail = do
    x <- hashNew
    y <- hashEnv
    bind x y

testEquiv :: (%i::Id) => STM (Cell a) -> STM (Cell b) -> STM Bool
testEquiv x y = do
    x' <- x
    y' <- y
    (x' == y')

testBind :: (%i::Id) => STM (Cell a) -> STM (Cell a) -> STM ()
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

type GenContainer a = STM (Cell a)

class Evalable a b | a -> b where
    eval :: (%i :: Id) => a -> STM (Cell b)

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
    f :: STM Bool
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
    print =<< atomically (testEquiv hashEnv hashEnv)
    putStrLn "==> %ENV =:= %foo;"
    print =<< atomically (testEquiv hashEnv hashNew)
    putStrLn "==> %foo =:= %bar;"
    print =<< atomically (testEquiv hashNew hashNew)
    putStrLn "==> %foo := %bar;"
    print =<< atomically (testBind hashNew hashNew)
    putStrLn "==> %ENV := %ENV;"
    print =<< atomically (testBind hashEnv hashEnv)
    putStrLn "==> untie(%ENV); %foo := %ENV;"
    print =<< atomically (testBind hashNew $ do { env <- hashEnv; untie env; return env })
    putStrLn "==> %foo := %ENV;"
    print =<< atomically (testBind hashNew hashEnv)

#endif
