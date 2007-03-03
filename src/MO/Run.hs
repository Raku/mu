{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances -fparr #-}

module MO.Run (
    module MO.Run,
    module MO.Base
) where

-- FIXME: systematize a nice order for imports (steal Pugs')
import MO.Util
import MO.Base
import MO.Compile as C
import Data.Map as M
import Data.Typeable hiding (cast)
import GHC.PArr
import qualified Data.Typeable as Typeable

mkArgs :: (Typeable1 m, Monad m) => [Invocant m] -> Arguments m
mkArgs x = CaptSub{ c_feeds = [: MkFeed { f_positionals = toP x, f_nameds = M.empty } :] }

-- Abstract Roles
-- class Invocation a
-- class Responder a

data MethodInvocation m
    = MkMethodInvocation
        { miName      :: !MethodName  
        , miArguments :: !(Arguments m)
        }

-- instance Invocation (MethodInvocation m)

-- | This is a static method table.
data MethodTable m
    = MkMethodTable
        { mtMethods :: !(M.Map MethodName (MethodCompiled m))
        }

emptyResponder :: (Typeable1 m, Monad m) => AnyResponder m
emptyResponder = MkResponder (return NoResponse)

data Monad m => NoResponse m = NoResponse

instance Monad m => ResponderInterface m (NoResponse m) where
    dispatch _ _ _      = fail "Dispatch failed - NO CARRIER"
    fromMethodList _    = return NoResponse
    toNameList _        = []

__ :: (Typeable1 m, Monad m, Ord a, Show a, Typeable a) => a -> Invocant m
__ = (`MkInvocant` emptyResponder)

stubInvocant :: (Typeable1 m, Monad m) => Invocant m
stubInvocant = MkInvocant () emptyResponder

data AnyResponder m = forall c. ResponderInterface m c => MkResponder !(m c)
data AnyResponder_Type deriving Typeable

instance (Typeable1 m, Monad m) => Typeable (AnyResponder m) where
    typeOf _ = typeOf (undefined :: m AnyResponder_Type)

class Monad m => ResponderInterface m a | a -> m where
    fromMethodList :: [(MethodName, MethodCompiled m)] -> m a
    dispatch :: a -> Invocant m -> MethodInvocation m -> m (Invocant m)
    -- here for debugging purposes.
    toNameList :: a -> [MethodName]

instance ResponderInterface m a => Show a where
    show = show . toNameList

instance (Typeable1 m, Monad m) => ResponderInterface m (MethodTable m) where
    fromMethodList = return . MkMethodTable . M.fromList
    dispatch mt responder inv@(MkMethodInvocation _ args) = do
        method_compiled <- mtMethod mt inv
        runMC method_compiled (withInvocant args responder)
    toNameList = M.keys . mtMethods

mtMethod :: Monad m => MethodTable a -> MethodInvocation m -> m (MethodCompiled a)
mtMethod table (MkMethodInvocation n _) = case M.lookup n (mtMethods table) of
    Just r  -> return r
    _       -> fail $ "No such method: " ++ show n

data (Typeable1 m, Monad m) => Invocant m
    = forall a. (Show a, Eq a, Ord a, Typeable a) => MkInvocant
        a                   -- Invocant
        (AnyResponder m)    -- Responder

data Invocant_Type deriving (Typeable)

fromInvocant :: forall m b. (Typeable1 m, Monad m, Typeable b) => Arguments m -> m b
fromInvocant CaptSub{}                  = fail "No invocant"
fromInvocant CaptMeth{ c_invocant = x } = case Typeable.cast x of
    Just y -> return y
    _      -> fail $ "Cannot cast from " ++ (show $ typeOf x) ++ " to " ++ (show $ typeOf (undefined :: b))


instance (Typeable1 m, Monad m) => Typeable (Invocant m) where
    typeOf _ = typeOf (undefined :: m Invocant_Type)

ivDispatch :: (Typeable1 m, Monad m) => Invocant m -> MethodInvocation m -> m (Invocant m)
ivDispatch i@(MkInvocant _ (MkResponder ri)) mi = do
    table   <- ri
    dispatch table i mi

instance (Typeable1 m, Monad m) => Show (Invocant m) where
    show (MkInvocant x _) = show x

instance (Typeable1 m, Monad m) => Eq (Invocant m) where
    MkInvocant a _ == MkInvocant b _ = a ?==? b
instance (Typeable1 m, Monad m) => Ord (Invocant m) where
    MkInvocant a _ `compare` MkInvocant b _ = a ?<=>? b

