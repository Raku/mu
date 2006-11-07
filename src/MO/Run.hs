{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-}

module MO.Run (
    module MO.Run,
    ArgList(..),
    NoCode(..),
    HsCode(..),
    PureCode(..),
    Arguments(..),
) where

-- FIXME: systematize a nice order for imports (steal Pugs')
import MO.Util
import MO.Base
import MO.Compile as C
import Data.Map as M
import Data.Typeable


-- Abstract Roles
-- class Invocation a
-- class Responder a




-- 
data MethodInvocation m
    = forall a. Arguments m a => MkMethodInvocation
        { miName      :: String  
        , miArguments :: a
        , miCaller    :: Invocant m
        }

-- instance Invocation (MethodInvocation m)

-- maybe creating an empty template like this... 
-- newMethodInvocation = MkMethodInvocation { miName = "", miArgs = [], miCaller = undefined }
-- TODO: remember to check in Pugs code for something similar (saw it somewhere)


-- | This is a static method table.
data MethodTable m
    = MkMethodTable
        { mtMethods :: M.Map String (MethodCompiled m)
        }

emptyResponder :: (Typeable1 m, Monad m) => AnyResponder m
emptyResponder = AnyResponder (return NoResponse)

data NoResponse = NoResponse deriving Typeable

instance Monad m => ResponderInterface m NoResponse where
    dispatch _ _ _      = fail "Dispatch failed - NO CARRIER"
    fromMethodList _    = return NoResponse
    toNameList _        = []

__ :: (Typeable1 m, Monad m, Ord a, Show a, Typeable a) => a -> Invocant m
__ = (`MkInvocant` emptyResponder)

stubInvocant :: (Typeable1 m, Monad m) => Invocant m
stubInvocant = MkInvocant () emptyResponder

data AnyResponder m = forall c. ResponderInterface m c => AnyResponder (m c)
data AnyResponder_Type deriving Typeable

instance (Typeable1 m, Monad m) => Typeable (AnyResponder m) where
    typeOf x = typeOf (undefined :: m AnyResponder_Type)

class Monad m => ResponderInterface m a | a -> m where
    fromMethodList :: [(String, MethodCompiled m)] -> m a
    dispatch :: a -> Invocant m -> MethodInvocation m -> m (Invocant m)
    -- here for debugging purposes.
    toNameList :: a -> [String]

instance ResponderInterface m a => Show a where
    show = show . toNameList

instance Monad m => ResponderInterface m (MethodTable m) where
    fromMethodList = return . MkMethodTable . M.fromList
    dispatch mt responder inv@MkMethodInvocation{ miArguments = args } = do
        method_compiled <- mtMethod mt inv
        runMC method_compiled (withInvocant args responder)
    toNameList = M.keys . mtMethods

mtMethod :: Monad m => MethodTable a -> MethodInvocation m -> m (MethodCompiled a)
mtMethod table inv = M.lookup (miName inv) (mtMethods table)

--
data (Typeable1 m, Monad m) => Invocant m
    = forall a. (Show a, Eq a, Ord a, Typeable a) => MkInvocant
        { ivInvocant  :: a
        , ivResponder :: AnyResponder m
        }
data Invocant_Type deriving (Typeable)

instance (Typeable1 m, Monad m) => Typeable (Invocant m) where
    typeOf x = typeOf (undefined :: m Invocant_Type)

ivDispatch :: (Typeable1 m, Monad m) => Invocant m -> MethodInvocation m -> m (Invocant m)
ivDispatch i@MkInvocant{ ivResponder = AnyResponder ri } mi = do
    table   <- ri
    dispatch table i mi

instance (Typeable1 m, Monad m) => Show (Invocant m) where
    show (MkInvocant x _) = show x
instance (Typeable1 m, Monad m) => Eq (Invocant m) where
    MkInvocant a _ == MkInvocant b _ = a ?==? b
instance (Typeable1 m, Monad m) => Ord (Invocant m) where
    MkInvocant a _ `compare` MkInvocant b _ = a ?<=>? b

--instance Object Invocant

-- instance Responder (Invocant m)


-- XXX: not a good way to map, should we use that 'open types' kind of stuff
-- for everything -- so every class will be easy to extend and inherit?


