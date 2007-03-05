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

-- Little overview.
--
-- Suppose someone is calling a method, like: $foo.moose(1,2,3). Usually, we
-- create a MethodInvocation containing "moose" as the name of the method and
-- some Arguments thing, contaning the "1,2,3".
--
-- The "$foo" object is _represented_ by an Invocant datatype, which has a
-- pointer to "$foo" itself and an ResponderInterface (usually provided by the
-- Class that $foo was instantiated), which knows how to answer for a method
-- call, this is called 'dispatch' in the ResponderInterface class.
--
-- One example of ResponderInterface is the MethodTable, it has a Map of
-- MethodCompileds (identified by MethodName). Its 'dispatch' takes an Invocant
-- and a MethodInvocation, add the Invocant to the MInv Arguments,
-- lookup the MInv method name in it's on table, if found, run the compiled method
-- with the augmented Arguments.
--
-- The function ivDispatch does almost same as 'dispatch', but it gets the RI
-- that the Invocant has inside it (given by the Class, for example). So you can
-- think of "$foo.moose(1,2,3)" as a call to
-- "ivDispatch (Invocant_of_$foo) (Arguments_containing_(1,2,3))"


-- FIXME: At first we thought of having these two abstractions, but now
-- seem unnecessary, but I may be forgetting something :P
-- class Invocation a
-- class Responder a


data MethodInvocation m
    = MkMethodInvocation
        { mi_name      :: !MethodName  
        , mi_arguments :: !(Arguments m)
        }


class Monad m => ResponderInterface m a | a -> m where
    fromMethodList :: [(MethodName, MethodCompiled m)] -> m a
    dispatch :: a -> Invocant m -> MethodInvocation m -> m (Invocant m)
    -- here for debugging purposes.
    -- toNameList :: a -> [MethodName]

{-
instance ResponderInterface m a => Show a where
    show = show . toNameList
-}


data Monad m => NoResponse m = NoResponse

instance Monad m => ResponderInterface m (NoResponse m) where
    dispatch _ _ _      = fail "Dispatch failed - NO CARRIER"
    fromMethodList _    = return NoResponse
    -- toNameList _        = []

emptyResponder :: (Typeable1 m, Monad m) => AnyResponder m
emptyResponder = MkResponder (return NoResponse)



-- | This is a static method table.
data MethodTable m
    = MkMethodTable
        { mt_methods :: !(M.Map MethodName (MethodCompiled m))
        }

instance (Typeable1 m, Monad m) => ResponderInterface m (MethodTable m) where
    fromMethodList = return . MkMethodTable . M.fromList
    dispatch mt responder inv@(MkMethodInvocation n args) = case M.lookup n (mt_methods mt) of
        Just method_compiled -> do
            runMC method_compiled (withInvocant args responder)
        _ -> fail $ "Can't locate object method " ++ show n ++ " of invocant: " ++ show responder
            
    -- toNameList = M.keys . mt_methods


data AnyResponder m = forall c. ResponderInterface m c => MkResponder !(m c)

instance (Typeable1 m, Monad m) => Typeable (AnyResponder m) where
    typeOf _ = mkTyConApp (mkTyCon "AnyResponder") [typeOf1 (undefined :: m ())]



-- Invocant represent an object aggregated with an ResponderInterface

data (Typeable1 m, Monad m) => Invocant m
    = forall a. (Show a, Eq a, Ord a, Typeable a) => MkInvocant
        a                   -- Invocant
        (AnyResponder m)    -- Responder

fromInvocant :: forall m b. (Typeable1 m, Monad m, Typeable b) => Arguments m -> m b
fromInvocant CaptSub{}                  = fail "No invocant"
fromInvocant CaptMeth{ c_invocant = MkInvocant x _ } = case Typeable.cast x of
    Just y -> return y
    _      -> fail $ "Could not coerce from " ++ (show $ typeOf x) ++ " to " ++ (show $ typeOf (undefined :: b))


instance (Typeable1 m, Monad m) => Typeable (Invocant m) where
    typeOf _ = mkTyConApp (mkTyCon "Invocant") [typeOf1 (undefined :: m ())]

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

-- Helpers to create simple/empty invocants.
__ :: (Typeable1 m, Monad m, Ord a, Show a, Typeable a) => a -> Invocant m
__ = (`MkInvocant` emptyResponder)

stubInvocant :: (Typeable1 m, Monad m) => Invocant m
stubInvocant = MkInvocant () emptyResponder


-- Helper to create a Arguments based on a list of Invocants
mkArgs :: (Typeable1 m, Monad m) => [Invocant m] -> Arguments m
mkArgs x = CaptSub{ c_feeds = [: MkFeed { f_positionals = toP x, f_nameds = M.empty } :] }

