{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-}

module MO.Compile.Class where

import MO.Base ()
import MO.Compile
import MO.Compile.Attribute
import MO.Compile.Role
import MO.Run
import MO.Util
import Data.Typeable (Typeable1, Typeable(..))
import Control.Monad (liftM)
import qualified Data.Typeable as Typeable

import qualified Pugs.Class.C3 as C3 (linearize)

import qualified Data.Map as Map

type ClassName = ID

class (Typeable1 m, Monad m, Typeable c, Eq c) => Class m c | c -> m where
    class_name               :: c -> ClassName
    superclasses             :: c -> [AnyClass m]

    -- These three methods below are shared between all C3-happy classes.
    class_precedence_list    :: c -> [AnyClass m]
    class_precedence_list cls = case C3.linearize (Just . superclasses) (MkClass cls) of
        Just ok -> ok
        _       -> error "..."

    all_attributes        :: c -> [Attribute m]
    all_attributes c
        = concatMap attributes (class_precedence_list c)
        ++ concatMap allRoleAttributes (roles c)
        where
        allRoleAttributes r = role_attributes r ++ concatMap allRoleAttributes (parent_roles r)

    all_attribute_methods :: c -> [AnyMethod m]
    all_attribute_methods c = shadow (from_c ++ [from_r])
        where
        from_c = map attribute_methods (class_precedence_list c)
        from_r = all_using_role_shadowing (merged_roles c) role_attribute_methods
        -- Take all public attributes of this class and make read-only accessor for them
        attribute_methods = cmap makeAccessorMethod . newCollection' a_accessor_name . attributes
        role_attribute_methods = cmap makeAccessorMethod . newCollection' a_accessor_name . r_attributes
        makeAccessorMethod attr = MkMethod $ MkSimpleMethod
            { sm_name        = a_accessor_name attr
            , sm_definition  = MkMethodCompiled $ error . show . getInvocant
            }

    all_methods     :: c -> [AnyMethod m]
    all_methods cls = all_attribute_methods cls ++ all_regular_methods cls

    all_regular_methods :: c -> [AnyMethod m]
    all_regular_methods c = shadow (from_c ++ [from_r])
      where from_c = map public_methods (class_precedence_list c)
            from_r = all_using_role_shadowing
                         (merged_roles c) role_public_methods

    roles                    :: c -> [Role m]
    merged_roles             :: c -> Role m
    merged_roles c           = emptyRole { r_roles = roles c }
    
--  attribute_grammars       :: c -> [AttributeGrammar]
    attributes               :: c -> [Attribute m]
    public_methods           :: c -> Collection (AnyMethod m)
    private_methods          :: c -> Collection (AnyMethod m)

    class_interface :: c -> AnyResponder m
    class_interface = MkResponder
                       . (fromMethodList :: [(MethodName, MethodCompiled m)] -> m (MethodTable m))
                       . map (\m -> (methodName m, methodCompile m))
                       . all_methods

data AnyClass m = forall c. Class m c => MkClass !c
data AnyClass_Type deriving Typeable

instance (Typeable1 m, Monad m) => Typeable (AnyClass m) where
    typeOf _ = typeOf (undefined :: m AnyClass_Type)

instance (Typeable1 m, Monad m) => Eq (AnyClass m) where
    MkClass x == MkClass y = case Typeable.cast y of
        Just y' -> x == y'  -- same type, compare with its Eq
        _       -> False    -- not same type, never eq

instance (Typeable1 m, Monad m) => Show (AnyClass m) where
    show = show . class_name

-- TODO: How hackish is instantiating the AnyMoose for the class Moose?
-- Could it cause serious problems? Well, there's a DRY problem here, but
-- what else?
instance (Typeable1 m, Monad m) => Class m (AnyClass m) where
    class_name              (MkClass c) = class_name c
    superclasses            (MkClass c) = superclasses c
    class_precedence_list   (MkClass c) = class_precedence_list c
    all_methods             (MkClass c) = all_methods c
    roles                   (MkClass c) = roles c
--  attribute_grammars      (MkClass c) = attribute_grammars c
    attributes              (MkClass c) = attributes c
    public_methods          (MkClass c) = public_methods c
    private_methods         (MkClass c) = private_methods c
    class_interface         (MkClass c) = class_interface c

-- FIXME: hmm.. how to do Subclassing properly, ie. have MOClass and MOClass share about
-- everything except for just a couple of things? Type-classes doesn't seem to
-- match right, specially because I want nice constructors via record syntax.

data (Monad m, Typeable1 m) => MOClass m
    = MkMOClass
        { moc_parents                :: [AnyClass m]
        , moc_roles                  :: [Role m]
--      , moc_attribute_grammar      :: [AttributeGrammar]
        , moc_attributes             :: [Attribute m]
        , moc_public_methods         :: Collection (AnyMethod m)
        , moc_private_methods        :: Collection (AnyMethod m)
        , moc_name                   :: ClassName
        }
        -- deriving (Eq)

data MOClass_Type deriving Typeable
instance (Typeable1 m, Monad m) => Show (MOClass m) where
    show = ('^':) . cast . moc_name
instance (Typeable1 m, Monad m) => Ord (MOClass m) where
    compare x y = moc_name x `compare` moc_name y
instance (Typeable1 m, Monad m) => Eq (MOClass m) where
    x == y = moc_name x == moc_name y
instance (Typeable1 m, Monad m) => Typeable (MOClass m) where
    typeOf _ = typeOf (undefined :: m MOClass_Type)

emptyMOClass :: (Typeable1 m, Monad m) => MOClass m
emptyMOClass = MkMOClass
    { moc_parents         = []
    , moc_roles           = []
    , moc_attributes      = []
    , moc_public_methods  = newCollection []
    , moc_private_methods = newCollection []
    , moc_name            = nullID
    }

_bless :: MethodName
_bless = _cast "bless"

-- FIXME: Method then AnyMethod then MethodAttached then AnyMethod again is ugly
newMOClass :: (Typeable1 m, Monad m) => MOClass m -> MOClass m
newMOClass old = new
    where attach        = MkMethod . MkMethodAttached new
          withBless     = insert _bless (blessMOClass new)
          withCreate    = id -- insert "CREATE"   (createMOClass new)
          new           = old { moc_public_methods = cmap attach . withBless . withCreate $ moc_public_methods old }

blessMOClass :: Class m c => c -> AnyMethod m
blessMOClass c = MkMethod $ MkSimpleMethod
    { sm_name        = _bless
    , sm_definition  = MkMethodCompiled constructor
    }
    where
    -- Here we generate a structure from some layout.  The "params" here 
    -- contains initial values of those attributes.
    constructor params = do
        -- For each attribute, create a new instance of it.
        structure <- liftM Map.fromList . (`mapM` all_attributes c) $ \attr -> do
            let name = a_name attr
                userDefinedVal = namedArg params name
            val <- case userDefinedVal of
                Just obj    -> return obj
                _           -> a_default attr
            return (a_name attr, val)
        return $ MkInvocant structure (class_interface c)

instance (Typeable1 m, Monad m) => Class m (MOClass m) where
    class_name               = moc_name
    superclasses             = moc_parents
    roles                    = moc_roles
    attributes               = moc_attributes
    public_methods           = moc_public_methods
    private_methods          = moc_private_methods

--    add_class_method c@MkMOClass{siClassMethods = ms} m =
--        c {siClassMethods = m:ms}


-- MethodAttached 
data MethodAttached m
    = forall c a. (Class m c, Method m a) => MkMethodAttached
        !c       -- Origin
        !a       -- Method

instance Monad m => Method m (MethodAttached m) where
    methodName (MkMethodAttached _ m) = methodName m
    methodCompile (MkMethodAttached _ m) = methodCompile m

