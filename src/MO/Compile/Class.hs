{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-}

module MO.Compile.Class where

import MO.Base
import MO.Compile
import MO.Compile.Attribute
import MO.Compile.Role
import MO.Run
import MO.Util
import Data.Typeable (Typeable1, Typeable(..), cast)
import Control.Monad (liftM)

import qualified Pugs.Class.C3 as C3 (linearize)

import Data.Maybe (maybeToList, fromJust)
import qualified Data.Map as Map

class (Typeable1 m, Monad m, Typeable c, Eq c) => Class m c | c -> m where
    class_name               :: c -> String
    superclasses             :: c -> [AnyClass m]

    -- These three methods below are shared between all C3-happy classes.
    class_precedence_list    :: c -> [AnyClass m]
    class_precedence_list cls = case C3.linearize (Just . superclasses) (AnyClass cls) of
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
        attribute_methods = cmap makeAccessorMethod . newCollection' attrAccessorName . attributes
        role_attribute_methods = cmap makeAccessorMethod . newCollection' attrAccessorName . roAttributes
        makeAccessorMethod attr = AnyMethod $ MkSimpleMethod
            { smName        = attrAccessorName attr
            , smDefinition  = MkMethodCompiled $ PureCode (error . show . getInvocant)
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
    merged_roles c           = emptyRole { roRoles = roles c }
    
--  attribute_grammars       :: c -> [AttributeGrammar]
    attributes               :: c -> [Attribute m]
    public_methods           :: c -> Collection (AnyMethod m)
    private_methods          :: c -> Collection (AnyMethod m)

    class_interface :: c -> AnyResponder m
    class_interface = AnyResponder
                       . (fromMethodList :: [(String, MethodCompiled m)] -> m (MethodTable m))
                       . map (\m -> (methodName m, methodCompile m))
                       . all_methods

data AnyClass m = forall c. Class m c => AnyClass c
data AnyClass_Type deriving Typeable

instance (Typeable1 m, Monad m) => Typeable (AnyClass m) where
    typeOf x = typeOf (undefined :: m AnyClass_Type)

instance (Typeable1 m, Monad m) => Eq (AnyClass m) where
    AnyClass x == AnyClass y = case cast y of
        Just y' -> x == y'  -- same type, compare with its Eq
        _       -> False    -- not same type, never eq

instance (Typeable1 m, Monad m) => Show (AnyClass m) where
    show = class_name

-- TODO: How hackish is instantiating the AnyMoose for the class Moose?
-- Could it cause serious problems? Well, there's a DRY problem here, but
-- what else?
instance (Typeable1 m, Monad m) => Class m (AnyClass m) where
    class_name              (AnyClass c) = class_name c
    superclasses            (AnyClass c) = superclasses c
    class_precedence_list   (AnyClass c) = class_precedence_list c
    all_methods             (AnyClass c) = all_methods c
    roles                   (AnyClass c) = roles c
--  attribute_grammars      (AnyClass c) = attribute_grammars c
    attributes              (AnyClass c) = attributes c
    public_methods          (AnyClass c) = public_methods c
    private_methods         (AnyClass c) = private_methods c
    class_interface      (AnyClass c) = class_interface c

-- FIXME: hmm.. how to do Subclassing properly, ie. have MI and MI share about
-- everything except for just a couple of things? Type-classes doesn't seem to
-- match right, specially because I want nice constructors via record syntax.

data (Monad m, Typeable1 m) => MI m
    = MkMI
        { clsParents                :: [AnyClass m]
        , clsRoles                  :: [Role m]
--      , clsAttributeGrammars      :: [AttributeGrammar]
        , clsAttributes             :: [Attribute m]
        , clsPublicMethods          :: Collection (AnyMethod m)
        , clsPrivateMethods         :: Collection (AnyMethod m)
        , clsName                   :: String
        }
        -- deriving (Eq)

data MI_Type deriving Typeable
instance (Typeable1 m, Monad m) => Eq (MI m) where
    x == y = clsName x == clsName y
instance (Typeable1 m, Monad m) => Typeable (MI m) where
    typeOf x = typeOf (undefined :: m MI_Type)

emptyMI :: (Typeable1 m, Monad m) => MI m
emptyMI = MkMI
    { clsParents        = []
    , clsRoles          = []
    , clsAttributes     = []
    , clsPublicMethods  = newCollection []
    , clsPrivateMethods = newCollection []
    , clsName           = ""
    }

-- FIXME: Method then AnyMethod then MethodAttached then Anymethod again is ugly
newMI :: (Typeable1 m, Monad m) => MI m -> MI m
newMI old = new
    where attach = AnyMethod . MkMethodAttached new
          withBless     = insert "bless"    (blessMI new)
          withCreate    = id -- insert "CREATE"   (createMI new)
          new = old { clsPublicMethods = cmap attach . withBless . withCreate $ clsPublicMethods old }

blessMI :: Class m c => c -> AnyMethod m
blessMI c = AnyMethod MkSimpleMethod
    { smName = "bless"
    , smDefinition = MkMethodCompiled (HsCode constructor)
    }
    where
    -- Here we generate a structure from some layout.  The "params" here 
    -- contains initial values of those attributes.
    constructor params = do
        -- For each attribute, create a new instance of it.
        structure <- liftM Map.fromList . (`mapM` all_attributes c) $ \attr -> do
            let name = attrName attr
                userDefinedVal = namedArg params name
            val <- case userDefinedVal of
                Just obj    -> return obj
                _           -> attrDefault attr
            return (attrName attr, val)
        return $ MkInvocant structure (class_interface c)

instance (Typeable1 m, Monad m) => Class m (MI m) where
    class_name               = clsName
    superclasses             = clsParents
    roles                    = clsRoles
    attributes               = clsAttributes
    public_methods           = clsPublicMethods
    private_methods          = clsPrivateMethods

--    add_class_method c@MkMI{siClassMethods = ms} m =
--        c {siClassMethods = m:ms}


-- MethodAttached 
data MethodAttached m
    = forall c a. (Class m c, Method m a) => MkMethodAttached
        c       -- Origin
        a       -- Method

instance Monad m => Method m (MethodAttached m) where
    methodName (MkMethodAttached _ m) = methodName m
    methodCompile (MkMethodAttached _ m) = methodCompile m

