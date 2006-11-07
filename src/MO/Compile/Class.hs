{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}

module MO.Compile.Class where

import MO.Base
import MO.Compile
import MO.Compile.Attribute
import MO.Compile.Role
import MO.Run
import MO.Util
import Data.Typeable (Typeable1, Typeable(..), cast)
import Control.Monad (liftM)

import qualified MO.C3 as C3 (linearize)

import Data.Maybe (maybeToList, fromJust)
import qualified Data.Map as Map

type AttributeGrammar = ()

-- FIXME: I merged Abstract::Class and Class roles just to make
-- things easier now

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
        ++ concatMap roleAttributes (roles c)
        where
        roleAttributes r = roAttributes r ++ concatMap roleAttributes (roRoles r)

    all_class_methods        :: c -> [AnyMethod m]
    all_class_methods c      = shadow (from_c ++ [from_r])
      where from_c = map class_methods (class_precedence_list c)
            from_r = all_using_role_shadowing
                         (merged_roles c) role_class_methods

    all_attribute_instance_methods :: c -> [AnyMethod m]
    all_attribute_instance_methods c = shadow (from_c ++ [from_r])
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

    all_instance_methods     :: c -> [AnyMethod m]
    all_instance_methods cls = all_attribute_instance_methods cls ++ all_regular_instance_methods cls

    all_regular_instance_methods :: c -> [AnyMethod m]
    all_regular_instance_methods c = shadow (from_c ++ [from_r])
      where from_c = map instance_methods (class_precedence_list c)
            from_r = all_using_role_shadowing
                         (merged_roles c) role_instance_methods

    roles                    :: c -> [Role m]
    merged_roles             :: c -> Role m
    merged_roles c           = emptyRole { roRoles = roles c }
    
    attribute_grammars       :: c -> [AttributeGrammar]
    attributes               :: c -> [Attribute m]
    instance_methods         :: c -> Collection (AnyMethod m)
    private_instance_methods :: c -> Collection (AnyMethod m)
    class_methods            :: c -> Collection (AnyMethod m)
    private_class_methods    :: c -> Collection (AnyMethod m)

    instance_interface :: c -> AnyResponder m
    instance_interface = AnyResponder
                       . (fromMethodList :: [(String, MethodCompiled m)] -> m (MethodTable m))
                       . map (\m -> (name m, compile m))
                       . all_instance_methods

    class_interface :: c -> AnyResponder m
    class_interface = AnyResponder
                    . (fromMethodList :: [(String, MethodCompiled m)] -> m (MethodTable m))
                    . map (\m -> (name m, compile m))
                    . all_class_methods

--    add_class_method :: c -> AnyMethod -> c -- go monadic?




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
    class_name               (AnyClass c) = class_name c
    superclasses             (AnyClass c) = superclasses c
    class_precedence_list    (AnyClass c) = class_precedence_list c
    all_class_methods        (AnyClass c) = all_class_methods c
    all_instance_methods     (AnyClass c) = all_instance_methods c

    roles                    (AnyClass c) = roles c
    attribute_grammars       (AnyClass c) = attribute_grammars c
    attributes               (AnyClass c) = attributes c
    instance_methods         (AnyClass c) = instance_methods c
    private_instance_methods (AnyClass c) = private_instance_methods c
    class_methods            (AnyClass c) = class_methods c
    private_class_methods    (AnyClass c) = private_class_methods c

    instance_interface       (AnyClass c) = instance_interface c
    class_interface          (AnyClass c) = class_interface c

--    add_class_method         (AnyClass c) = AnyClass . add_class_method c



-- FIXME: hmm.. how to do Subclassing properly, ie. have MI and MI share about
-- everything except for just a couple of things? Type-classes doesn't seem to
-- match right, specially because I want nice constructors via record syntax.

data (Monad m, Typeable1 m) => MI m
    = MkMI
        { clsParents                :: [AnyClass m]
        , clsRoles                  :: [Role m]
--      , clsAttributeGrammars      :: [AttributeGrammar]
        , clsAttributes             :: [Attribute m]
        , clsMethods                :: Collection (AnyMethod m)
        , clsPrivateMethods         :: Collection (AnyMethod m)
--      , clsInstanceMethods        :: Collection (AnyMethod m)
--      , clsPrivateInstanceMethods :: [AnyMethod m]
--      , clsClassMethods           :: Collection (AnyMethod m)
--      , clsPrivateClassMethods    :: [AnyMethod m]
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
            { clsParents                = []
            , clsRoles                  = []
--          , clsAttributeGrammars      = []
            , clsAttributes             = []
            , clsMethods                = newCollection []
            , clsPrivateMethods         = newCollection []
            , clsName                   = "emptyMI"
            }

-- FIXME: Method then AnyMethod then MethodAttached then Anymethod again is ugly
newMI :: (Typeable1 m, Monad m) => MI m -> MI m
newMI old = new
    where attach = AnyMethod . MkMethodAttached new
          withBless     = insert "bless"    (blessMI new)
          withCreate    = id -- insert "CREATE"   (createMI new)
          new = old { clsMethods = cmap attach . withBless . withCreate $ clsMethods old }

blessMI :: Class m c => c -> AnyMethod m
blessMI c = AnyMethod MkSimpleMethod
    { smName = "bless"
    , smDefinition = MkMethodCompiled
        { mcBody = HsCode constructor }
    }
    where
    -- Here we generate a structure from some layout.  The "params" here 
    -- contains initial values of those attributes.
    constructor params = do
        -- For each attribute, create a new instance of it.
        structure <- liftM Map.fromList . (`mapM` all_attributes c) $ \attr -> do
            let name = attrName attr
            userDefinedVal <- namedArg params name
            val <- case userDefinedVal of
                Just obj    -> return obj
                _           -> attrDefault attr
            return (attrName attr, val)
        return $ MkInvocant structure (instance_interface c)

instance (Typeable1 m, Monad m) => Class m (MI m) where
    class_name               = clsName
    superclasses             = clsParents
    roles                    = clsRoles
    attribute_grammars       = const [] -- clsAttributeGrammars
    attributes               = clsAttributes
    instance_methods         = clsMethods
    private_instance_methods = clsPrivateMethods
    class_methods            = clsMethods
    private_class_methods    = clsPrivateMethods

--    add_class_method c@MkMI{siClassMethods = ms} m =
--        c {siClassMethods = m:ms}


-- MethodAttached 
data MethodAttached m
    = forall c a. (Class m c, Method m a) => MkMethodAttached
    { maOrigin :: c
    , maMethod :: a
    }

instance Monad m => Method m (MethodAttached m) where
    name MkMethodAttached { maMethod = m } = name m
    compile MkMethodAttached { maMethod = m } = compile m

