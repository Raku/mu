{-# OPTIONS_GHC -fglasgow-exts -cpp #-}

{-|
    Class meta-model.  (object meta-meta-model)

>   Learn now the lore of Living Creatures!
>   First name the four, the free peoples:
>   Eldest of all, the elf-children;
>   Dwarf the delver, dark are his houses;
>   Ent the earthborn, old as mountains;
>   Man the mortal, master of horses...
-}

module Pugs.Class
    ( module Pugs.Class
    , module MO.Run
    , module MO.Compile
    , module MO.Compile.Class
    , module MO.Util
    ) where
import MO.Run hiding (__)
import MO.Compile
import MO.Compile.Class
import MO.Util
import Pugs.Internals

class (Typeable a, Ord a, Typeable1 m, Monad m) => Boxable m a | a -> m where
    classOfBox :: a -> MI m
    fromObjBox :: Invocant m -> m a

(...) :: forall t a b (m :: * -> *). (Show b, Boxable m b) => t -> (a -> b) -> (t, a -> m (Invocant m))
(...) x y = (x, mkObj . y)

(!!!) :: forall t a1 (m :: * -> *) a. (Show a, Boxable m a) => t -> (a1 -> m a) -> (t, a1 -> m (Invocant m))
(!!!) x y = (x, mkObjM . y)

mkObj :: (Show a, Boxable m a) => a -> m (Invocant m)
mkObj x = return $ MkInvocant x (class_interface (classOfBox x))

mkObjM :: (Show a, Boxable m a) => m a -> m (Invocant m)
mkObjM x = do
    x' <- x
    return $ MkInvocant x' (class_interface (classOfBox x'))

mkBoxClass :: forall t (m :: * -> *) (m1 :: * -> *).
    ( Method m1 (AnyMethod m1)
    , Code m1 (HsCode m)
    , Typeable t
    , Typeable1 m
    , Monad m
    , Typeable1 m1
    , Method m1 (SimpleMethod m1)
    ) => String -> [(String, t -> m (Invocant m))] -> MI m1
mkBoxClass cls methods = newMI MkMI
    { clsParents        = []
    , clsRoles          = []
    , clsAttributes     = []
    , clsPublicMethods  = newCollection' methodName $ map mkBoxMethod methods
    , clsPrivateMethods = newCollection []
    , clsName = cls
    }

mkBoxMethod :: forall t (m1 :: * -> *) (m :: * -> *).
    ( Method m (SimpleMethod m)
    , Code m (HsCode m1)
    , Typeable t
    , Typeable1 m1
    , Monad m1
    ) => (String, t -> m1 (Invocant m1)) -> AnyMethod m
mkBoxMethod (meth, fun) = AnyMethod $ MkSimpleMethod
    { smName = meth
    , smDefinition = MkMethodCompiled $ HsCode $ \args -> do
        str <- fromInvocant args
        fun str   -- Note that we expect "fun" to be monadic
    }

inv ./ meth = ivDispatch inv $ MkMethodInvocation meth (mkArgs [])
