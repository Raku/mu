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
import Pugs.AST.Eval

class (Show a, Typeable a, Ord a, Typeable1 m, Monad m) => Boxable m a | a -> m where
    classOf :: a -> MI m
    classOf o = mkBoxClass ty ([] :: [(ID, ID -> m (Invocant m))])
        where
        ty = _cast . takeTypeName "" . reverse . show $ typeOf o
        -- Here we intuit "Str" from "Pugs.Val.Str.PureStr".
        takeTypeName acc [] = acc
        takeTypeName acc (x:xs)
            | isLower x = takeTypeName (x:acc) xs
            | otherwise = x:acc

    fromObj :: Invocant m -> m a
    fromObj (MkInvocant x _) = fromTypeable x

(...) :: forall a b (m :: * -> *). (Boxable m b) => String -> (a -> b) -> (ID, a -> m (Invocant m))
(...) x y = (_cast x, mkObj . y)

(!!!) :: forall a1 (m :: * -> *) a. (Boxable m a) => String -> (a1 -> m a) -> (ID, a1 -> m (Invocant m))
(!!!) x y = (_cast x, mkObjM . y)

mkObj :: (Boxable m a) => a -> m (Invocant m)
mkObj x = return $ MkInvocant x (class_interface (classOf x))

mkObjM :: (Boxable m a) => m a -> m (Invocant m)
mkObjM x = do
    x' <- x
    return $ MkInvocant x' (class_interface (classOf x'))

mkBoxClass :: forall t (m :: * -> *) (m1 :: * -> *).
    ( Method m1 (AnyMethod m1)
    , Codeable m1 (HsCode m)
    , Typeable t
    , Typeable1 m
    , Monad m
    , Typeable1 m1
    , Method m1 (SimpleMethod m1)
    ) => String -> [(ID, t -> m (Invocant m))] -> MI m1
mkBoxClass cls methods = newMI MkMI
    { clsParents        = []
    , clsRoles          = []
    , clsAttributes     = []
    , clsPublicMethods  = newCollection' methodName $ map mkBoxMethod methods
    , clsPrivateMethods = newCollection []
    , clsName           = _cast cls
    }

mkBoxMethod :: forall t (m1 :: * -> *) (m :: * -> *).
    ( Method m (SimpleMethod m)
    , Codeable m (HsCode m1)
    , Typeable t
    , Typeable1 m1
    , Monad m1
    ) => (ID, t -> m1 (Invocant m1)) -> AnyMethod m
mkBoxMethod (meth, fun) = AnyMethod $ MkSimpleMethod
    { smName = meth
    , smDefinition = MkMethodCompiled $ HsCode $ \args -> do
        str <- fromInvocant args
        fun str   -- Note that we expect "fun" to be monadic
    }

(./) :: (Typeable1 m, Monad m) => Invocant m -> ID -> m (Invocant m)
inv ./ meth = ivDispatch inv $ MkMethodInvocation meth (mkArgs [])
