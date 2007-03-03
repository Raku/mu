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
    , module Pugs.AST.Eval
    , module MO.Run
    , module MO.Compile
    , module MO.Compile.Class
    , module MO.Util
    , module Control.Monad.Fix
    ) where
import MO.Run hiding (__)
import MO.Compile
import MO.Compile.Class
import MO.Util
import Pugs.Internals
import Pugs.AST.Eval
import Control.Monad.Fix

class (Show a, Typeable a, Ord a, Typeable1 m, Monad m) => Boxable m a | a -> m where
    mkObj :: a -> Invocant m
    mkObj x = MkInvocant x (class_interface (classOf x))

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

(...) :: Boxable m b => String -> (a -> b) -> (ID, a -> m (Invocant m))
(...) x y = (_cast x, (return . mkObj) . y)

(!!!) :: Boxable m b => String -> (a -> m b) -> (ID, a -> m (Invocant m))
(!!!) x y = (_cast x, mkObjM . y)

mkObjM :: Boxable m a => m a -> m (Invocant m)
mkObjM x = do
    x' <- x
    return $ MkInvocant x' (class_interface (classOf x'))

mkBoxClass :: 
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

-- | Variant of @mkBoxClass@ meant to be called with the fixed-point
-- combinator, that adds the standard HOW and WHICH methods. E.g.:
--    _StrClass = fix $ mkBoxPureClass "Str" [Str methods]
mkBoxPureClass ::
    ( Boxable m a1
    , Boxable m a
    , Codeable m1 (HsCode m)
    , Typeable1 m1
    ) => String -> [(ID, a1 -> m (Invocant m))] -> a -> MI m1
mkBoxPureClass cls methods self =
    mkBoxClass cls methods'
    where
    methods' = flip (++) methods
        [ "HOW"         ... const self
        , "WHAT"        ... const (raiseWhatError ("Can't access attributes of prototype: " ++ cls) `asTypeOf` self)
        , "WHICH"       ... id
        ]

raiseWhatError :: String -> a
raiseWhatError = error

mkBoxMethod ::
    ( Method m (SimpleMethod m)
    , Codeable m (HsCode m1)
    , Typeable t
    , Typeable1 m1
    , Monad m1
    ) => (ID, t -> m1 (Invocant m1)) -> AnyMethod m
mkBoxMethod (meth, fun) = MkMethod $ MkSimpleMethod
    { smName = meth
    , smDefinition = MkMethodCompiled $ HsCode $ \args -> do
        str <- fromInvocant args
        fun str   -- Note that we expect "fun" to be monadic
    }

(./) :: (Typeable1 m, Monad m) => Invocant m -> ID -> m (Invocant m)
inv ./ meth = ivDispatch inv $ MkMethodInvocation meth (mkArgs [])

type PureClass = MI Eval

instance Boxable Eval a => Boxable Eval [a]
instance Boxable Eval ID
instance Boxable Eval PureClass where
    classOf _ = _PureClass

_PureClass :: PureClass
_PureClass = mkBoxClass "Class"
    [ "HOW"         ... (const _PureClass :: PureClass -> PureClass)
    , "methods"     ... (map methodName . all_methods)
    ]

