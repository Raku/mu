{-# OPTIONS_GHC -fglasgow-exts -fparr -fallow-undecidable-instances -fallow-incoherent-instances #-}

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
import MO.Util hiding (traceM, traceShow)
import Pugs.Internals
import Pugs.AST.Eval
import Control.Monad.Fix
import qualified StringTable.AtomMap as AtomMap
import qualified Data.Typeable as Typeable

type Val = Invocant Eval
type Call = MethodInvocation Eval

class (Show a, Typeable a, Ord a) => Boxable a where
    mkVal :: a -> Val
    mkVal x = MkInvocant x (class_interface (classOf x))

    coerceVal :: Val -> Eval a
    coerceVal (MkInvocant x _) = case Typeable.cast x of
        Just y -> return y
        _      -> fail $ "Cannot coerce from " ++ (show $ typeOf x) ++ " to " ++ (show $ typeOf (undefined :: a))

    instanceMethods :: [(ID, MethodPrim a)]
    instanceMethods = []

    classOf :: a -> PureClass
    classOf _ = mkPureClass (classNameOf (undefined :: a)) (instanceMethods :: [(ID, MethodPrim a)])

    classNameOf :: a -> String
    classNameOf _ = takeTypeName "" . reverse . show . typeOf $ (undefined :: a)
        -- Here we intuit "Str" from "Pugs.Val.Str.PureStr".
        where
        takeTypeName acc [] = acc
        takeTypeName acc (x:xs)
            | isLower x = takeTypeName (x:acc) xs
            | otherwise = x:acc

type MethodPrim a = (a -> [:Val:] -> Eval Val)

class Boxable b => MethodPrimable a b | a -> b where 
    asPrim :: a -> MethodPrim b

instance Boxable a => MethodPrimable Val a where
    asPrim v _ _ = return v

instance Boxable a => MethodPrimable Call a where
    asPrim f x _ = ivDispatch (mkVal x) f

-- Auto-generate pure instances from Eval instances
instance MethodPrimable (a -> b -> Eval z) a => MethodPrimable (a -> b -> z) a where
    asPrim f = asPrim ((\x args -> return (f x args)) :: (a -> b -> Eval z))

instance MethodPrimable (a -> b -> c -> Eval z) a => MethodPrimable (a -> b -> c -> z) a where
    asPrim f = asPrim ((\x y args -> return (f x y args)) :: (a -> b -> c -> Eval z))

instance (Boxable a, Boxable z) => MethodPrimable (a -> z) a where
    asPrim f x _ = return (mkVal (f x))

instance (Boxable a, Boxable z) => MethodPrimable (a -> Eval z) a where
    asPrim f x _ = fmap mkVal (f x)

instance (Boxable a, Boxable z) => MethodPrimable (a -> Val -> Eval z) a where
    asPrim f x args = fmap mkVal (f x (args !: 0))

instance (Boxable a, Boxable z) => MethodPrimable (a -> [:Val:] -> Eval z) a where
    asPrim f x args = fmap mkVal (f x args)

instance (Boxable a, Boxable z) => MethodPrimable (a -> [Val] -> Eval z) a where
    asPrim f x args = fmap mkVal (f x (cast args))

instance (Boxable a, Boxable b, Boxable z) => MethodPrimable (a -> [b] -> Eval z) a where
    asPrim f x args = do
        args' <- mapM coerceVal (cast args)
        fmap mkVal (f x args')

instance (Boxable a, Boxable b, Boxable z) => MethodPrimable (a -> b -> Eval z) a where
    asPrim f x args = do
        y <- coerceVal (args !: 0)
        fmap mkVal (f x y)

instance (Boxable a, Boxable b, Boxable c, Boxable z) => MethodPrimable (a -> b -> c -> Eval z) a where
    asPrim f x args = do
        y <- coerceVal (args !: 0)
        z <- coerceVal (args !: 1)
        fmap mkVal (f x y z)

(...) :: MethodPrimable a b => String -> a -> (ID, MethodPrim b)
(...) x y = (_cast x, asPrim y)

(!!!) :: Boxable b => String -> (a -> Eval b) -> (ID, a -> Eval Val)
(!!!) x y = (_cast x, mkValM . y)

mkValM :: Boxable a => Eval a -> Eval Val
mkValM x = do
    x' <- x
    return $ MkInvocant x' (class_interface (classOf x'))

mkBoxClass :: Boxable a => String -> [(ID, MethodPrim a)] -> PureClass
mkBoxClass cls methods = newMOClass MkMOClass
    { moc_parents         = []
    , moc_roles           = []
    , moc_attributes      = []
    , moc_public_methods  = newCollection' methodName $ map mkBoxMethod methods
    , moc_private_methods = newCollection []
    , moc_name            = _cast cls
    }



-- | Variant of @mkBoxClass@ making use of the fixed-point combinator
-- to tye in its "self", and, that adds the standard HOW and WHICH methods.
-- mkPureClass :: (Boxable a) => String -> [(ID, MethodPrim a)] -> PureClass
mkPureClass :: Boxable a => String -> [(ID, MethodPrim a)] -> PureClass
mkPureClass cls methods = fix . (mkBoxClass cls .) $ \self -> flip (++) methods
    [ ""        ... mkVal self
    , "ITEM"    ... id
    , "LIST"    ... id
    ]

raiseWhatError :: String -> a
raiseWhatError = error

mkBoxMethod :: forall a. Boxable a => (ID, MethodPrim a) -> AnyMethod Eval
mkBoxMethod (meth, fun) = MkMethod $ MkSimpleMethod
    { sm_name       = meth
    , sm_definition = MkMethodCompiled $ \args -> do
        inv  <- fromInvocant args :: Eval a
        fun inv $ concatMapP f_positionals (c_feeds args)
    }

type PureClass = MOClass Eval

instance (Show a, Typeable a, Ord a) => Boxable (Maybe a)

instance Boxable a => Boxable [a]
instance Boxable a => Boxable [:a:]

instance Boxable ID
instance Boxable PureClass where
    classOf _ = _PureClass

_PureClass :: PureClass
_PureClass = mkPureClass "Class"
    [ "methods"     ... ((filter (/= nullID) . map methodName . all_methods) :: PureClass -> [ID])
    ]

instance ((:>:) Call) String where
    cast = (`MkMethodInvocation` CaptSub{ c_feeds = [::] }) . _cast

instance ((:>:) Call) ByteString where
    cast = (`MkMethodInvocation` CaptSub{ c_feeds = [::] }) . cast

instance ((:>:) Call (ByteString, [Val], AtomMap Val)) where
    cast (meth, pos, named) = MkMethodInvocation (cast meth) CaptSub
        { c_feeds = [: MkFeed (toP pos) (AtomMap.map (\x -> [:x:]) named) :]}

