{-# OPTIONS_GHC -fglasgow-exts -fparr #-}

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
import qualified Data.Map as Map
import qualified Data.Typeable as Typeable

class (Show a, Typeable a, Ord a) => Boxable a where
    mkVal :: a -> Invocant Eval
    mkVal x = MkInvocant x (class_interface (classOf x))

    coerceVal :: Invocant Eval -> Eval a
    coerceVal (MkInvocant x _) = case Typeable.cast x of
        Just y -> return y
        _      -> fail $ "Cannot coerce from " ++ (show $ typeOf x) ++ " to " ++ (show $ typeOf (undefined :: a))

    classOf :: a -> MOClass Eval
    classOf o = mkPureClass ty ([] :: [(ID, ID -> Eval (Invocant Eval))])
        where
        ty = _cast . takeTypeName "" . reverse . show $ typeOf o
        -- Here we intuit "Str" from "Pugs.Val.Str.PureStr".
        takeTypeName acc [] = acc
        takeTypeName acc (x:xs)
            | isLower x = takeTypeName (x:acc) xs
            | otherwise = x:acc

(...) :: Boxable b => String -> (a -> b) -> (ID, a -> Eval (Invocant Eval))
(...) x y = (_cast x, (return . mkVal) . y)

(!!!) :: Boxable b => String -> (a -> Eval b) -> (ID, a -> Eval (Invocant Eval))
(!!!) x y = (_cast x, mkValM . y)

mkValM :: Boxable a => Eval a -> Eval (Invocant Eval)
mkValM x = do
    x' <- x
    return $ MkInvocant x' (class_interface (classOf x'))

mkBoxClass :: Typeable t => String -> [(ID, t -> Eval (Invocant Eval))] -> MOClass Eval
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
mkPureClass :: (Boxable a) => String -> [(ID, a -> Eval (Invocant Eval))] -> MOClass Eval
mkPureClass cls methods =
    fix (mkBoxClass cls . methods')
    where 
    methods' self = flip (++) methods
        [ "HOW"         ... const self
        , "WHAT"        ... const (raiseWhatError ("Can't access attributes of prototype: " ++ cls) `asTypeOf` self)
        , "WHICH"       ... id
        , "ITEM"        ... id
        , "LIST"        ... id
        ]

raiseWhatError :: String -> a
raiseWhatError = error

mkBoxMethod :: Typeable t => (ID, t -> Eval (Invocant Eval)) -> AnyMethod Eval
mkBoxMethod (meth, fun) = MkMethod $ MkSimpleMethod
    { sm_name       = meth
    , sm_definition = MkMethodCompiled $ HsCode $ \args -> do
        str <- fromInvocant args
        fun str   -- Note that we expect "fun" to be monadic
    }

type PureClass = MOClass Eval

instance Boxable a => Boxable [a]
instance Boxable ID
instance Boxable PureClass where
    classOf _ = _PureClass

_PureClass :: PureClass
_PureClass = mkPureClass "Class"
    [ "methods"     ... ((map methodName . all_methods) :: PureClass -> [ID])
    ]

instance ((:>:) (MethodInvocation Eval)) String where
    cast = (`MkMethodInvocation` CaptSub{ c_feeds = [::] }) . _cast

instance ((:>:) (MethodInvocation Eval)) ByteString where
    cast = (`MkMethodInvocation` CaptSub{ c_feeds = [::] }) . cast

instance ((:>:) (MethodInvocation Eval) (ByteString, [Invocant Eval], Map ID (Invocant Eval))) where
    cast (meth, pos, named) = MkMethodInvocation (cast meth) CaptSub{ c_feeds = [: MkFeed (toP pos) (Map.map (\x -> [:x:]) named) :]}

