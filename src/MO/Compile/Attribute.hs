{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}

module MO.Compile.Attribute where
import MO.Base
import MO.Util
import Data.Typeable

type AttributeName = ID

data Monad m => Attribute m = MkAttribute
    { a_name          :: AttributeName
    , a_accessor_name :: AttributeName
    , a_is_private    :: Bool
    , a_default       :: m (Invocant m)
    }

instance Monad m => Show (Attribute m) where
    show (MkAttribute a b c _) = "<attr:" ++ show (a, b, c) ++ ">"

instance Monad m => Eq (Attribute m) where
    MkAttribute ax bx cx _ == MkAttribute ay by cy _ = (ax, bx, cx) == (ay, by, cy)

instance Monad m => Ord (Attribute m) where
    MkAttribute ax bx cx _ `compare` MkAttribute ay by cy _ = (ax, bx, cx) `compare` (ay, by, cy)

mkAttributeMandatory :: Monad m => AttributeName -> Attribute m
mkAttributeMandatory name = MkAttribute name name False (fail $ "Missing mandatory attribute: " ++ show name)

mkPrivateAttributeMandatory :: Monad m => AttributeName -> Attribute m
mkPrivateAttributeMandatory name = MkAttribute name name True (fail $ "Missing mandatory attribute: " ++ show name)

mkAttributeStub :: (Typeable1 m, Monad m) => AttributeName -> Attribute m
mkAttributeStub name = MkAttribute name name False (return stubInvocant)

mkPrivateAttributeStub :: (Typeable1 m, Monad m) => AttributeName -> Attribute m
mkPrivateAttributeStub name = MkAttribute name name True (return stubInvocant)

mkAttribute :: Monad m => AttributeName -> Invocant m -> Attribute m
mkAttribute name x = MkAttribute name name False (return x)

mkPrivateAttribute :: Monad m => AttributeName -> Invocant m -> Attribute m
mkPrivateAttribute name x = MkAttribute name name True (return x)
