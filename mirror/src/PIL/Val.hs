{-# OPTIONS_GHC -fglasgow-exts #-}

module PIL.Val where
import PIL.Str
import PIL.Internals
import PIL.MetaModel
import PIL.Container
import Prelude hiding (Num)

-- | Any PIL expression can only evaluate to one of three value results.
data Val
    = Void
    | Item Item
    | List List
--  | Control Control
    deriving (Eq, Ord, Show, Typeable)

type List = [Item]

-- | 'Item' is either one of the five intrisic types, or an object.
data Item
    = Undef
    | Object Object
    -- Intrinsic types, according to S02.
    | Int Int | Num Num | Str Str | Ref Ref | Bit Bit
    -- These four below are apocryphal (not part of spec).
    | Pair Pair | Junc Junc | Type Type | Code Code 
    deriving (Eq, Ord, Show, Typeable)

-- | 'Ref' always points to a container; values are promoted to constant containers.
newtype Ref = MkRef { unRef :: Container }
    deriving (Eq, Ord, Show, Typeable)
data Pair = MkPair { pairKey :: Item, pairVal :: Container }
    deriving (Eq, Ord, Show, Typeable)

type Bit = Bool
type Num = Double
data Code = MkCode String -- XXX
    deriving (Eq, Ord, Show, Typeable)
data Control = MkControl -- XXX
    deriving (Eq, Ord, Show, Typeable)
data Junc = MkJunc -- XXX
    deriving (Eq, Ord, Show, Typeable)
newtype Type = MkType { unType :: String }
    deriving (Eq, Ord, Show, Typeable)

