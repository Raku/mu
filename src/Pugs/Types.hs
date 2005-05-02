{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans -funbox-strict-fields #-}

{-
    Implementation Types.

    Three Rings for the Elven-kings under the sky,
    Seven for the Dwarf-lords in their halls of stone,
    Nine for Mortal Men doomed to die,
    One for the Dark Lord on his dark throne
    In the Land of Mordor where the Shadows lie.
-}

module Pugs.Types where
import Pugs.Internals

data Type
    = MkType !String
    | TypeOr  !Type !Type
    | TypeAnd !Type !Type
    deriving (Eq, Ord)

instance Show Type where
    show t = "(mkType \"" ++ showType t ++ "\")"

showType :: Type -> String
showType (MkType typ)    = typ
showType (TypeOr t1 t2)  = showType t1 ++ "|" ++ showType t2
showType (TypeAnd t1 t2) = showType t1 ++ "&" ++ showType t2

type ClassTree = Tree Type

data Cxt = CxtVoid | CxtItem !Type | CxtSlurpy !Type
    deriving (Eq, Show, Ord)

anyType :: Type
anyType = mkType "Any"

cxtItem   :: String -> Cxt
cxtItem   = CxtItem . mkType
cxtSlurpy :: String -> Cxt
cxtSlurpy = CxtItem . mkType
cxtVoid   :: Cxt
cxtVoid   = CxtVoid

typeOfCxt :: Cxt -> Type
typeOfCxt CxtVoid           = anyType
typeOfCxt (CxtItem typ)     = typ
typeOfCxt (CxtSlurpy typ)   = typ

cxtItemAny :: Cxt
cxtItemAny   = CxtItem anyType
cxtSlurpyAny :: Cxt
cxtSlurpyAny = CxtSlurpy anyType

isSlurpyCxt :: Cxt -> Bool
isSlurpyCxt (CxtSlurpy _) = True
isSlurpyCxt _             = False
isItemCxt :: Cxt -> Bool
isItemCxt   (CxtItem _)   = True
isItemCxt   _             = False
isVoidCxt :: Cxt -> Bool
isVoidCxt   CxtVoid       = True
isVoidCxt   _             = False

mkType :: String -> Type
mkType str
    | (t1, (_:t2)) <- span (/= '|') str
    = TypeOr (MkType t1) (mkType t2)
    | (t1, (_:t2)) <- span (/= '&') str
    = TypeAnd (MkType t1) (mkType t2)
    | otherwise
    = MkType str

-- |Variable name
type Var   = String
-- |Uses Haskell's underlying representation
type VStr  = String
-- |Uses Haskell's underlying representation
type VBool = Bool
-- |Uses Haskell's underlying representation
type VInt  = Integer
-- |Uses Haskell's underlying representation
type VRat  = Rational
-- |Uses Haskell's underlying representation
type VNum  = Double
-- |Uses Haskell's underlying representation
type VComplex = Complex VNum
-- |Uses Haskell's underlying representation
type VHandle = Handle
-- |Uses Haskell's underlying representation
type VSocket = Socket
-- |Uses Haskell's underlying representation
type VThread = ThreadId
data VRule     = MkRule
    { rxRegex     :: !Regex
    , rxGlobal    :: !Bool
    }
    deriving (Show, Eq, Ord)

instance Ord VHandle where
    compare x y = compare (show x) (show y)
instance Ord VSocket where
    compare x y = compare (show x) (show y)
instance (Ord a) => Ord (Tree a) where
    compare _ _ = EQ
