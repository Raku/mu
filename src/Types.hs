{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans #-}

{-
    Implementation Types.

    Three Rings for the Elven-kings under the sky,
    Seven for the Dwarf-lords in their halls of stone,
    Nine for Mortal Men doomed to die,
    One for the Dark Lord on his dark throne
    In the Land of Mordor where the Shadows lie.
-}

module Types where
import Internals

type Var   = String             -- Variable name
type VStr  = String
type VBool = Bool
type VInt  = Integer
type VRat  = Rational
type VNum  = Double
type VComplex = Complex VNum
type VHandle = Handle
type VSocket = Socket
type VThread = ThreadId
data VRule     = MkRule
    { rxRegex     :: Regex
    , rxGlobal    :: Bool
    }
    deriving (Show, Eq, Ord)

instance Ord VHandle where
    compare x y = compare (show x) (show y)
instance Ord VSocket where
    compare x y = compare (show x) (show y)
