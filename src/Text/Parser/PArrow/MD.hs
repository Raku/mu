{-# OPTIONS_GHC -O2 -fglasgow-exts #-}
module Text.Parser.PArrow.MD (
    MD(..), Label(..), label, mkLabel, Monoid(..),
    MStarRes, MinQuant, MaxQuant(..),
) where

import Control.Arrow
import Data.FastPackedString (FastString, pack, cons, snoc, unpack)
import Data.List(intersperse, unzip)
import GHC.Prim(unsafeCoerce#)
import Text.Parser.PArrow.CharSet
import Data.Set (Set, empty, singleton, unions, union)
import qualified Data.Set as Set
import Data.Seq (Seq)
import Data.Monoid
import Data.Generics

data Label = MkLabel
    { expects   :: !(Set FastString)
    , unexpects :: !(Set FastString)
    }
    deriving (Eq, Ord, Data, Typeable)

instance Show Label where
    show (MkLabel es us) = showSet (\x -> "<!" ++ x ++ ">") us ++ showSet id es

showSet :: (String -> String) -> Set FastString -> String
showSet f s = case Set.toAscList s of
    []  -> ""
    [x] -> f (unpack x)
    xs  -> f (concat (intersperse "|" (map unpack xs)))

instance Monoid Label where
    mempty = MkLabel Set.empty Set.empty
    mappend (MkLabel e1 u1) (MkLabel e2 u2) = MkLabel (e1 `union` e2) (u1 `union` u2)
    mconcat xs = MkLabel (unions es) (unions us)
        where
        (es, us) = unzip [ (e, u) | MkLabel e u <- xs ]

mkLabel :: FastString -> Label
mkLabel s = MkLabel (singleton s) Set.empty

label :: MD i o -> Label
label (MNot x)        = let MkLabel e u = label x in MkLabel u e
label (MChoice xs)    = mconcat (map label xs)
label (MEqual s)      = mkLabel ('\'' `cons` s `snoc` '\'')
label (MSeq x _)      = label x -- XXX (label y) 
label (MEmpty)        = mempty
label (MPure l _)     = l
label (MCSet c)       = MkLabel (singleton (pack (show c))) Set.empty
label (MParWire x y)  = mappend (label x) (label y) 
label (MJoin x y)     = mappend (label x) (label y) 
label (MGreedy _ _ x) = label x
label (MLazy _ _ x)   = label x

type MStarRes o = Either FastString (Seq o)

data MD i o where 
    MNot    :: MD i o -> MD i o
    MChoice :: [MD i o] -> MD i o
    MEqual  :: FastString -> MD i FastString
    MSeq    :: MD i t -> MD t o -> MD i o
    MEmpty  :: MD i o
    MPure   :: Label -> (i->o) -> MD i o
    MCSet   :: CharSet -> MD i FastString
    MParWire:: MD i1 o1 -> MD i2 o2 -> MD (i1,i2) (o1,o2)
    MJoin   :: MD i o1  -> MD i o2  -> MD i (o1,o2)
    -- Quantifiers
    MGreedy :: MinQuant -> MaxQuant -> MD i o -> MD i (MStarRes o)
    MLazy   :: MinQuant -> MaxQuant -> MD i o -> MD i (MStarRes o)

type MinQuant = Int
data MaxQuant = QuantInt !Int | QuantInf
    deriving (Show, Eq, Ord, Data, Typeable)

instance Eq (MD i o) where
    MNot a    == MNot b    = a == b
    MChoice a == MChoice b = a == b
    MEqual c  == MEqual d  = c == d
    MSeq a b  == MSeq c d  = a == force c && b == force d
    MGreedy a b c == MGreedy a' b' c' = a == a' && b == b' && c == c'
    MLazy a b c   == MLazy   a' b' c' = a == a' && b == b' && c == c'
    MEmpty    == MEmpty    = True
    MPure s _ == MPure t _ = s == t
    MCSet s   == MCSet t   = s == t
    MParWire a b == MParWire c d = a == c && b == d
    MJoin a b == MJoin c d = a == c && b == d
    _         == _         = False

force :: MD a b -> MD c d 
force = unsafeCoerce#

instance Arrow MD where
    arr f    = MPure mempty f
    a >>> b  = a `MSeq` b
    a *** b  = a `MParWire` b
    a &&& b  = a `MJoin` b
    first a  = a `MParWire` (mempty `MPure` id)
    second b = (mempty `MPure` id) `MParWire` b

instance ArrowZero MD where
    zeroArrow = MEmpty

instance ArrowPlus MD where
    MChoice a <+> MChoice b = MChoice (a++b)
    a         <+> MChoice b = MChoice (a:b)
    a         <+> b         = MChoice [a,b]

instance Show (MD i o) where 
    show (MEqual c)    = show c -- if c `elem` ".[]{}|" then ['\\',c] else [c]
    show (MPure s _)   = "{" ++ show s ++ "}"
--    show (MPure s _)   = ""
    show (MCSet s)     = show s
    show (MNot p)      = "[^"++show p++"]"
    show (MChoice m)   = if all simple m 
                           then "["++concatMap show m++"]"
                           else "(" ++ concat (intersperse "|" $ map show m) ++ ")"
    show (MSeq a b)    = "("++show a ++ "~~" ++ show b ++ ")"
    show (MEmpty)      = ""
    show (MJoin a b)   = show a ++ " >> " ++ show b
    show (MParWire a b)= show a ++ " || " ++ show b
    show (MGreedy min max x) = show x ++ "**{" ++ show min ++ ".." ++ showMax max ++ "}"
        where
        showMax QuantInf = ""
        showMax (QuantInt i) = show i
    show (MLazy min max x) = show (MGreedy min max x) ++ "?"

-- for formatting
simple :: MD i o -> Bool
simple (MPure _ _) = True
simple (MCSet _)   = True
simple (MEqual _)  = True
simple (MChoice m) = all simple m
simple _ = False
