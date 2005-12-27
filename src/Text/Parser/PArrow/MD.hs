{-# OPTIONS_GHC -O2 -fglasgow-exts #-}
module Text.Parser.PArrow.MD (MD(..)) where

import Control.Arrow
import Data.List(intersperse)
import GHC.Prim(unsafeCoerce#)
import Text.Parser.PArrow.CharSet

data MD i o where 
    MNot    :: MD i o -> MD i o
    MChoice :: [MD i o] -> MD i o
    MEqual  :: Char -> MD i Char
    MSeq    :: MD i t -> MD t o -> MD i o
    MStar   :: MD i o -> MD i [o]
    MEmpty  :: MD i o
    MPure   :: String -> (i->o) -> MD i o
    MCSet   :: CharSet -> MD i Char
    MParWire:: MD i1 o1 -> MD i2 o2 -> MD (i1,i2) (o1,o2)
    MJoin   :: MD i o1  -> MD i o2  -> MD i (o1,o2)

instance Eq (MD i o) where
    MNot a    == MNot b    = a == b
    MChoice a == MChoice b = a == b
    MEqual c  == MEqual d  = c == d
    MSeq a b  == MSeq c d  = a == force c && b == force d
    MStar a   == MStar b   = a == b
    MEmpty    == MEmpty    = True
    MPure s _ == MPure t _ = s == t
    MCSet s   == MCSet t   = s == t
    MParWire a b == MParWire c d = a == c && b == d
    MJoin a b == MJoin c d = a == c && b == d
    _         == _         = False

force :: MD a b -> MD c d 
force = unsafeCoerce#

instance Arrow MD where
    arr f    = MPure "" f
    a >>> b  = a `MSeq` b
    a *** b  = a `MParWire` b
    a &&& b  = a `MJoin` b
    first a  = a `MParWire` ("id" `MPure` id)
    second b = ("id" `MPure` id) `MParWire` b

instance ArrowZero MD where
    zeroArrow = MEmpty

instance ArrowPlus MD where
    MChoice a <+> MChoice b = MChoice (a++b)
    a         <+> MChoice b = MChoice (a:b)
    a         <+> b         = MChoice [a,b]



instance Show (MD i o) where 
    show (MEqual c)    = if c `elem` ".[]{}|" then ['\\',c] else [c]
    show (MPure s _)   = show s
--    show (MPure s _)   = ""
    show (MCSet s)     = show s
    show (MNot p)      = "[^"++show p++"]"
    show (MChoice m)   = if all simple m 
                           then "["++concatMap show m++"]"
                           else "(" ++ concat (intersperse "|" $ map show m) ++ ")"
    show (MSeq x (MStar y)) | x == force y = if simple x then show x ++ "+" else "("++show x++")+"
    show (MSeq a b)    = show a ++ show b
    show (MStar r)     = "("++show r++")*"
    show (MEmpty)      = ""
    show (MJoin a b)   = show a ++ " >> " ++ show b
    show (MParWire a b)= show a ++ " || " ++ show b

-- for formatting
simple :: MD i o -> Bool
simple (MPure _ _) = True
simple (MCSet _)   = True
simple (MEqual _)  = True
simple (MChoice m) = all simple m
simple _ = False
