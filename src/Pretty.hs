{-# OPTIONS -fglasgow-exts #-}

{-
    Pretty printing for various data structures.

    Galadriel! Galadriel!
    Clear is the water of your well;
    White is the stars in your white hand;
    Unmarred, unstained is leaf and land
    In Dwimordene, in Lorien
    More fair than thoughts of Mortal Men.
-}

module Pretty where
import Internals
import AST

class (Show a) => Pretty a where
    pretty :: a -> String
    pretty x = show x

instance Pretty Val where
    pretty (VJunc j l) = "(" ++ joinList mark items ++ ")"
        where
        items = map pretty l
        mark  = case j of
            JAny  -> " | "
            JAll  -> " & "
            JOne  -> " ^ "
            JNone -> " ! "
    pretty (VPair x y) = "(" ++ pretty x ++ " => " ++ pretty y ++ ")"
    pretty (VBool x) = if x then "#t" else "#f"
    pretty (VNum x) = if x == 1/0 then "Inf" else show x
    pretty (VInt x) = show x
    pretty (VStr x) = show x -- XXX escaping
    pretty (VRat x) = show $ (fromIntegral $ numerator x) / (fromIntegral $ denominator x)
    pretty (VComplex x) = show x
    pretty (VRef (VList x)) = "[" ++ joinList ", " (map pretty x) ++ "]"
    pretty (VRef x) = "\\(" ++ pretty x ++ ")"
    pretty (VList x) = "(" ++ joinList ", " (map pretty x) ++ ")"
    pretty (VSub x) = "sub {...}"
    pretty (VBlock x) = "{...}"
    pretty (VError x y) = "*** Error: " ++ x ++ "\n    in " ++ show y
    pretty (VArray (MkArray x)) = pretty (VList x)
    pretty VUndef = "undef"

joinList x y = concat $ intersperse x y

