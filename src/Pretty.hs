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

-- Unmatched right curly bracket at - line 1, at end of line
-- syntax error at - line 1, near "}"
-- Execution of - aborted due to compilation errors.

instance Pretty VStr

instance Pretty Exp where
    pretty (Val (VError msg (NonTerm pos))) = "Syntax error at " ++ (show pos) ++ msg
    pretty (Val v) = pretty v
    pretty (Syn x vs) = "Syn " ++ pretty x ++ " {{ " ++ joinList "; " (map pretty vs) ++ " }}"
    pretty x = show x

instance Pretty Env where
    pretty x = "{ " ++ (pretty $ envBody x) ++ " }"

instance Pretty (Val, Val) where
    pretty (x, y) = pretty x ++ " => " ++ pretty y

instance Pretty Val where
    pretty (VJunc (Junc j dups vals)) = "(" ++ joinList mark items ++ ")"
        where
        items = map pretty $ values
        values = setToList vals ++ (concatMap (replicate 2)) (setToList dups)
        mark  = case j of
            JAny  -> " | "
            JAll  -> " & "
            JOne  -> " ^ "
            JNone -> " ! "
    pretty (VPair x y) = "(" ++ pretty (x, y) ++ ")"
    pretty (VBool x) = if x then "bool::true" else "bool::false"
    pretty (VNum x) = if x == 1/0 then "Inf" else show x
    pretty (VInt x) = show x
    pretty (VStr x) = show x -- XXX escaping
    pretty (VRat x) = show $ (fromIntegral $ numerator x) / (fromIntegral $ denominator x)
    pretty (VComplex x) = show x
    pretty (VRef (VList x))
        | not . null . (drop 100) $ x
        = "[" ++ pretty (head x) ++ ", ...]"
        | otherwise = "[" ++ joinList ", " (map pretty x) ++ "]"
    pretty (VRef x) = "\\(" ++ pretty x ++ ")"
    pretty (VList x)
        | not . null . (drop 100) $ x
        = "(" ++ pretty (head x) ++ ", ...)"
        | otherwise = "(" ++ joinList ", " (map pretty x) ++ ")"
    pretty (VSub x) = "sub {...}"
    pretty (VBlock x) = "{...}"
    pretty (VError x y) = "*** Error: " ++ x ++ "\n    in " ++ show y
    pretty (VArray (MkArray x)) = pretty (VList x)
    pretty (VHash (MkHash x)) = "{" ++ joinList ", " (map pretty $ fmToList x) ++ "}"
    pretty (VHandle x) = show x
    pretty (MVal x) = "<mval>" -- pretty $ castV x
    pretty VUndef = "undef"

joinList x y = concat $ intersperse x y

