{-# OPTIONS_GHC -fglasgow-exts -funbox-strict-fields -fallow-overlapping-instances -fvia-C #-}

module DrIFT.Perl6Class where

showPerl6RoleDef :: String -> String        -- ^ Perl 6 role definition
showPerl6RoleDef name =
    "role " ++ name ++ " is TaggedUnion;\n"

doShowPerl6ClassDef :: String               -- ^ role name (Hs datatype)
                    -> String               -- ^ class name (Hs variant)
                    -> [(String -> String)] -- ^ member type+name pairs
                    -> String               -- ^ Perl 6 class definition
doShowPerl6ClassDef role constructor members =
    unlines $ [clsHead] ++ (map memberDef members) ++ [clsTail]
    where
    clsHead = "class " ++ name ++ " does " ++ role ++ " {"
    clsTail = "}"
    memberDef (ty, nm) = "\thas " ++ ty ++ " $." ++ nm ++ ";"

class Perl6Class a where
    asPerl6Object :: a -> String
    asPerl6Object simple = "new " ++ show simple

-- hw factor high; probably needs to got back to the preprocessor.
--showNewPerl6PosObject cls [attr] = cls ++ ".new(" ++ (intersperse "," $ map dynShow attr) ++ ")"

-- | typeclass for dumping literals in Perl 6 source code.
class Show a => PLit a where
    plShow :: a -> String
    plShow = show

instance PLit String where
    plShow s = "q\"" ++ (show s) ++ "'" -- XXX: wrong.

