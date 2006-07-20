{-# OPTIONS_GHC -fglasgow-exts -funbox-strict-fields -fallow-overlapping-instances -fvia-C #-}

module DrIFT.Perl6Class where

showPerl6RoleDef :: String -> String
showPerl6RoleDef name =
    "role " ++ name ++ " is TaggedUnion;\n"

showPerl6ClassHead, showSimplePerl6ClassDef :: String -> String -> String
showSimplePerl6ClassDef role name =
    "class " ++ name ++ " does " ++ role ++ ";\n"

showPerl6ClassHead role name =
    "class " ++ name ++ " does " ++ role ++ " {\n"

class Perl6Class a where
    asPerl6Object :: a -> String
    asPerl6Object simple = "new " ++ show simple

-- hw factor high; probably needs to got back to the preprocessor.
--showNewPerl6PosObject cls [attr] = cls ++ ".new(" ++ (intersperse "," $ map dynShow attr) ++ ")"

