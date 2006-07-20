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

