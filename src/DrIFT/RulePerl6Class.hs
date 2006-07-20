{-# OPTIONS_GHC -fglasgow-exts #-}
module RulePerl6Class (rules) where

import RuleUtils
import List 
import GenUtil

type AlwaysPositional = Bool

rules =
    [ ("Perl6Class", userRulePerl6Class, "Representation", "serialize into Perl 6 classes", Nothing)
    ]

userRulePerl6Class = instanceSkeleton' "Perl6Class"
    [ (makeAsObject, const empty)
    ]
{-
    [ (const empty, caseHead)
    , (makeFromYAML alwaysPos, const empty)
    , (const empty, caseTail)
    , (makeAsYAML alwaysPos, const empty)
    ]
-}

instanceSkeleton' :: Class -> [(IFunction,[Body] -> Doc)] -> Data -> Doc
instanceSkeleton' s ii  d = (simpleInstance s d <+> text "where") 
                $$ block functions
    where
    functions  = makeDefs : concatMap f ii
    f (i,dflt) = map i (body d) ++ [dflt $ body d]
    makeDefs   = text "showPerl6TypeDef _" <+> equals <+> text "unlines" $$ (nest 8 $ mlist defs)
    defs       = roleDef : classDefs
    roleDef    = text "showPerl6RoleDef" <+> (dq $ text $ role)
    classDefs  = map (makeClassDef role) (body d)
    role       = name d

makeClassDef role bod@(Body constructor labels types) =
    hsep [text "showPerl6ClassDef", qt constructor, equals,
        text "doShowPerl6ClassDef", qt role, qt constructor, text $ show mkAllAttr]
    where
    mkAllAttr = zip types' names'
    mkPosAttr = varNames types
    mkRecAttr = map text labels
    types'    = map (qt . p6Type) types
    names'    = map dq (if null labels then mkPosAttr else mkRecAttr)
    p6Type (Con ty) = ty -- XXX should be: lookup the type in some Hs->P6 map

makeAsObject bod@(Body constructor labels types)
    | null types  = empty
    | null labels = hsep [text "asPerl6Object", (parens $ hsep (qt constructor : varNames types)), equals, text "error", qt "not yet"]
    | otherwise   = text "asPerl6Object _" <+> equals <+> text "error $" <+> qt "not yet: " <+> text "++" <+> text (show (show bod))

dq    = doubleQuotes
qt    = dq . text
mlist [] = lbrack $$ rbrack
mlist (x:xs) = lbrack <+> x $$ (vcat $ map (\e -> comma <+> e) xs) $$ rbrack
