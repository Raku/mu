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
    functions = makeDefs : concatMap f ii
    f (i,dflt) = map i (body d) ++ [dflt $ body d]      
    makeDefs = text "showPerl6TypeDef _" <+> equals <+> text "concat" <+> (brackets $ fsep $ punctuate comma defs)
    defs = roleDef : classDefs
    roleDef = text "showPerl6RoleDef" <+> (dq $ text $ role)
    classDefs = map (makeClassDef role) (body d)
    role = name d

makeClassDef role bod@(Body constructor labels types)
    | null types  = hsep [text "showSimplePerl6ClassDef", qt role, qt constructor]
    | null labels = hsep [clsHead, text "++", mkPosAttr, text "++", clsTail]
    | otherwise   = hsep [clsHead, text "++", mkRecAttr, text "++", clsTail]
    where
    mkPosAttr      = mkAllAttr $ varNames types
    mkRecAttr      = mkAllAttr $ map text labels
    mkAllAttr vars = hsep $ intersperse (text "++") $ map mkAttr $ zip types vars
    mkAttr ((Con ty), nm) = dq $ text "has" <+> p6Type ty <+> text "$" <> nm <> text ";\\n"
    p6Type = text -- XXX should be: text . lookup the type in some Hs->P6 map
    clsHead = parens $ hsep [text "showPerl6ClassHead", qt role, qt constructor]
    clsTail = qt "}\\n"

makeAsObject bod@(Body constructor labels types)
    | null types  = empty
    | null labels = hsep [text "asPerl6Object", (parens $ hsep (qt constructor : varNames types)), equals, text "error", qt "not yet"]
    | otherwise   = text "error $" <+> qt "not yet: " <+> text "++" <+> text (show (show bod))

dq = doubleQuotes
qt = dq . text

