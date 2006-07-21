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
    makeDefs   = text "showPerl6TypeDef _" <+> equals <+> text "unlines" $$ (nest 8 $ commaList defs)
    defs       = roleDef : classDefs
    roleDef    = text "showPerl6RoleDef" <+> (dq $ text $ role)
    classDefs  = map (makeClassDef role) (body d)
    role       = name d

makeClassDef role bod@(Body constructor labels types) =
    hsep [text "showPerl6ClassDef", qt role, qt constructor, mkAllAttr]
    where
    mkAllAttr = text $ show $ zipWith3 (\t s n -> (t, dq $ s<>n)) types' sigils names'
    mkPosAttr = varNames types
    mkRecAttr = map text labels
    types'    = map (qt . snd . p6Type) types
    sigils    = map (text . fst . p6Type) types
    names'    = if null labels then mkPosAttr else mkRecAttr
    p6Type (Con ty)        = ("$.", ty) -- XXX should be: lookup the type in some Hs->P6 map
    p6Type (List (Con ty)) = ("@.", ty) -- simple list
    p6Type (List {})       = ("@.", "") -- too deep for a P6 constraint, for now.
    p6Type x               = error $ "don't know what to do with " ++ (show x)

makeAsObject bod@(Body constructor labels types)
    | null types  = empty
    | null labels = hsep [text "asPerl6Object", (parens $ hsep (text constructor : varNames types)), equals, text "error", qt "not yet"]
    | otherwise   = hsep [text "asPerl6Object", text constructor, text "{}", equals,
        text "error $", qt "not yet: ", text "++", text (show (show bod))]

dq    = doubleQuotes
qt    = dq . text

commaList, statementList :: [Doc] -> Doc
-- | punctuation-first list data
commaList     = genSeq lbrack rbrack comma
-- | punctuation-first statement block
statementList = genSeq lbrace rbrace semi

{-| generate a punctuation-first style sequence, such as this:
     [ firstElem
     , secondElem
     , thirdElem
     ]
Use @commaList@ for the above, or @statementList@ for layout-less
explicitly delimited blocks.
-}
genSeq :: Doc -> Doc -> Doc -> [Doc] -> Doc
genSeq lft rht _   []     = lft $$ rht
genSeq lft rht del (x:xs) = lft <+> x $$ (vcat $ map (del <+>) xs) $$ rht


