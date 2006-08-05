{-# OPTIONS_GHC -fglasgow-exts #-}
module RulePerl6Class (rules) where

import RuleUtils
import List 
import GenUtil

type AlwaysPositional = Bool

rules =
    [ ("Perl6Class", userRulePerl6Class, "Representation", "serialize into Perl 6 classes", Nothing)
    , ("MooseClass", userRuleMoose, "Representation", "serialize into Moose Perl 5 classes", Nothing)
    ]

userRulePerl6Class = instanceSkeleton' "Perl6Class"
    [ (makeAsObject, const empty)
    ]

userRuleMoose = instanceSkeleton' "MooseClass"
    [
    --[ (makeAsMooseObject, const empty)
    ]

instanceSkeleton' :: Class -> [(IFunction,[Body] -> Doc)] -> Data -> Doc
instanceSkeleton' s ii  d = (simpleInstance s d <+> text "where") 
                $$ block functions
    where
    (typeDefF, roleDefF, classDefF)
        | s == "Perl6Class" = ("showPerl6TypeDef", "showPerl6RoleDef", makePerl6ClassDef)
        | otherwise         = ("showMooseTypeDef", "showMooseRoleDef", makeMooseClassDef)
    functions  = makeDefs : concatMap f ii
    f (i,dflt) = map i (body d) ++ [dflt $ body d]
    makeDefs   = (text $ typeDefF ++ " ns _") <+> equals <+> text "unlines" $$ (nest 8 $ commaList defs)
    defs       = roleDef : classDefs
    roleDef    = (text $ roleDefF ++ " ns") <+> (dq $ text $ role)
    classDefs  = map (classDefF role) (body d)
    role       = name d

makePerl6ClassDef role bod@(Body constructor labels types) =
    hsep [text "showPerl6ClassDef ns", qt role, qt constructor, mkAllAttr]
    where
    mkAllAttr = text $ show $ zipWith4 (\t s n l -> (t, dq $ s<>n, show l)) types' sigils names' lossage
    mkPosAttr = varNames types
    mkRecAttr = map text labels
    types'    = map (qt   . r_typename . p6Type) types
    sigils    = map (text . r_sigil    . p6Type) types
    lossage   = map (text . r_lossage  . p6Type) types
    names'    = if null labels then mkPosAttr else mkRecAttr

makeMooseClassDef role bod@(Body constructor labels types) =
    hsep [text "showMooseClassDef ns", qt role, qt constructor, mkAllAttr]
    where
    mkAllAttr = text $ show $ zipWith3 (\t n l -> (t, dq $ n, show l)) types' names' lossage
    mkPosAttr = varNames types
    mkRecAttr = map text labels
    types'    = map (qt   . m_typename . mType) types
    lossage   = map (text . m_lossage  . mType) types
    names'    = if null labels then mkPosAttr else mkRecAttr

data P6TypeRep = MkRep
    { r_sigil    :: String
    , r_typename :: String
    , r_lossage  :: String
    }

p6Type :: Type -> P6TypeRep
p6Type (Con ty)                       = MkRep "$." ty "" -- XXX should be: lookup the type in some Hs->P6 map
p6Type (List (Con ty))                = MkRep "@." ty "" -- simple list
p6Type ty@(List {})                   = MkRep "@." "" $ show ty -- too deep for a simple P6 constraint
p6Type (LApply (Con "Maybe") (ty:[])) = p6Type ty  -- drop Maybe silently
p6Type ty                             = MkRep "$." "" $ show ty

data MooseTypeRep = MkMooseRep
    { m_typename :: String
    , m_lossage  :: String
    }

mType :: Type -> MooseTypeRep
mType (Con ty)                       = MkMooseRep ty "" -- XXX should be: lookup the type in some Hs->P6 map
mType ty@(List {})                   = MkMooseRep "ArrayRef" $ show ty
mType (LApply (Con "Maybe") (ty:[])) = mType ty  -- drop Maybe silently
mType ty                             = MkMooseRep "" $ show ty


makeAsObject bod@(Body constructor labels types) =
    hsep [text "asPerl6Object", (parens $ hsep (text constructor : varNames types)), equals,
        qt $ constructor ++ ".new(", paste, mkPos, paste, qt ")"]
    where
    mkPos         = parens $ (text "concat $ intersperse \", \"") <+> mkPosElems
    mkPosElems    = brackets $ hsep $ punctuate comma $ map mkPosElem $ varNames types
    mkPosElem var = text "plShow" <+> var
    paste = text "++"

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

