{-# OPTIONS_GHC -fglasgow-exts -funbox-strict-fields -fallow-overlapping-instances -fvia-C #-}

module DrIFT.Perl6Class where

import Data.Typeable
import Text.PrettyPrint.HughesPJ

showPerl6RoleDef, showMooseRoleDef
                    :: NamespaceMangler -> String -> String           -- ^ Perl 6 role definition
showPerl6RoleDef ns name = render $
    hsep $ map text ["role", ns name, "is TaggedUnion;"]

showMooseRoleDef ns name = render $
    vcat [ text "package" <+> (text $ ns name) <> semi
         , text "use Moose::Role;"
         , empty
         , text "with 'TaggedUnion';"
         , empty
         ]

showPerl6ClassDef, showMooseClassDef
                    :: NamespaceMangler           -- ^ (e.g, ("v6::AST::" ++))
                    -> String                     -- ^ role name (Hs datatype)
                    -> String                     -- ^ class name (Hs variant)
                    -> [(String, String, String)] -- ^ member type+name pairs
                    -> String                     -- ^ Perl 6 class definition
showPerl6ClassDef ns role cls members = render $
    catF [clsHead, mem, clsTail]
    where
    mem = nest 4 $ vcat $ map memberDef members
    catF | null members = cat   -- can't emit oneliner classes w/"has" because
         | otherwise    = vcat  -- sometimes there are '#'-style comments.
    clsHead = hsep $ map text ["class", ns cls, "does", ns role, "{"]
    clsTail = rbrace <> semi
    memberDef (ty, nm, ann) = hsep [text "has", ty' ty, text nm] <> semi <+> annComment ann
    ty' x   = if null x then empty else text x
    annComment x = if null x then empty else text "#" <+> text x

showMooseClassDef ns role cls members = render $
    clsHead $+$ mem $+$ text ""
    where
    mem = vcat $ map memberDef members
    clsHead = vcat
        [ text "package" <+> (text $ ns cls) <> semi
        , text "use Moose;"
        , text ""
        , text "extends" <+> quotes (text $ ns role) <> semi
        , text ""
        ]
    memberDef (ty, nm, ann) = hsep [text "has", qt nm, text "=>"] <+>
        (parens $ hsep [text "is 'rw',", ty' ty]) <> semi <+> annComment ann
    ty' x   = if null x then empty else text "isa =>" <+> (quotes $ text x)
    annComment x = if null x then empty else text "#" <+> text x

qt :: String -> Doc
qt = doubleQuotes . text

type NamespaceMangler = String -> String

class Typeable a => MooseClass a where
    showMooseTypeDef :: NamespaceMangler -> a -> String

class Typeable a => Perl6Class a where
    showPerl6TypeDef :: NamespaceMangler -> a -> String
    asPerl6Object :: a -> String
    asPerl6Object simple = "new " ++ (show $ typeOf simple)

-- hw factor high; probably needs to got back to the preprocessor.
--showNewPerl6PosObject cls [attr] = cls ++ ".new(" ++ (intersperse "," $ map dynShow attr) ++ ")"

-- | typeclass for dumping literals in Perl 6 source code.
class Show a => PLit a where
    plShow :: a -> String
    plShow = show

instance PLit String where
    plShow s = "q\"" ++ (show s) ++ "\"" -- XXX: wrong.

