{-# OPTIONS_GHC -fglasgow-exts -funbox-strict-fields -fallow-overlapping-instances -fvia-C -fallow-incoherent-instances -fallow-undecidable-instances #-}

module DrIFT.Perl6Class where

import Data.Typeable
import Data.List
import qualified Data.Map as Map
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
    memberDef (ty, nm, ann) = hsep [text "has", ty' ty, nm' nm] <> semi <+> annComment ann
    ty' x   = if null x then empty else text x
    nm' (_:'_':n) = text n
    nm' n         = text n
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
    memberDef (ty, nm, ann) = hsep [text "has", nm' nm, text "=>"] <+>
        (parens $ hsep [text "is 'rw',", ty' ty]) <> semi <+> annComment ann
    ty' x   = if null x then empty else text "isa =>" <+> (quotes $ text x)
    nm' (_:'_':n) = qt n
    nm' n         = qt n
    annComment x = if null x then empty else text "#" <+> text x

qt :: String -> Doc
qt = doubleQuotes . text

type NamespaceMangler = String -> String

class Typeable a => MooseClass a where
    showMooseTypeDef :: NamespaceMangler -> a -> String

class PLit a => Perl6Class a where
    showPerl6TypeDef :: NamespaceMangler -> a -> String
    showPerl6TypeDef _ ty = error $ "showPerl6TypeDef " ++ (show $ typeOf ty)
    asPerl6Object :: a -> String
    asPerl6Object simple = "new " ++ (show $ typeOf simple)

instance Perl6Class a => Perl6Class [a] where
    showPerl6TypeDef _ ty = error $ "showPerl6TypeDef " ++ (show $ typeOf ty)
    asPerl6Object xs = (show $ typeOf xs) ++ ".new(" ++ (concat $ intersperse ", " $ map asPerl6Object xs) ++ ")"

instance (Perl6Class a, Perl6Class b, PLit a, PLit b, PLit (Map.Map a b)) => (Perl6Class (Map.Map a b)) where
    asPerl6Object h = render $ cat $ qbraces $ map (\(k, v) -> (text $ show $ plShow k) <+> qt "=>" <+> (text $ show $ plShow v)) $ Map.assocs h

qbraces :: [Doc] -> [Doc]
qbraces ls = doubleQuotes lbrace : ls ++ [doubleQuotes rbrace]

-- hw factor high; probably needs to got back to the preprocessor.
--showNewPerl6PosObject cls [attr] = cls ++ ".new(" ++ (intersperse "," $ map dynShow attr) ++ ")"

-- | typeclass for dumping literals in Perl 6 source code.
class (Typeable a, Show a) => PLit a where
    plShow :: a -> String
    plShow = show

instance PLit String where
    plShow = render . cat . showStringLiteral

instance PLit a => PLit [a] where
    plShow x = "[" ++ (concat $ intersperse ", " $ map plShow x) ++ "]"

instance PLit a => PLit (Maybe a) where
    plShow Nothing  = "undef"
    plShow (Just x) = plShow x

instance (Typeable a, Show a) => PLit a where
    plShow = show

-- TODO: fps this.
-- | Turn a string into source-code fitting Perl 6 string literal.
--   May result in code for concatenation of several such literals.
--   The restult is a [Doc] rather than a single String so that
--   calling pretty-printers can render linebreaks at the correct places
--   trivially with cat.
showStringLiteral :: String -> [Doc]
showStringLiteral str =
    intersperse catter $ map slQuote $ strQuoteSplit [] "" str
    where
    catter = text "~ \"'\" ~"
    -- XXX: this function could and should also do unicode quoting.
    strQuoteSplit :: [String] -> String -> String -> [String]
    strQuoteSplit qa sa ""       = qa ++ [sa]
    strQuoteSplit qa sa ('\'':xs) = strQuoteSplit (qa ++ [sa])  ""      xs
    strQuoteSplit qa sa (x:xs)   = strQuoteSplit qa       (sa++[x])  xs
    slQuote str = text "qn'" <> text str <> text "'"

