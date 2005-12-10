{-# OPTIONS_GHC -fglasgow-exts #-}
{-# OPTIONS_GHC -#include "../UnicodeC.h" #-}

{-|
    Pretty printing for various data structures.

>   Galadriel! Galadriel!
>   Clear is the water of your well;
>   White is the stars in your white hand;
>   Unmarred, unstained is leaf and land
>   In Dwimordene, in Lorien
>   More fair than thoughts of Mortal Men.
-}

module Pugs.Pretty (
    Pretty, pretty,
) where
import Pugs.Internals
import Pugs.Types
import Pugs.AST
import Text.PrettyPrint
import qualified Data.Set as Set
import qualified Data.Map as Map

defaultIndent :: Int
defaultIndent = 2

class (Show a) => Pretty a where
    format :: a -> Doc
    format x = text $ show x

instance Pretty VStr

instance Pretty Exp where
    format (NonTerm pos) = text "Syntax error at" <+> format pos
    format (Val v) = format v
    format (Syn x vs) = text "Syn" <+> format x <+> (braces $ vcat (punctuate (text ";") (map format vs)))
    format (Stmts exp1 exp2) = (vcat $ punctuate (text ";") $ (map format) [exp1, exp2])
    format (App (Var name) invs args) = text "App" <+> text name <+> parens (nest defaultIndent $ cat (punctuate (text ": ") [ cat (punctuate (text ", ") (map format x)) | x <- [maybeToList invs, args] ]))
    format (App sub invs args) = text "App" <+> parens (format sub) <+> parens (nest defaultIndent $ vcat (punctuate (text ", ") (map format $ maybeToList invs ++ args)))
    format (Sym scope name exp) = text "Sym" <+> text (show scope) <+> format name $+$ format exp
    format (Pad scope pad exp) = text "Pad" <+> text (show scope) <+> format pad $+$ format exp
    format (Ann _ exp) = format exp
    format x = text $ show x

instance Pretty (TVar Bool, TVar VRef) where
    format (_, tvar) = format tvar

instance Pretty Pad where
    format pad = vcat $ map formatAssoc $ padToList pad
        where
        formatAssoc (name, var) = format name <+> text ":=" <+> (nest defaultIndent $ vcat $ map format var)

instance Pretty Pos where
    format pos =
        let file = posName pos
            bln  = show $ posBeginLine pos
            bcl  = show $ posBeginColumn pos
            eln  = show $ posEndLine pos
            ecl  = show $ posEndColumn pos
            fmt ln cl = text "line" <+> text ln <> comma <+> text "column" <+> text cl
        in text file <+> case (bln == eln, bcl == ecl) of
            (True, True)  -> fmt bln bcl
            (True, False) -> fmt bln (bcl ++ "-" ++ ecl)
            (False, _)    -> fmt bln bcl <+> (text "-" <+> fmt eln ecl)

instance Pretty SubType where
    format = text . map toLower . drop 3 . show

instance Pretty Env where
    format x = doubleBraces $ nest defaultIndent (format $ envBody x) 

instance Pretty (Val, Val) where
    format (x, y) = hang (format x <+> text "=>") defaultIndent (format y)

instance Pretty (Exp, SourcePos) where
    format (x, _) = format x 

instance Pretty (TVar VRef) where
    format x = braces $ text $ "ref:" ++ show x

instance Pretty VRef where
    format x = braces $ text $ "ref:" ++ show x

instance Pretty VMatch where
    format m = joinList (text ", ")
        [ form ("ok",        matchOk)
        , form ("from",      matchFrom)
        , form ("to",        matchTo)
        , form ("str",       matchStr)
        , form ("sub_pos",   matchSubPos)
        , form ("sub_named", matchSubNamed)
        ]
        where
        form :: Pretty a => (String, VMatch -> a) -> Doc
        form (s, f) = hang (text s <+> text "=>") defaultIndent (format $ f m)

instance Pretty Bool where
    format x = text $ if x then "bool::true" else "bool::false"

instance Pretty Int where
    format i = int i

instance Pretty VList where
    format [x] = parens $ format x <> text ","
    format xs
        | not . null . (drop 100) $ xs = parens $ (format (head xs) <+> text ", ...")
        | otherwise = parens $ (joinList $ text ", ") (map format xs)

instance Pretty VHash where
    format x = cat
        [ text "{"
        , nest defaultIndent . joinList (text ", ") $
            [ format (VStr k, v) | (k, v) <- Map.toList x ]
        , text "}"
        ]

instance Pretty Val where
    format (VJunc j) = parens $ joinList mark items 
        where
        dups = juncDup j
        vals = juncSet j
        items = map format $ values
        values = Set.elems vals ++ (concatMap (replicate 2)) (Set.elems dups)
        mark  = case juncType j of
            JAny  -> text " | "
            JAll  -> text " & "
            JOne  -> text " ^ "
            JNone -> text " ! "
    format (VBool x) = format x
    format (VNum x) = if x == 1/0
                         then text "Inf"
                         else if x == -1/0 
                                then text "-Inf"
                                else text $ show x
    format (VInt x) = integer x
    format (VStr x) = text $ "\"" ++ encodeUTF8 (concatMap quoted x) ++ "\""
    format (VRat x) = text $ showTrueRat x
    format (VComplex x) = text $ show x
    format (VControl (ControlEnv _)) = text "<env>"
    format (VControl x) = text $ show x
    format (VProcess x) = text $ show x
    format (VOpaque (MkOpaque x)) = braces $ text $ "obj:" ++ show x
{-
    format (VRef (VList x))
        | not . null . (drop 100) $ x
        = brackets $ format (head x) <+> text ", ..."
        | otherwise = brackets $ cat $ (punctuate $ text ", ") (map format x)
-}
    format (VRef x) = format x
    format (VList x) = format x
    format (VCode _) = text "sub {...}"
    format (VBlock _) = text "{...}"
    format (VError x posList)
        -- Is this correct? Does this work on win32, too?
        | last s == '\n' = text . init $ s
        | otherwise      = text "***" <+>
            (vcat (map text $ split "\n" s) $+$ (text "at" <+> vcat (map format $ reverse posList)))
        where
        s = case x of
              VStr s' -> s'
              _       -> pretty x
--  format (VArray x) = format (VList $ Array.elems x)
--  format (VHash h) = braces $ (joinList $ text ", ") $
--      [ format (VStr k, v) | (k, v) <- Map.toList h ]
    format (VHandle x) = text $ show x
    format (VThread t) = text $ takeWhile isDigit $ dropWhile (not . isDigit) $ show t
    format (VSocket x) = text $ show x
    -- format (MVal v) = text $ unsafePerformIO $ do
    --     val <- readTVar v
    --     return $ pretty val
    format (VRule _) = text $ "{rule}"
    format (VSubst _) = text $ "{subst}"
    format (VType t) = text $ "::" ++ showType t
    format (VObject o) = text $ "{obj:" ++ showType (objType o) ++ "}"
    format (VMatch m) = cat
        [ text "Match.new("
        , nest defaultIndent $ format m
        , text ")"
        ]
    format (PerlSV _) = text $ "{obj-perl5}"
    format VUndef = text $ "undef"

quoted :: Char -> String
quoted '\'' = "\\'"
quoted '\\' = "\\\\"
quoted '"'  = "\\\""
quoted '{'  = "\\{"
quoted '\t' = "\\t"
quoted '\r' = "\\r"
quoted '\n' = "\\n"
quoted '$'  = "\\$"
quoted '@'  = "\\@"
quoted '%'  = "\\%"
quoted '&'  = "\\&"
quoted '^'  = "\\^"
quoted x | x < ' ' || x > '~' = "\\d[" ++ show (ord x) ++ "]"
quoted x = [x]

doubleBraces :: Doc -> Doc
doubleBraces x = vcat [ (lbrace <> lbrace), nest defaultIndent x, rbrace <> rbrace]

joinList :: Doc -> [Doc] -> Doc
joinList x y = cat $ punctuate x y

pretty :: Pretty a => a -> String
pretty a = render $ format a

