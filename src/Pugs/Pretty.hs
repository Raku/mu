{-# OPTIONS_GHC -fglasgow-exts #-}

{-
    Pretty printing for various data structures.

    Galadriel! Galadriel!
    Clear is the water of your well;
    White is the stars in your white hand;
    Unmarred, unstained is leaf and land
    In Dwimordene, in Lorien
    More fair than thoughts of Mortal Men.
-}

module Pugs.Pretty where
import Pugs.Internals
import Pugs.Types
import Pugs.AST
import Text.PrettyPrint
import qualified Data.Set as Set

defaultIndent :: Int
defaultIndent = 2

class (Show a) => Pretty a where
    format :: a -> Doc
    format x = text $ show x

instance Pretty VStr

instance Pretty Exp where
    format (Val (VError msg (NonTerm pos))) = text "Syntax error at" <+> (format pos) <+> format msg
    format (NonTerm pos) = format pos
    format (Val v) = format v
    format (Syn x vs) = text "Syn" <+> format x <+> (braces $ vcat (punctuate (text ";") (map format vs)))
    format (Stmts exp1 exp2) = (vcat $ punctuate (text ";") $ (map format) [exp1, exp2])
    format (App sub invs args) = text "App" <+> format sub <+> parens (nest defaultIndent $ vcat (punctuate (text ", ") (map format $ invs ++ args)))
    format (Sym scope name exp) = text "Sym" <+> text (show scope) <+> format name $+$ format exp
    format (Pad scope pad exp) = text "Pad" <+> text (show scope) <+> format pad $+$ format exp
    format (Pos _ exp) = format exp
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

instance Pretty Env where
    format x = doubleBraces $ nest defaultIndent (format $ envBody x) 

instance Pretty (Val, Val) where
    format (x, y) = format x <+> text "=>" <+> format y

instance Pretty (Exp, SourcePos) where
    format (x, _) = format x 

instance Pretty (TVar VRef) where
    format x = braces $ text $ "ref:" ++ show x

instance Pretty VRef where
    format x = braces $ text $ "ref:" ++ show x

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
    format (VBool x) = if x then text "bool::true" else text "bool::false"
    format (VNum x) = if x == 1/0 then text "Inf" else text $ show x
    format (VInt x) = integer x
    format (VStr x) = text $ "'" ++ encodeUTF8 (concatMap quoted x) ++ "'"
    format v@(VRat _) = text $ vCast v
    format (VComplex x) = text $ show x
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
    format (VList x)
        | not . null . (drop 100) $ x
        = parens $ (format (head x) <+> text ", ...")
        | otherwise = parens $ (joinList $ text ", ") (map format x)
    format (VCode _) = text "sub {...}"
    format (VBlock _) = text "{...}"
    format (VError x y@(NonTerm _)) =
        text "*** Error:" <+> (text x <+> (text "at" <+> format y))
    format (VError x _) = text "*** Error:" <+> text x
--  format (VArray x) = format (VList $ Array.elems x)
--  format (VHash h) = braces $ (joinList $ text ", ") $
--      [ format (VStr k, v) | (k, v) <- Map.toList h ]
    format (VHandle x) = text $ show x
    format t@(VThread _) = text $ vCast t
    format (VSocket x) = text $ show x
    -- format (MVal v) = text $ unsafePerformIO $ do
    --     val <- readTVar v
    --     return $ pretty val
    format (VRule _) = text $ "{rule}"
    format (VSubst _) = text $ "{subst}"
    format VUndef = text $ "undef"

quoted :: Char -> String
quoted '\'' = "\\'"
quoted '\\' = "\\\\"
quoted x = [x]

ratToNum :: VRat -> VNum
ratToNum x = (fromIntegral $ numerator x) / (fromIntegral $ denominator x)

doubleBraces :: Doc -> Doc
doubleBraces x = vcat [ (lbrace <> lbrace), nest defaultIndent x, rbrace <> rbrace]

joinList :: Doc -> [Doc] -> Doc
joinList x y = cat $ punctuate x y

commasep :: [Doc] -> Doc
commasep x = cat $ (punctuate (char ',')) x

pretty :: Pretty a => a -> String
pretty a = render $ format a 

