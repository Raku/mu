{-# OPTIONS -fglasgow-exts #-}

{-
    Pretty printing for various data structures.

    Galadriel! Galadriel!
    Clear is the water of your well;
    White is the stars in your white hand;
    Unmarred, unstained is leaf and land
    In Dwimordene, in Lorien
    More fair than thoughts of Mortal Men.
-}

module Pretty where
import Internals
import AST
import Text.PrettyPrint

defaultIndent :: Int
defaultIndent = 2

class (Show a) => Pretty a where
    format :: a -> Doc
    format x = text $ show x

instance Pretty VStr

instance Pretty Exp where
    format (Val (VError msg (NonTerm pos))) = text "Syntax error at" <+> (text $ show pos) <+> format msg
    format (Val v) = format v
    format (Syn x vs) = text "Syn" <+> format x $+$ (braces $ vcat (punctuate (text ";") (map format vs)))
    format (Statements lines) = (vcat $ punctuate (text ";") $ (map format) lines)
    format (App sub invs args) = text "App" <+> format sub <+> parens (nest defaultIndent $ vcat (punctuate (text ", ") (map format $ invs ++ args)))
    format (Sym (Symbol scope name exp)) = text "Sym" <+> text (show scope) <+> format name <+> text ":=" <+>  (nest defaultIndent $ format exp)
    format x = text $ show x

instance Pretty Env where
    format x = doubleBraces $ nest defaultIndent (format $ envBody x) 

instance Pretty (Val, Val) where
    format (x, y) = format x <+> text "=>" <+> format y

instance Pretty (Exp, SourcePos) where
    format (x, _) = format x 

instance Pretty Val where
    format (VJunc (Junc j dups vals)) = parens $ joinList mark items 
        where
        items = map format $ values
        values = setToList vals ++ (concatMap (replicate 2)) (setToList dups)
        mark  = case j of
            JAny  -> text " | "
            JAll  -> text " & "
            JOne  -> text " ^ "
            JNone -> text " ! "
    format (VPair (x, y)) = parens $ format (x, y)
    format (VBool x) = if x then text "bool::true" else text "bool::false"
    format (VNum x) = if x == 1/0 then text "Inf" else text $ show x
    format (VInt x) = integer x
    format (VStr x) = text "Str" <+> (doubleQuotes $ text $ x) -- XXX escaping
    format (VRat x) = double $ ((fromIntegral $ numerator x) / (fromIntegral $ denominator x) :: Double)
    format (VComplex x) = text $ show x
    format (VControl x) = text $ show x
    format (VRef (VList x))
        | not . null . (drop 100) $ x
        = brackets $ format (head x) <+> text ", ..."
        | otherwise = brackets $ cat $ (punctuate $ text ", ") (map format x)
    format (VRef x) = text "\\" <> (parens $ format x)
    format (VList x)
        | not . null . (drop 100) $ x
        = parens $ (format (head x) <+> text ", ...")
        | otherwise = parens $ (joinList $ text ", ") (map format x)
    format (VSub _) = text "sub {...}"
    format (VBlock _) = text "{...}"
    format (VError x y) = hang (text "*** Error: " <> format x) defaultIndent (text "at" <+> format y)
    format (VArray (MkArray x)) = format (VList x)
    format (VHash (MkHash x)) = braces $ (joinList $ text ", ") (map format $ fmToList x)
    
    format (VHandle x) = text $ show x
    format (MVal _) = text $ "<mval>" -- format $ castV x
    format VUndef = text $ "undef"
    
doubleBraces :: Doc -> Doc
doubleBraces x = vcat [ (lbrace <> lbrace), nest defaultIndent x, rbrace <> rbrace]

joinList x y = cat $ punctuate x y

commasep x = cat $ (punctuate (char ',')) x

pretty :: Pretty a => a -> String
pretty a = render $ format a 

