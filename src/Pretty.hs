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

module Pretty where
import Internals
import Types
import AST
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
    format (Syn x vs) = text "Syn" <+> format x $+$ (braces $ vcat (punctuate (text ";") (map format vs)))
    format (Statements lines) = (vcat $ punctuate (text ";") $ (map format) lines)
    format (App sub invs args) = text "App" <+> format sub <+> parens (nest defaultIndent $ vcat (punctuate (text ", ") (map format $ invs ++ args)))
    format (Sym syms) = text "Sym" <+> format syms
    format x = text $ show x

instance Pretty [Symbol a] where
    format syms = cat $ map format syms

instance Pretty (Symbol a) where
    format (SymVar scope name var) = text (show scope) <+> format name <+> text ":=" <+> (nest defaultIndent $ format var)
    format (SymExp scope name exp) = text (show scope) <+> format name <+> text ":=" <+> (nest defaultIndent $ format exp)

instance Pretty SourcePos where
    format pos =
        let file = sourceName pos
            line = show $ sourceLine pos
            col  = show $ sourceColumn pos
        in text $ file ++ " at line " ++ line ++ ", column " ++ col

instance Pretty Env where
    format x = doubleBraces $ nest defaultIndent (format $ envBody x) 

instance Pretty (Val, Val) where
    format (x, y) = format x <+> text "=>" <+> format y

instance Pretty (Exp, SourcePos) where
    format (x, _) = format x 

instance Pretty VRef where
    format x = braces $ text $ "ref:" ++ show x

instance Pretty Val where
    format (VJunc (Junc j dups vals)) = parens $ joinList mark items 
        where
        items = map format $ values
        values = Set.elems vals ++ (concatMap (replicate 2)) (Set.elems dups)
        mark  = case j of
            JAny  -> text " | "
            JAll  -> text " & "
            JOne  -> text " ^ "
            JNone -> text " ! "
    format (VPair (x, y)) = parens $ format (x, y)
    format (VBool x) = if x then text "bool::true" else text "bool::false"
    format (VNum x) = if x == 1/0 then text "Inf" else text $ show x
    format (VInt x) = integer x
    format (VStr x) = text $ "'" ++ encodeUTF8 (concatMap quoted x) ++ "'"
    format v@(VRat _) = text $ vCast v
    format (VComplex x) = text $ show x
    format (VControl x) = text $ show x
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
    format (VError x y) = hang (text "*** Error:" <+> text x) defaultIndent (text "at" <+> format y)
--  format (VArray x) = format (VList $ Array.elems x)
--  format (VHash h) = braces $ (joinList $ text ", ") $
--      [ format (VStr k, v) | (k, v) <- Map.toList h ]
    format (VHandle x) = text $ show x
    format t@(VThread _) = text $ vCast t
    format (VSocket x) = text $ show x
    -- format (MVal v) = text $ unsafePerformIO $ do
    --     val <- readIORef v
    --     return $ pretty val
    format (VThunk _) = text $ "{thunk}"
    format (VRule _) = text $ "{rule}"
    format (VSubst _) = text $ "{subst}"
    format VUndef = text $ "undef"

quoted '\'' = "\\'"
quoted '\\' = "\\\\"
quoted x = [x]

ratToNum :: VRat -> VNum
ratToNum x = (fromIntegral $ numerator x) / (fromIntegral $ denominator x)

doubleBraces :: Doc -> Doc
doubleBraces x = vcat [ (lbrace <> lbrace), nest defaultIndent x, rbrace <> rbrace]

joinList x y = cat $ punctuate x y

commasep x = cat $ (punctuate (char ',')) x

pretty :: Pretty a => a -> String
pretty a = render $ format a 

