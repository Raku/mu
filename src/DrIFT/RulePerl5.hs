module RulePerl5 (rules) where

import RuleUtils
import List 
import GenUtil

rules = [
    ("Perl5", userRulePerl5, "Representation", "bless into Perl 5 objects", Nothing)
    ]

userRulePerl5 = instanceSkeleton "Perl5" [(makePerl5, empty)] 

makePerl5 :: IFunction
makePerl5 (Body{constructor=constructor,labels=labels,types=types})
    | null types = fnName <+> fsep [headfn, clsName constructor]
--  | null labels && (length types == 1) = fnName <+> sep
--      [headfn, bodyStartScalar, bodyArray, bodyEndScalar]
    | null labels = fnName <+> fsep
        [headfn, bodyStartArray, bodyArray]
    | otherwise = fnName <+> fsep
        [headfn, bodyStartHash, bodyHash]
    where
    fnName = text "showPerl5"
    headfn = fsep [(pattern constructor types), equals]
--  bodyStartScalar = dq (text "bless(\\\\(") <+> text "++"
--  bodyEndScalar = text "++" <+> dq (text ") =>" <+> c)
--  bodyScalar = sep b
    bodyStartArray = text "showP5ArrayObj" <+> c
    bodyArray = brackets $ fsep (sepWith comma b)
    bodyStartHash = text "showP5HashObj" <+> c
    bodyHash = brackets $ fsep (sepWith comma b')
    c = clsPkg constructor
    b = map (\x -> sep [text "showPerl5", x]) (varNames types)
    b' = zipWith (\x l -> parens (dq (text l) <> comma <+> x))
                                b labels
    clsName s = text "showP5Class" <+> clsPkg s
    clsPkg = dq . text . concat . intersperse "::" . splitBy (== '_')
    dq = doubleQuotes

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy f list =  first : splitBy f (dropWhile f rest)
   where
     (first, rest) = break f list

