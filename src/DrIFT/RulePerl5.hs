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
    | null labels && (length types == 1) = fnName <+> fsep
        [headfn, bodyStartScalar, bodyArray, bodyEndArray]
    | null labels = fnName <+> fsep
        [headfn, bodyStartArray, bodyArray, bodyEndArray]
    | otherwise = fnName <+> fsep
        [headfn, bodyStartHash, bodyHash, bodyEndHash]
    where
    fnName = text "showPerl5"
    headfn = fsep [(pattern constructor types), equals]
    bodyStartScalar = dq (text "bless(\\\\(") <+> text "++"
    bodyEndScalar = text "++" <+> dq (text ") =>" <+> c)
    bodyStartArray = dq (text "bless([") <+> text "++"
    bodyEndArray = text "++" <+> dq (text "] =>" <+> c)
    bodyStartHash = dq (text "bless({") <+> text "++"
    bodyEndHash = text "++" <+> dq (text "} =>" <+> c)
    bodyScalar = fsep b
    bodyArray = fsep [fsep (sepWith s' b)]
    bodyHash = fsep [fsep (sepWith s' b')]
    c = text "'" <> text (clsPkg constructor) <> text "')"
    b = map (\x -> fsep[text "showPerl5", x]) (varNames types)
    b' = zipWith (\x l -> fsep [dq (text (l ++ " => ")), comp,x])
                                b labels
    s' = fsep [comp,dq (char ','),comp]
    comp = text "++"
    clsName s = text "\"'" <> text (clsPkg s) <> text "'\""
    clsPkg = concat . intersperse "::" . splitBy (== '_')
    dq = doubleQuotes

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy f list =  first : splitBy f (dropWhile f rest)
   where
     (first, rest) = break f list

