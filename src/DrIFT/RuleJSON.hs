module RuleJSON (rules) where

import RuleUtils
import List 
import GenUtil

rules = [
    ("JSON", userRuleJSON, "Representation", "JSON Representation", Nothing)
    ]

userRuleJSON = instanceSkeleton "JSON" [(makeJSON, empty)] 

makeJSON :: IFunction
makeJSON (Body{constructor=constructor,labels=labels,types=types})
    | null types = fnName <+> fsep [headfn, clsName constructor]
--  | null labels && (length types == 1) = fnName <+> sep
--      [headfn, bodyStartScalar, bodyArray, bodyEndScalar]
    | null labels = fnName <+> fsep
        [headfn, bodyStartArray, bodyArray]
    | otherwise = fnName <+> fsep
        [headfn, bodyStartHash, bodyHash]
    where
    fnName = text "showJSON"
    headfn = fsep [(pattern constructor types), equals]
--  bodyStartScalar = dq (text "bless(\\\\(") <+> text "++"
--  bodyEndScalar = text "++" <+> dq (text ") =>" <+> c)
--  bodyScalar = sep b
    bodyStartArray = text "showJSArrayObj" <+> c
    bodyArray = brackets $ fsep (punctuate comma b)
    bodyStartHash = text "showJSHashObj" <+> c
    bodyHash = brackets $ fsep (punctuate comma b')
    c = clsPkg constructor
    b = map (\x -> sep [text "showJSON", x]) (varNames types)
    b' = zipWith (\x l -> parens (dq (text l) <> comma <+> x))
                                b labels
    clsName s = text "showJSScalar" <+> clsPkg s
    clsPkg = dq . text -- . concat . intersperse "::" . splitBy (== '_')
    dq = doubleQuotes

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy f list =  first : splitBy f (dropWhile f rest)
   where
     (first, rest) = break f list

