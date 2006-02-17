module RuleYAML (rules) where

import RuleUtils
import List 
import GenUtil

rules = [
    ("YAML", userRuleYAML, "Representation", "serialize into YAML nodes", Nothing)
    ]

userRuleYAML = instanceSkeleton "YAML" [(const empty, caseHead), (makeFromYAML, empty), (const empty, caseTail), (makeAsYAML, empty)] 
--userRuleYAML = instanceSkeleton "YAML" [(const empty, caseHead), (makeFromYAML, caseTail), (makeAsYAML, empty)] 

caseHead, caseTail :: Doc
caseHead = text "fromYAML n@MkYamlNode{tag=t, el=e} = case deTag n of" <+> lbrace
caseTail = rbrace

makeFromYAML, makeAsYAML :: IFunction
--makeYAML f = makeFromYAML f $+$ makeAsYAML f

{-
        ; "Nul" -> return Nul
        ; "Pos" -> do
        ;     let YamlSeq [aa, ab] = e
        ;     liftM2 Pos (fromYAML aa) (fromYAML ab)
        ; "Rec" -> do
        ;     let YamlMap assocs = e
        ;     let [aa, ab] = map snd assocs
        ;     liftM2 Rec (fromYAML aa) (fromYAML ab)
-}
makeFromYAML Body{constructor=constructor,labels=labels,types=types} =
    semi <+> dqt constructor <+> match <+> makeFromYAML'
    where
    dqt   = doubleQuotes . text
    match = text "->"
    dot   = text "do"
    makeFromYAML'
        | null types = text "return" <+> text constructor
        | null labels = fsep
            [ dot  <+> lbrace
            , text "let YamlSeq" <+> (list $ varNames types) <+> equals <+> text "e"
            , semi <+> liftNfy
            , rbrace
            ]
        | otherwise = fsep
            [ dot <+> lbrace
            , text "let YamlMap assocs = e"
            , semi <+> text "let" <+> (list $ varNames types) <+> equals <+> text "map snd assocs"
            , semi <+> liftNfy
            , rbrace
            ]
    fy v = parens (text "fromYAML" <+> v)
    list = brackets . hsep . intersperse comma
    liftNfy = text "liftM" <> (text $ show $ length types) <+> text constructor <+> (hsep $ map fy (varNames types))

makeAsYAML (Body{constructor=constructor,labels=labels,types=types})
    | null types = fnName <+> fsep [headfn, clsName constructor]
    | null labels = fnName <+> fsep
        [headfn, bodyStartArray, bodyArray]
    | otherwise = fnName <+> fsep
        [headfn, bodyStartHash, bodyHash]
    where
    fnName = text "asYAML"
    headfn = fsep [(pattern constructor types), equals]
    bodyStartArray = text "asYAMLseq" <+> c
    bodyArray = brackets $ fsep (sepWith comma b)
    bodyStartHash = text "asYAMLmap" <+> c
    bodyHash = brackets $ fsep (sepWith comma b')
    c = clsPkg constructor
    b = map (\x -> sep [text "asYAML", x]) (varNames types)
    b' = zipWith (\x l -> parens (dq (text l) <> comma <+> x))
                                b labels
    clsName s = text "asYAMLcls" <+> clsPkg s
    clsPkg = dq . text
    dq = doubleQuotes
