module RuleYAML (rules) where

import RuleUtils
import List 
import GenUtil

rules = [
    ("YAML", userRuleYAML, "Representation", "serialize into YAML nodes", Nothing)
    ]

userRuleYAML = instanceSkeleton "YAML" [(const empty, caseHead), (makeFromYAML, empty), (const empty, caseTail), (makeAsYAML, empty)] 

caseHead, caseTail :: Doc
caseHead = text "fromYAML n@MkYamlNode{tag=t, el=e} = case deTag n of"
caseTail = empty

makeFromYAML, makeAsYAML :: IFunction

makeFromYAML Body{constructor=constructor,labels=labels,types=types} =
    nest 8 $ dqt constructor <+> match <+> makeFromYAML' $+$ extraLifts
    where
    dqt   = doubleQuotes . text
    match = text "->"
    dot   = text "do"
    makeFromYAML'
        | null types = text "return" <+> text constructor
        | null labels = vcat
            [ dot
            , nest 4 $ text "let YamlSeq" <+> (list $ varNames types) <+> equals <+> text "e"
            , nest 4 $ liftNfy
            ]
        | otherwise = vcat
            [ dot
            , nest 4 $ text "let YamlMap assocs = e"
            , nest 4 $ text "let" <+> (list $ varNames types) <+> equals <+> text "map snd assocs"
            , nest 4 $ liftNfy
            ]
    fy v = parens (text "fromYAML" <+> v)
    list = brackets . hsep . intersperse comma
    liftN = text "liftM" <> (if (arity == 1) then empty else text $ show arity)
    liftNfy = liftN <+> text constructor <+> (hsep $ map fy (varNames types))
    extraLifts
        | length types < 6 = empty
        | otherwise = nest 16 $ (text "where") $+$ (hsep $ -- XXX: pull me to the level of the case?
            [ (text $ "liftM" ++ (show arity))
            , text "f"
            ] ++ (map (\x -> text $ "m" ++ show x) [1 .. arity]) ++
            [ equals, dot, lbrace ] ++ 
                map (\(i, v) -> text $ "x" ++ i ++ " <- m" ++ i ++ ";") (zip (map show [1..]) (varNames types))
            ++
            [ text "return"
            , parens $ hsep
                [ (text "f")
                , (hsep $ map (\(x, n) -> (text x) <> (text n)) $ zip (repeat "x") (map show [1..arity]))
                ]
            , rbrace
            ])
    arity = length types

--liftM9 :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m a6 -> m a7 -> m a8 -> m a9 -> m r
--liftM9 f m1 m2 m3 m4 m5 m6 m7 m8 m9 = do { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5; x6 <- m6; x7 <- m7; x8 <- m8; x9 <- m9; return (f x1 x2 x3 x4 x5 x6 x7 x8 x9) }

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
