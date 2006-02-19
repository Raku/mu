module RuleYAML (rules) where

import RuleUtils
import List 
import GenUtil

type AlwaysPositional = Bool

rules =
    [ ("YAML", userRuleYAML False, "Representation", "serialize into YAML nodes", Nothing)
    , ("YAML_Pos", userRuleYAML True, "Representation", "serialize into YAML nodes (ignore record labels)", Nothing)
    ]

userRuleYAML alwaysPos = instanceSkeleton' "YAML"
    [ (const empty, caseHead)
    , (makeFromYAML alwaysPos, const empty)
    , (const empty, caseTail)
    , (makeAsYAML alwaysPos, const empty)
    ]

instanceSkeleton' :: Class -> [(IFunction,[Body] -> Doc)] -> Data -> Doc
instanceSkeleton' s ii  d = (simpleInstance s d <+> text "where") 
				$$ block functions
	where
	functions = concatMap f ii
	f (i,dflt) = map i (body d) ++ [dflt $ body d]      

caseHead, caseTail :: [Body] -> Doc
caseHead _ = text "fromYAML MkYamlNode{tag=Just t, el=e} | 't':'a':'g':':':'h':'s':':':tag <- unpackFS t = case tag of"
caseTail bodies = nest 4 (text $ "_ -> fail $ \"unhandled tag: \" ++ show t ++ \", expecting \" ++ show " ++ show (map constructor bodies) ++ " ++ \" in node \" ++ show e")
       $+$ text "fromYAML _ = fail \"no tag found\""

makeFromYAML, makeAsYAML :: AlwaysPositional -> IFunction

makeFromYAML alwaysPos Body{constructor=constructor,labels=labels,types=types} =
    nest 4 $ eqv <+> match <+> dot $+$ extraLifts $+$ makeFromYAML'
    where
    dqt   = doubleQuotes . text
    match = text "->"
    dot   = text "do"
--  eqv   = text "| t == packFS" <+> dqt ("tag:hs:" ++ constructor)
    eqv   = dqt constructor
    makeFromYAML'
        | null types = nest 4 $ text "return" <+> text constructor
        | (alwaysPos || null labels) = vcat
            [ nest 4 $ text "let YamlSeq" <+> (list $ varNames types) <+> equals <+> text "e"
            , nest 4 $ liftNfy
            ]
        | otherwise = vcat
            [ nest 4 $ text "let YamlMap assocs = e"
            , nest 4 $ text "let" <+> (list $ varNames types) <+> equals <+> text "map snd assocs"
            , nest 4 $ liftNfy
            ]
    fy v = parens (text "fromYAML" <+> v)
    list = brackets . hsep . intersperse comma
    liftN = text "liftM" <> (if (arity == 1) then empty else text $ show arity)
    liftNfy = liftN <+> text constructor <+> (hsep $ map fy (varNames types))
    extraLifts
        | length types < 6 = empty
        | otherwise = nest 4 $ (text "let") <+> (hsep $ -- XXX: pull me to the level of the case?
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

makeAsYAML alwaysPos (Body{constructor=constructor,labels=labels,types=types})
    | null types = fnName <+> fsep [headfn, clsName constructor]
    | (alwaysPos || null labels) = fnName <+> fsep
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
