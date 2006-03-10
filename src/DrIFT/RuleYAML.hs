{-# OPTIONS_GHC -fglasgow-exts #-}
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
    xvars = vars 'x'
    mvars = vars 'm'
    vars c = map ((char c <>) . int) [1 .. arity]
--  eqv   = text "| t == packFS" <+> dqt ("tag:hs:" ++ constructor)
    eqv   = dqt constructor
    makeFromYAML'
        | null types = nest 4 $ text "return" <+> text constructor
        | (alwaysPos || null labels) = vcat $ map (nest 4)
            [ text "let YamlSeq" <+> (list $ varNames types) <+> equals <+> text "e"
            , liftNfy
            ]
        | otherwise = vcat $ map (nest 4)
            [ text "let YamlMap assocs = e"
            , text "let" <+> (list $ varNames types) <+> equals <+> text "map snd assocs"
            , liftNfy
            ]
    fy v = parens (text "fromYAML" <+> v)
    list = brackets . hsep . punctuate comma
    liftN = text "liftM" <> (if (arity == 1) then empty else text $ show arity)
    liftNfy = liftN <+> text constructor <+> (hsep $ map fy (varNames types))
    extraLifts                     -- in some cases, we need to say e.g. "liftM12".
        | length types < 6 = empty -- Control.Monad provides liftM .. liftM5 already
        | otherwise = nest 4 $ text "let" <+> extraLiftsDef
    extraLiftsDef = 
        text "liftM" <> int arity <+> text "f" <+> hsep mvars <+> equals <+> dot $$
        braces extraLiftsBody
    extraLiftsBody =
        hsep [x <+> text "<-" <+> m <> semi | x <- xvars | m <- mvars ] <+>
        text "return" <+> parens (char 'f' <+> hsep xvars)
        
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
    bodyArray = brackets $ fsep (punctuate comma b)
    bodyStartHash = text "asYAMLmap" <+> c
    bodyHash = brackets $ fsep (punctuate comma b')
    c = clsPkg constructor
    b = map (\x -> sep [text "asYAML", x]) (varNames types)
    b' = zipWith (\x l -> parens (dq (text l) <> comma <+> x))
                                b labels
    clsName s = text "asYAMLcls" <+> clsPkg s
    clsPkg = dq . text
    dq = doubleQuotes
