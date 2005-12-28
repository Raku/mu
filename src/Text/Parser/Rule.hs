{-# OPTIONS_GHC -O2 -fglasgow-exts -funbox-strict-fields #-}
module Text.Parser.Rule where
import Prelude hiding (length, lookup, null, drop, span)
import Text.Parser.OpTable
import Text.Parser.PArrow
import Data.FastPackedString hiding (concatMap, concat)
import Data.Char (isAlphaNum)
import Control.Arrow

main :: IO ()
main = print (parse "1+|2*" "12")

parse :: String -> String -> Either (Int, [String]) String
parse rule input = runParser (comp $ opParse ruleTable (pack rule)) (pack input)

comp :: Rule -> MD i String
comp (RuleLit x) = string (unpack x)
comp (RuleQuant x 0 QuantInf QuantEager) = many (comp x) >>^ concat
comp (RuleQuant x 1 QuantInf QuantEager) = many1 (comp x) >>^ concat
comp (RuleConcat x y) = comp x >>> comp y
comp (RuleAltern x y) = choice [comp x, comp y]

data RuleCut
    = CutThis   -- :
    | CutGroup  -- ::
    | CutAll    -- :::
    deriving (Eq, Show, Ord)

data RuleAnchor
    = AnchorBoundary    -- \b
    | AnchorBoundaryNot -- \B
    | AnchorBegin       -- ^
    | AnchorEnd         -- $
    | AnchorBeginLine   -- ^^
    | AnchorEndLine     -- $$
    deriving (Eq, Show, Ord)

data RuleShortcut
    = ShortcutAny       -- .
    | ShortcutDigit     -- \d
    | ShortcutDigitNot  -- \D
    | ShortcutSpace     -- \s
    | ShortcutSpaceNot  -- \S
    | ShortcutWord      -- \w
    | ShortcutWordNot   -- \W
    | ShortcutNewline   -- \n
    | ShortcutNewlineNot-- \N
    deriving (Eq, Show, Ord)

data RuleEnum
    = EnumChars !Str                -- <[abcd]>
    | EnumRange !Char !Char         -- <[a..z]>
    | EnumPlus !RuleEnum !RuleEnum  -- <[]+[]>
    | EnumMinus !RuleEnum !RuleEnum -- <[]-[]>
    | EnumComplement !RuleEnum      -- <-[]>
    deriving (Eq, Show, Ord)

data RuleGroup
    = GroupCapture      -- (...)
    | GroupCaptureNot   -- [...]
    deriving (Eq, Show, Ord)

type Name = FastString

data RuleSubrule
    = SubruleCapture    !Name  -- <...>
    | SubruleCaptureNot !Name  -- [...]
    deriving (Eq, Show, Ord)

data Rule
    = RuleCommit                -- <commit>
    | RuleCut !RuleCut          -- <cut> : :: :::
    | RuleAnchor !RuleAnchor    -- ^ $ \b
    | RuleLit !FastString       -- a b c d
    | RuleConcat !Rule !Rule
    | RuleAltern !Rule !Rule
    | RuleConj !Rule !Rule
    | RuleSubrule !RuleSubrule
    | RuleEnum !RuleEnum
    | RuleClosure !RuleClosure
    | RuleBind !RuleVar !Rule
    | RuleQuant                 -- **{2} ? + *
        { quantRule :: !Rule
        , quantMin  :: !RuleQuantMin
        , quantMax  :: !RuleQuantMax
        , quantLazy :: !RuleQuantLazy
        }
 -- | RuleModifier !RuleModifier !Rule
    deriving (Eq, Show, Ord)

type RuleClosure = () -- not supported yet
type RuleQuantMin = Int
data RuleQuantMax = Quant !Int | QuantInf
    deriving (Eq, Show, Ord)
data RuleQuantLazy = QuantEager | QuantLazy
    deriving (Eq, Show, Ord)
data RuleVar = VarPos !Int | VarNamed !Str
    deriving (Eq, Show, Ord)
data RuleModifier
    = ModifierIgnorecase
    | ModifierGlobal
    | ModifierPos !Int
    | ModifierOnce
    deriving (Eq, Show, Ord)


ruleTable :: OpTable Rule
ruleTable = mkOpTable
    [ op _Lit Term (span isAlphaNum)
    , concatMap (\x -> op (_Quant x) Postfix x) ["*", "+"]
    , op _Concat Infix AssocRight ""
    , op _Altern Infix "|" 
    ]
    where
    _Quant :: String -> DynMkMatch Rule
    _Quant "*" _ [x] = RuleQuant x 0 QuantInf QuantEager
    _Quant "+" _ [x] = RuleQuant x 1 QuantInf QuantEager
    _Quant _   _ _   = error "unknown quant"
    _Lit, _Concat, _Altern :: DynMkMatch Rule
    _Lit tok _ = RuleLit (tokStr tok)
    _Concat _ [x, y] = RuleConcat x y
    _Altern _ [x, y] = RuleAltern x y
