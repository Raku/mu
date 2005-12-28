{-# OPTIONS_GHC -O2 -fglasgow-exts -funbox-strict-fields #-}
module Text.Parser.Rule where
import Prelude hiding (length, lookup, null, drop, span, break, head)
import Text.Parser.OpTable
import Text.Parser.PArrow
import Data.FastPackedString hiding (concatMap, concat, elem)
import Data.Char (isAlphaNum, isSpace)
import Control.Arrow

main :: IO ()
main = print (parse "1+|2*" "12")

parse :: String -> String -> Either (Int, [String]) String
parse rule input = runParser (comp (parseRuleOptimized rule)) (pack input)

optimize :: Rule -> Rule
optimize (RuleConcat (RuleLit x) (RuleLit y)) = RuleLit (append x y)
optimize (RuleConcat (RuleWhitespace _) x) = x
optimize (RuleConcat x (RuleWhitespace _)) = x
optimize x = x

parseRuleOptimized :: String -> Rule
parseRuleOptimized = optimize . parseRule

parseRule :: String -> Rule
parseRule = opParseAll ruleTable . pack

comp :: Rule -> MD i String
comp (RuleLit x) = string (unpack x)
comp (RuleQuant x 0 QuantInf Eager) = many (comp x) >>^ concat
comp (RuleQuant x 1 QuantInf Eager) = many1 (comp x) >>^ concat
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
    = GroupCapture !Rule     -- (...)
    | GroupCaptureNot !Rule  -- [...]
    deriving (Eq, Show, Ord)

type Name = FastString

data Rule
    = RuleWhitespace !Str       -- #comment
    | RuleCommit                -- <commit>
    | RuleCut !RuleCut          -- <cut> : :: :::
    | RuleAnchor !RuleAnchor    -- ^ $ \b
    | RuleLit !FastString       -- a b c d
    | RuleConcat !Rule !Rule
    | RuleAltern !Rule !Rule
    | RuleConj !Rule !Rule
    | RuleShortcut !RuleShortcut
    | RuleSubrule !RuleCapturing !Rule  -- <?...> <...> 
    | RuleGroup !RuleCapturing !Rule    -- [...] (...)
    | RuleEnum !RuleEnum
    | RuleClosure !RuleClosure
    | RuleBind !RuleVar !Rule
    | RuleQuant                 -- **{2} ? + *
        { quantRule :: !Rule
        , quantMin  :: !RuleQuantMin
        , quantMax  :: !RuleQuantMax
        , quantLazy :: !RuleLaziness
        }
 -- | RuleModifier !RuleModifier !Rule
    deriving (Eq, Show, Ord)

type RuleClosure = () -- not supported yet
type RuleQuantMin = Int
data RuleQuantMax = Quant !Int | QuantInf
    deriving (Eq, Show, Ord)
data RuleCapturing = Capture | NoCapture
    deriving (Eq, Show, Ord)
data RuleLaziness = Eager | Lazy
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
    [ noWs (op _Lit  Term wsLiteral)
   ++ noWs (op _Term Term ": :: ::: \\b \\B ^ ^^ $$ . \\d \\D \\s \\S \\w \\W \\n <commit>")
   ++ noWs (op (_Group Capture)   Circumfix "( )")
   ++ noWs (op (_Group NoCapture) Circumfix "[ ]")
    , op _Quant Postfix "* + ?"
    , noWs $ op _Concat Infix AssocRight ""
    , op _Altern Infix "|" 
   ++ op _Conj   Infix "&" 
    ]
    where
    isMetaChar = (`elem` " \\%*+?:|.^$@[]()<>{}")
    isNewline = (`elem` "\x0a\x0d\x0c\x85\x2028\x2029")
    wsLiteral str
        | null str = Just (str, str)
        | res@(pre, _) <- break isMetaChar str, not (null pre) = Just res
        | res@(pre, _) <- span isSpace str,     not (null pre) = Just res
        | head str == '#', res <- break isNewline str = Just res
        | otherwise = Nothing
    _Quant, _Term :: String -> DynMkMatch Rule
    _Quant "*" _ [x] = RuleQuant x 0 QuantInf  Eager
    _Quant "+" _ [x] = RuleQuant x 1 QuantInf  Eager
    _Quant "?" _ [x] = RuleQuant x 0 (Quant 1) Eager -- XXX Lazify
    _Quant _   _ _   = error "unknown quant"
    _Term "::"  _ _  = RuleCut CutGroup
    _Term ":::" _ _  = RuleCut CutAll
    _Term "\\b" _ _  = RuleAnchor AnchorBoundary
    _Term "\\B" _ _  = RuleAnchor AnchorBoundaryNot
    _Term "^"   _ _  = RuleAnchor AnchorBegin
    _Term "^^"  _ _  = RuleAnchor AnchorBeginLine
    _Term "$$"  _ _  = RuleAnchor AnchorEndLine
    _Term "."   _ _  = RuleShortcut ShortcutAny
    _Term "\\d" _ _  = RuleShortcut ShortcutDigit
    _Term "\\D" _ _  = RuleShortcut ShortcutDigitNot
    _Term "\\s" _ _  = RuleShortcut ShortcutSpace
    _Term "\\S" _ _  = RuleShortcut ShortcutSpaceNot
    _Term "\\w" _ _  = RuleShortcut ShortcutWord
    _Term "\\W" _ _  = RuleShortcut ShortcutWordNot
    _Term "\\n" _ _  = RuleShortcut ShortcutNewline
    _Term "\\N" _ _  = RuleShortcut ShortcutNewlineNot
    _Term "<commit>" _ _ = RuleCommit
    _Group :: RuleCapturing -> DynMkMatch Rule
    _Group c _ [x] = RuleGroup c x
    _Lit, _Concat, _Altern, _Conj :: DynMkMatch Rule
    _Lit tok _ | null str           = RuleWhitespace str
               | head str == '#'    = RuleWhitespace str
               | isSpace (head str) = RuleWhitespace str
               | otherwise          = RuleLit str
        where
        str = tokStr tok
    _Concat _ [x, y] = RuleConcat x y
    _Altern _ [x, y] = RuleAltern x y
    _Conj   _ [x, y] = RuleConj x y
