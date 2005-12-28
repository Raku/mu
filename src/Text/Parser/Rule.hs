{-# OPTIONS_GHC -O2 -fglasgow-exts -funbox-strict-fields -fallow-undecidable-instances #-}
module Text.Parser.Rule where
import Prelude hiding (length, lookup, null, drop, span, break, head)
import Text.Parser.OpTable
import Text.Parser.PArrow
import Text.Parser.PArrow.MD (MD(..))
import Data.FastPackedString hiding (concatMap, concat, elem, foldl, foldl1, map)
import Data.Set (Set)
import Data.Char (isAlphaNum, isSpace)
import Control.Arrow

main :: IO ()
main = print (match "1+|2*" "2")

match :: String -> String -> Either (Int, [String]) String
match rule input = runParser (doComp (parseOptimized rule)) (pack input)

parseOptimized :: String -> Rule
parseOptimized = optimize . parse

doComp :: Rule -> MD String String
doComp (RTerm x)            = comp x
doComp (RQuant (QuantOne x))= comp x
doComp (RQuant x)           = comp x
doComp (RConj (Conj x))     = comp x
doComp (RConcat (Concat x)) = comp x
doComp (RAltern (Altern x)) = comp x

optimize :: Rule -> Rule
optimize x = x
{-
optimize (RuleConcat (RuleLit x) (RuleLit y)) = RuleLit (append x y)
optimize (RuleConcat (RuleWhitespace _) x) = x
optimize (RuleConcat x (RuleWhitespace _)) = x
-}

class Compilable a where
    comp :: a -> MD String String
    compMany :: [a] -> MD String String

instance Compilable a => Compilable [a] where
    comp []  = pure (const "")
    comp [x] = comp x
    comp xs  = compMany xs
    compMany = comp . concat

instance Compilable RuleTerm where
    comp (TermLit x) = string (unpack x)
    comp (TermWhitespace x) = pure (const "")
    compMany = foldl1 (\a b -> a &&& b >>^ snd) . map comp

instance Compilable RuleQuant where
    comp (QuantOne x) = comp x
    comp (Quant x 0 QuantInf Eager) = many (comp x) >>^ concat
    comp (Quant x 1 QuantInf Eager) = many1 (comp x) >>^ concat
    compMany = foldl1 (\a b -> a &&& b >>^ snd) . map comp


instance Compilable RuleConcat where
    comp (Concat x) = comp x
    compMany = foldl1 (\a b -> a &&& b >>^ snd) . map comp

instance Compilable RuleConj where
    comp (Conj x) = comp x
    compMany = choice . map comp

instance Compilable RuleAltern where
    comp (Altern x) = comp x
    compMany = error "impossible"

parse :: String -> Rule
parse = opParseAll ruleTable . pack

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

data RuleQuant
    = QuantOne !RuleTerm
    | Quant -- **{2} ? + *
        { quantTerm :: !RuleTerm
        , quantMin  :: !RuleQuantMin
        , quantMax  :: !RuleQuantMax
        , quantLazy :: !RuleLaziness
        }
    deriving (Eq, Show, Ord)

newtype RuleConcat = Concat [RuleQuant]
    deriving (Eq, Show, Ord)
newtype RuleConj   = Conj   [RuleConcat]    -- a & b & c
    deriving (Eq, Show, Ord)
newtype RuleAltern = Altern [RuleConj]      -- a | b | c
    deriving (Eq, Show, Ord)

type Flag = ()
data RulePattern = MkPattern
    { patFlags   :: Set Flag
    , patAlterns :: RuleAltern
    }
    deriving (Eq, Show, Ord)

data Rule
    = RTerm   !RuleTerm
    | RQuant  !RuleQuant
    | RConcat !RuleConcat
    | RConj   !RuleConj
    | RAltern !RuleAltern
    deriving (Eq, Show, Ord)

data RuleTerm
    = TermWhitespace !Str               -- #comment
    | TermCommit                        -- <commit>
    | TermCut !RuleCut                  -- <cut> : :: :::
    | TermAnchor !RuleAnchor            -- ^ $ \b
    | TermLit !FastString               -- a b c d
    | TermShortcut !RuleShortcut        -- . \d \w
    | TermSubrule !RuleCapturing !Rule  -- <?...> <...> 
    | TermGroup !RuleCapturing !Rule    -- [...] (...)
    | TermEnum !RuleEnum                -- <[a-z]>
    | TermClosure !RuleClosure          -- {...}
    | TermBind !RuleVar !RuleTerm       -- $1 := ...
    deriving (Eq, Show, Ord)

type RuleClosure = () -- not supported yet
type RuleQuantMin = Int
data RuleQuantMax = QuantInt !Int | QuantInf
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
    , noWs $ op _Concat Infix AssocList ""
    , op _Conj   Infix "&" 
    , op _Altern Infix "|" 
    ]
    where
    isMetaChar x = isSpace x || (x `elem` "\\%*+?:|.^$@[]()<>{}#")
    isNewline = (`elem` "\x0a\x0d\x0c\x85\x2028\x2029")
    wsLiteral str
        | null str = Just (str, str)
        | head str == '#', res <- break isNewline str = Just res
        | res@(pre, _) <- span isSpace str,     not (null pre) = Just res
        | res@(pre, _) <- break isMetaChar str, not (null pre) = Just res
        | otherwise = Nothing
    _Lit :: DynMkMatch Rule
    _Lit tok _ | null str           = mk TermWhitespace str
               | head str == '#'    = mk TermWhitespace str
               | isSpace (head str) = mk TermWhitespace str
               | otherwise          = mk TermLit str
        where
        str = tokStr tok
    _Term :: String -> DynMkMatch Rule
    _Term ":"   _ _  = mk TermCut CutThis
    _Term "::"  _ _  = mk TermCut CutGroup
    _Term ":::" _ _  = mk TermCut CutAll
    _Term "\\b" _ _  = mk TermAnchor AnchorBoundary
    _Term "\\B" _ _  = mk TermAnchor AnchorBoundaryNot
    _Term "^"   _ _  = mk TermAnchor AnchorBegin
    _Term "^^"  _ _  = mk TermAnchor AnchorBeginLine
    _Term "$$"  _ _  = mk TermAnchor AnchorEndLine
    _Term "."   _ _  = mk TermShortcut ShortcutAny
    _Term "\\d" _ _  = mk TermShortcut ShortcutDigit
    _Term "\\D" _ _  = mk TermShortcut ShortcutDigitNot
    _Term "\\s" _ _  = mk TermShortcut ShortcutSpace
    _Term "\\S" _ _  = mk TermShortcut ShortcutSpaceNot
    _Term "\\w" _ _  = mk TermShortcut ShortcutWord
    _Term "\\W" _ _  = mk TermShortcut ShortcutWordNot
    _Term "\\n" _ _  = mk TermShortcut ShortcutNewline
    _Term "\\N" _ _  = mk TermShortcut ShortcutNewlineNot
    _Term "<commit>" _ _ = mk TermCommit
    _Term x     _ _  = error x
    _Quant :: String -> DynMkMatch Rule
    _Quant "*" _ [x] = mk $ Quant (mk x) 0 QuantInf     Eager
    _Quant "+" _ [x] = mk $ Quant (mk x) 1 QuantInf     Eager
    _Quant "?" _ [x] = mk $ Quant (mk x) 0 (QuantInt 1) Eager -- XXX Lazify
    _Quant _   _ _   = error "unknown quant"
    _Group :: RuleCapturing -> DynMkMatch Rule
    _Group c _ [x] = mk $ TermGroup c x
    _Altern, _Concat, _Conj :: DynMkMatch Rule
    _Altern _ xs = mk $ Altern (mk xs)
    _Conj   _ xs = mk $ Conj (mk xs)
    _Concat _ xs = mk $ Concat (mk xs)

class MkClass a where mk :: a

instance (MkClass (a -> b)) => MkClass ([a] -> [b]) where
    mk = Prelude.map mk

instance MkClass (RuleQuant -> Rule) where mk = RQuant
instance MkClass (RuleTerm -> Rule) where mk = RTerm
instance MkClass (RuleConj -> Rule) where mk = RConj
instance MkClass (RuleConcat -> Rule) where mk = RConcat
instance MkClass (RuleAltern -> Rule) where mk = RAltern

instance MkClass (Rule -> RuleTerm) where
    mk (RTerm x) = x
    mk (RQuant (QuantOne x)) = x

instance MkClass (Rule -> RuleQuant) where
    mk (RTerm x) = QuantOne x
    mk (RQuant x) = x
    mk (RConj (Conj [Concat [x]])) = x
    mk x = error (show x)

instance MkClass (Rule -> RuleConcat) where
    mk (RTerm x) = Concat [QuantOne x]
    mk (RQuant x) = Concat [x]
    mk (RConj (Conj [x])) = x

instance MkClass (Rule -> RuleConj) where
    mk (RTerm x) = Conj [Concat [QuantOne x]]
    mk (RQuant x) = Conj [Concat [x]]
    mk (RConj x) = x

instance MkClass ((a -> RuleTerm) -> a -> Rule) where
    mk f x = RTerm (f x)
