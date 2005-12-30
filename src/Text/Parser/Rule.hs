{-# OPTIONS_GHC -O2 -fglasgow-exts -funbox-strict-fields -fallow-undecidable-instances #-}
module Text.Parser.Rule where
import Prelude hiding (lookup, null, drop, span, break, head, tail)
import Text.Parser.OpTable
import Text.Parser.PArrow
import Text.Parser.PArrow.MD (MD(..), Label(..), label, Monoid(..))
import Data.FastPackedString hiding (concatMap, concat, elem, foldl, foldl1, map, foldr, foldr1, elems)
import qualified Data.FastPackedString as Str
import qualified Data.Seq as Seq
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Parser.PArrow.CharSet
import Data.Set (Set, isSubsetOf)
import Data.Seq (Seq, fromList)
import Data.Map (Map)
import Data.IntMap (IntMap, insertWith, lookup, toAscList, elems, union)
import Data.Char (isAlphaNum, isSpace)
import Control.Arrow
import System.IO (stdout)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [x, y]  -> match x y
        _       -> putStrLn "*** This program takes two arguments: 'rule' and 'string'"

type NoMatch = IntMap Label

match :: String -> String -> IO ()
match r i = either (hPut stdout) print (matchRule r i)

matchRule :: String -> String -> Either FastString MatchRule
matchRule rule input = case runMatch (comp (parseOptimized rule)) str mempty of
    Left errs   -> Left (fst $ foldl (prettyErrs idxs) (Str.empty, mempty) (toAscList errs))
    Right ok    -> Right ok
    where
    str = pack input
    idxs = lineIdxs str

prettyErrs :: [Int] -> (FastString, Label) -> (Int, Label) -> (FastString, Label)
prettyErrs idxs (s, prev) (idx, this)
    | expects this `isSubsetOf` expects prev
    , unexpects this `isSubsetOf` unexpects prev
    = (s, prev)
    | Str.null s
    = (pack "Expecting: " `append` formatted, this)
    | otherwise
    = (s `append` pack "       or: " `append` formatted, this)
    where
    formatted = formWith expects `append` pack column
    column = " at line " ++ show lineNum ++ ", column " ++ show colNum ++ "\n"
    formWith f = formList (Set.toAscList (f this))
    formList [x] = x
    formList [x, y] = x `append` pack " or " `append` y
    formList (x:xs) = x `append` pack ", " `append` formList xs
    lns = (-1:Prelude.filter (< idx) idxs)
    colNum  = idx - Prelude.last lns
    lineNum = Prelude.length lns

-- a set of positions where newline occurs
lineIdxs :: FastString -> [Int]
lineIdxs ps 
    | null ps = []
    | otherwise = case elemIndexWord8 0x0A ps of
             Nothing -> []
             Just n  -> (n + idx ps:lineIdxs (drop (n+1) ps))

runMatch :: MD i o -> FastString -> NoMatch -> Either NoMatch o
runMatch p s errs | null s = Left errs
runMatch p s errs = case runParser p s of
    PErr err    -> runMatch p (tail s) (errs `union` err)
    POk _ ok    -> Right ok

insertErr :: Int -> Label -> NoMatch -> NoMatch
insertErr = insertWith mappend

{-
runOverlapMatch :: MD i o -> FastString -> Either NoMatch [o] -> Either NoMatch [o]
runOverlapMatch p s res | null s = res
runOverlapMatch p s (Left errs) = runOverlapMatch p (tail s)
    (either (Left . (\(idx, err) -> insertErr idx err errs)) (Right . (:[])) (runParser p s))
runOverlapMatch p s ok@(Right oks) = runOverlapMatch p (tail s)
    (either (const ok) (Right . (:oks)) (runParser p s))
-}

parseOptimized :: String -> Rule
parseOptimized = optimize . parse

optimize :: Rule -> Rule
optimize (RQuant (QuantNone x))     = REmpty
optimize (RQuant (QuantOne x))      = optimize $ mk x
optimize (RConcat (Concat xs))      = mk Concat (foldr joinConcat [] xs)
    where
    joinConcat (QuantNone _) ys = ys
    joinConcat x [] = [x]
    joinConcat x@(QuantOne tx) (y@(QuantOne ty):ys) = case joinTerm tx ty of
        Nothing -> (x:y:ys)
        Just x' -> (mk x':ys)
    joinConcat x (y:ys) = (x:y:ys)
    joinTerm (TermLit x) (TermLit y) = Just (TermLit (append x y))
    joinTerm _ _ = Nothing
optimize (RAltern (Altern [x]))     = optimize $ mk x
optimize (RConj (Conj [x]))         = optimize $ mk x
optimize x = x

-- | Rule Match object from PGE
data MatchRule
    = MatchObj
        { matBegin  :: !Int
        , matEnd    :: !Int
        , matSubPos :: !(Seq MatchRule)
        , matSubNam :: !(Map Str MatchRule)
        }
    | MatchSeq   !(Seq MatchRule)
    | MatchStr   !Str
    | MatchFail
    deriving (Eq, Show, Ord)

instance Monoid MatchRule where
    mempty = MatchFail
    mappend x@(MatchObj{}) MatchObj{matEnd=end} = x{matEnd=end}
    mappend x MatchFail = x
    mappend MatchFail y = y
    mappend x y = error (show (x, y))

nullMatch :: Str -> MatchRule
nullMatch s = MatchObj (idx s) (idx s) Seq.empty Map.empty

class Compilable a where
    comp :: a -> MD Str MatchRule
    compMany :: [a] -> MD Str MatchRule
    compMany = foldl1 (\a b -> a &&& b >>^ uncurry mappend) . map comp

instance Compilable Rule where
    comp (RTerm x)            = comp x
    comp (RQuant x)           = comp x
    comp (RConj (Conj x))     = comp x
    comp (RConcat (Concat x)) = comp x
    comp (RAltern (Altern x)) = comp x

instance Compilable a => Compilable [a] where
    comp []  = pure nullMatch
    comp [x] = comp x
    comp xs  = compMany xs
    compMany = comp . concat

strMatch :: Str -> MatchRule
strMatch (PS _ s l) = MatchObj s (s+l) Seq.empty Map.empty

instance Compilable Str where
    comp x = string x >>^ strMatch
    compMany = comp . Str.concat

instance Compilable RuleTerm where
    comp (TermLit x) = comp x
    comp (TermShortcut x) = MCSet x >>^ strMatch
    comp (TermGroup NonCapture r) = comp r

instance Compilable RuleQuant where
    comp (QuantNone x) = error "none"
    comp (QuantOne x) = comp x
    comp (Quant x 0 QuantInf Eager) = many (comp x) >>^ mconcat
    comp (Quant x 1 QuantInf Eager) = many1 (comp x) >>^ mconcat

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

{-
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
-}
type RuleShortcut = CharSet

data RuleEnum
    = EnumChars !Str                -- <[abcd]>
    | EnumRange !Char !Char         -- <[a..z]>
    | EnumPlus !RuleEnum !RuleEnum  -- <[]+[]>
    | EnumMinus !RuleEnum !RuleEnum -- <[]-[]>
    | EnumComplement !RuleEnum      -- <-[]>
    deriving (Eq, Show, Ord)

type Name = FastString

data RuleQuant
    = QuantOne !RuleTerm
    | QuantNone !Str                        -- #comment
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
    = REmpty
    | RTerm   !RuleTerm
    | RQuant  !RuleQuant
    | RConcat !RuleConcat
    | RConj   !RuleConj
    | RAltern !RuleAltern
    deriving (Eq, Show, Ord)

data RuleTerm
    = TermCommit                            -- <commit>
    | TermCut !RuleCut                      -- <cut> : :: :::
    | TermAnchor !RuleAnchor                -- ^ $ \b
    | TermLit !FastString                   -- a b c d
    | TermShortcut !RuleShortcut            -- . \d \w
    | TermSubrule !RuleCapturing !Rule      -- <?...> <...> 
    | TermGroup !RuleCapturing !Rule        -- [...] (...)
    | TermEnum !RuleEnum                    -- <[a-z]>
    | TermClosure !RuleClosure              -- {...}
    | TermBind !RuleVar !RuleTerm           -- $1 := ...
    deriving (Eq, Show, Ord)

data Negation = Positive | Negated
    deriving (Eq, Show, Ord)

type RuleClosure = () -- not supported yet
type RuleQuantMin = Int
data RuleQuantMax = QuantInt !Int | QuantInf
    deriving (Eq, Show, Ord)
data RuleCapturing = Capture | NonCapture
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
   ++ noWs (op (_Group Capture)    Circumfix "( )")
   ++ noWs (op (_Group NonCapture) Circumfix "[ ]")
    , op _Quant Postfix "* + ?"
    , noWs $ op _Concat Infix AssocList ""
    , op _Conj   Infix AssocList "&" 
    , op _Altern Infix AssocList "|" 
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
    _Lit tok _ | null str           = mk QuantNone str
               | head str == '#'    = mk QuantNone str
               | isSpace (head str) = mk QuantNone str
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
    _Term "."   _ _  = mk TermShortcut CS_Any
    _Term "\\d" _ _  = mk TermShortcut CS_Digit
    _Term "\\D" _ _  = mk TermShortcut (CS_Negated CS_Digit)
    _Term "\\s" _ _  = mk TermShortcut CS_Whitespace
    _Term "\\S" _ _  = mk TermShortcut (CS_Negated CS_Whitespace)
    _Term "\\w" _ _  = mk TermShortcut CS_Word
    _Term "\\W" _ _  = mk TermShortcut (CS_Negated CS_Word)
    _Term "\\n" _ _  = mk TermShortcut CS_Newline
    _Term "\\N" _ _  = mk TermShortcut (CS_Negated CS_Newline)
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

instance MkClass (RuleTerm -> RuleQuant) where
    mk = QuantOne

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
    mk (RConcat x) = Conj [x]
    mk (RConj x) = x

instance MkClass (a -> c) => MkClass ((b -> a) -> b -> c) where
    mk f x = mk (f x)

