{-# OPTIONS_GHC -O2 -fglasgow-exts -funbox-strict-fields -fallow-undecidable-instances #-}
module Text.Parser.Rule (
    module Text.Parser.Rule,
    module Text.Parser.PArrow,
) where
import Prelude hiding (lookup, null, drop, span, break, head, tail, splitAt)
import Text.Parser.OpTable
import Text.Parser.PArrow
import Text.Parser.PArrow.MD (MD(..), Label(..), label, Monoid(..))
import Data.FastPackedString hiding (concatMap, concat, elem, foldl, foldl1, map, foldr, foldr1, elems)
import Text.Parser.PArrow.CharSet
import Data.Set (Set, isSubsetOf)
import Data.Seq (Seq, toList, fromList, (<|), (|>), (><))
import Data.Map (Map)
import Data.Generics hiding (Infix)
import Data.IntMap (IntMap, insertWith, toAscList, union)
import Data.Char (isSpace)
import Data.Dynamic
import Control.Arrow
import System.IO (stdout)
import qualified Data.FastPackedString as Str
import qualified Data.Seq as Seq
import qualified Data.Map as Map
import qualified Data.Set as Set

type Parser = MD Str
type NoMatch = IntMap Label
type CompiledRule = MD Str MatchRule
type Grammar = Map Str CompiledRule

infixl 1 ~~
infixl ~:~
infixl ~&~
infixl .<>

(.<>) :: Grammar -> String -> CompiledRule
grammar .<> name = (Map.!) grammar (pack name)

(~:~) :: String -> String -> (Str, Rule)
name ~:~ rule = (pack name, parseOptimized rule)

(~&~) :: Typeable a => String -> OpTable a -> (Str, Rule)
name ~&~ tbl = (pack name, dynOpRule name tbl)

ruleGrammar :: Grammar
ruleGrammar = grammar
    [ "ruleDecl" ~:~ "rule \\s* \\{ <ruleBody> \\}"
    , "ruleBody" ~&~ ruleTable
    ]

grammar :: [(Str, Rule)] -> Grammar
grammar rules = Map.map comp normMap
    where
    ruleMap = Map.fromList rules
    normMap = Map.map replaceAll ruleMap
    replaceAll = everywhere (mkT replaceNode)
    replaceNode (TermSubrule c name) = TermGroup c ((Map.!) normMap name)
    replaceNode x = x

printMatch :: String -> String -> IO ()
printMatch r i = either (hPut stdout) print (matchRule r i)

mkRule :: String -> (MatchRule -> a) -> MD Str a
mkRule r f = rule r >>^ f

rule :: String -> CompiledRule
rule = comp . parseOptimized

dynOpRule :: Typeable a => String -> OpTable a -> Rule
dynOpRule label table = RTerm (TermDynamic (MkDynamicTerm (pack label) fun))
    where
    fun str = let (r, post) = opParsePartial table str in Just (toDyn r, post)

matchRule :: String -> String -> Either Str MatchRule
matchRule r = (~~ rule r)

(~~) :: String -> MD Str MatchRule -> Either Str MatchRule
(~~) input p = case runMatch p str mempty of
    Left errs   -> Left msg
        where
        (msg, _, _) = foldl (prettyErrs idxs) (Str.empty, -1, mempty) (toAscList errs)
    Right ok    -> Right (mkMatchObj ok)
    where
    str = pack input
    idxs = lineIdxs str

prettyErrs :: [Int] -> (Str, Int, Label) -> (Int, Label) -> (Str, Int, Label)
prettyErrs idxs (s, idx, prev) (idx', this)
    | succ idx == idx'
    , expects this `isSubsetOf` expects prev
    , unexpects this `isSubsetOf` unexpects prev
    = (s, idx', prev)
    | Str.null s
    = (pack "Expecting: " `append` formatted, idx', this)
    | otherwise
    = (s `append` pack "       Or: " `append` formatted, idx', this)
    where
    formatted = formWith expects `append` pack column
    column = " at line " ++ show lineNum ++ ", column " ++ show colNum ++ "\n"
    formWith f = formList (Set.toAscList (f this))
    formList [] = empty
    formList [x] = x
    formList [x, y] = x `append` pack " or " `append` y
    formList (x:xs) = x `append` pack ", " `append` formList xs
    lns = (-1:Prelude.filter (< idx') idxs)
    colNum  = idx' - Prelude.last lns
    lineNum = Prelude.length lns

-- a set of positions where newline occurs
lineIdxs :: Str -> [Int]
lineIdxs ps 
    | null ps = []
    | otherwise = case elemIndexWord8 0x0A ps of
             Nothing -> []
             Just n  -> (n + idx ps:lineIdxs (drop (n+1) ps))

runMatch :: Show o => MD i o -> Str -> NoMatch -> Either NoMatch o
runMatch _ s errs | null s = Left errs
runMatch p s errs = case runParser p s of
    PErr err    -> runMatch p (tail s) (errs `union` err)
    POk _ ok    -> Right ok

insertErr :: Int -> Label -> NoMatch -> NoMatch
insertErr = insertWith mappend

{-
runOverlapMatch :: MD i o -> Str -> Either NoMatch [o] -> Either NoMatch [o]
runOverlapMatch p s res | null s = res
runOverlapMatch p s (Left errs) = runOverlapMatch p (tail s)
    (either (Left . (\(idx, err) -> insertErr idx err errs)) (Right . (:[])) (runParser p s))
runOverlapMatch p s ok@(Right oks) = runOverlapMatch p (tail s)
    (either (const ok) (Right . (:oks)) (runParser p s))
-}

parseOptimized :: String -> Rule
parseOptimized = optimize . parseRule

class Optimizable a where
    optimize :: a -> a
    optimize = id
    
instance Optimizable Rule where
    optimize (RQuant (QuantNone _))     = REmpty
    optimize (RQuant (QuantOne x))      = optimize $ mk x
    optimize (RConcat (Concat [x]))     = optimize $ mk x
    optimize (RConcat x)                = mk $ optimize x
    optimize (RConj (Conj [x]))         = optimize $ mk x
    optimize (RConj x)                  = mk $ optimize x
    optimize (RAltern (Altern [x]))     = optimize $ mk x
    optimize (RAltern x)                = mk $ optimize x
    optimize x = x

instance Optimizable RuleQuant where
    optimize x = x

instance Optimizable RuleAltern where
    optimize (Altern xs) = Altern (map optimize xs)

instance Optimizable RuleConcat where
    optimize (Concat xs) = Concat (foldr joinConcat [] (map optimize xs))
        where
        joinConcat (QuantNone _) ys = ys
        joinConcat x [] = [x]
        joinConcat x@(QuantOne tx) (y@(QuantOne ty):ys) = case joinTerm tx ty of
            Nothing -> (x:y:ys)
            Just x' -> (mk x':ys)
        joinConcat x (y:ys) = (x:y:ys)
        joinTerm (TermLit x) (TermLit y) = Just (TermLit (append x y))
        joinTerm _ _ = Nothing

instance Optimizable RuleConj where
    optimize (Conj xs) = Conj (map optimize xs)

type EmptyStr = Str

-- | Rule Match object from PGE
data MatchRule
    = MatchObj
        { matchString :: !Str
        , matchSubPos :: !(Seq MatchRule)
        , matchSubNam :: !(Map Str MatchRule)
        }
    | MatchStr !Str
    | MatchNil !EmptyStr 
    | MatchSeq !(Seq MatchRule)
    -- below are intermediate forms
    | MatchPos !MatchRule
    | MatchNam !Str !MatchRule
    | MatchDyn !Str !Dynamic
    deriving (Show, Eq, Ord, Typeable)

instance Eq Dynamic where a == b = True
instance Ord Dynamic where compare a b = EQ

fin :: Str -> Int
fin (PS _ s l) = s + l

mkMatchObj :: MatchRule -> MatchRule
mkMatchObj x@MatchObj{} = x
mkMatchObj (MatchPos m) = MatchObj (matchStr m) (Seq.singleton m) Map.empty
mkMatchObj x@(MatchDyn s _) = MatchObj s (Seq.singleton x) Map.empty
mkMatchObj (MatchNam s m) = MatchObj (matchStr m) Seq.empty (Map.singleton s m)
mkMatchObj x@(MatchSeq l) = Seq.foldl doSeq (MatchObj (matchStr x) Seq.empty Map.empty) l
    where
    doSeq o (MatchPos m) = o{ matchSubPos = matchSubPos o |> m }
    doSeq o (MatchNam s m) = o{ matchSubNam = Map.insert s m (matchSubNam o) }
    doSeq o (MatchSeq l) = Seq.foldl doSeq o l
    doSeq o _ = o
mkMatchObj x = MatchObj (matchStr x) Seq.empty Map.empty

matchStr :: MatchRule -> Str
matchStr o@MatchObj{} = matchString o
matchStr (MatchStr s)   = s
matchStr (MatchNil s)   = s
matchStr (MatchSeq l)   = mergeStr
    (matchStr (Seq.index l 0))
    (matchStr (Seq.index l (pred (Seq.length l))))
matchStr (MatchPos m)   = matchStr m
matchStr (MatchNam _ m) = matchStr m
matchStr (MatchDyn s _)   = s

mergeStr :: Str -> Str -> Str
mergeStr (PS _ s _) (PS p s' l') = (PS p s (s'+l'-s))

instance Monoid MatchRule where
    mempty = error "empty match"
    mappend (MatchStr x) (MatchStr y) = MatchStr (mergeStr x y)
    mappend (MatchSeq x) (MatchSeq y) = MatchSeq (x >< y)
    mappend (MatchSeq x) y = MatchSeq (x |> y)
    mappend x (MatchSeq y) = MatchSeq (x <| y)
    mappend x y = MatchSeq (fromList [x, y])
    mconcat [] = error "empty concat"
    mconcat xs = foldl1 mappend xs

nullMatch :: Str -> MatchRule
nullMatch (PS p s _) = MatchStr (PS p s 0)

class Compilable a where
    comp :: a -> CompiledRule
    compMany :: [a] -> CompiledRule
    compMany = foldl1 (\a b -> a &&& b >>^ uncurry mappend) . map comp

instance Compilable Rule where
    comp REmpty               = MEmpty
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

instance Compilable Str where
    comp x = string x >>^ MatchStr
    compMany = comp . Str.concat

instance Compilable RuleTerm where
    comp (TermLit x) = comp x
    comp (TermDynamic x) = MDyn (mkLabel $ dynLabel x) (dynTerm x) >>^ uncurry MatchDyn
    comp (TermShortcut x) = MCSet x >>^ MatchStr
    comp (TermGroup NonCapture r) = comp r
    comp (TermGroup Negated r) = MNot (comp r)
    comp (TermGroup CapturePos r) = comp r >>^ MatchPos
    comp (TermGroup (CaptureNam n) r) = comp r >>^ MatchNam n
    comp x = error ("can't compile: " ++ show x)

instance Compilable RuleQuant where
    comp (QuantNone _) = error "none"
    comp (QuantOne x) = comp x
    comp (Quant x min max Greedy) = MGreedy min max (comp x) >>^
        either MatchNil (mconcat . toList)
    comp (Quant x min max Lazy) = MLazy min max (comp x) >>^
        either MatchNil (mconcat . toList)

instance Compilable RuleConcat where
    comp (Concat x) = comp x
    compMany = foldl1 (\a b -> a &&& b >>^ snd) . map comp

instance Compilable RuleConj where
    comp (Conj x) = comp x
    compMany = choice . map comp

instance Compilable RuleAltern where
    comp (Altern x) = comp x
    compMany = error "impossible"

parseRule :: String -> Rule
parseRule = opParseAll ruleTable . pack

data RuleCut
    = CutThis   -- :
    | CutGroup  -- ::
    | CutAll    -- :::
    deriving (Show, Eq, Ord, Data, Typeable)

data RuleAnchor
    = AnchorBoundary    -- \b
    | AnchorBoundaryNot -- \B
    | AnchorBegin       -- ^
    | AnchorEnd         -- $
    | AnchorBeginLine   -- ^^
    | AnchorEndLine     -- $$
    deriving (Show, Eq, Ord, Data, Typeable)

type RuleShortcut = CharSet

data RuleEnum
    = EnumChars !Str                -- <[abcd]>
    | EnumRange !Char !Char         -- <[a..z]>
    | EnumPlus !RuleEnum !RuleEnum  -- <[]+[]>
    | EnumMinus !RuleEnum !RuleEnum -- <[]-[]>
    | EnumComplement !RuleEnum      -- <-[]>
    deriving (Show, Eq, Ord, Data, Typeable)

type Name = Str

data RuleQuant
    = QuantOne !RuleTerm
    | QuantNone !Str                        -- #comment
    | Quant -- **{2} ? + *
        { quantTerm     :: !RuleTerm
        , quantMin      :: !MinQuant
        , quantMax      :: !MaxQuant
        , quantLaziness :: !RuleLaziness
        }
    deriving (Show, Eq, Ord, Data, Typeable)

newtype RuleConcat = Concat [RuleQuant]
    deriving (Show, Eq, Ord, Data, Typeable)
newtype RuleConj   = Conj   [RuleConcat]    -- a & b & c
    deriving (Show, Eq, Ord, Data, Typeable)
newtype RuleAltern = Altern [RuleConj]      -- a | b | c
    deriving (Show, Eq, Ord, Data, Typeable)

type Flag = ()
data RulePattern = MkPattern
    { patFlags   :: Set Flag
    , patAlterns :: RuleAltern
    }
    deriving (Show, Eq, Ord, Data, Typeable)

data Rule
    = REmpty
    | RTerm   !RuleTerm
    | RQuant  !RuleQuant
    | RConcat !RuleConcat
    | RConj   !RuleConj
    | RAltern !RuleAltern
    deriving (Show, Eq, Ord, Data, Typeable)

data RuleTerm
    = TermCommit                            -- <commit>
    | TermCut !RuleCut                      -- <cut> : :: :::
    | TermAnchor !RuleAnchor                -- ^ $ \b
    | TermLit !Str                          -- a b c d
    | TermShortcut !RuleShortcut            -- . \d \w
    | TermGroup !RuleCapturing !Rule        -- [...] (...)
    | TermEnum !RuleEnum                    -- <[a-z]>
    | TermClosure !RuleClosure              -- {...}
    | TermBind !RuleVar !RuleTerm           -- $1 := ...
    | TermSubrule !RuleCapturing !Name      -- <name>
    | TermDynamic !DynamicTerm
    deriving (Show, Eq, Ord, Data, Typeable)

data DynamicTerm = MkDynamicTerm 
    { dynLabel :: Str
    , dynTerm  :: Str -> Maybe (Dynamic, Str)
    }
    deriving (Data, Typeable)

instance Show DynamicTerm where show = show . dynLabel
instance Eq DynamicTerm where x == y = dynLabel x == dynLabel y
instance Ord DynamicTerm where compare x y = dynLabel x `compare` dynLabel y
instance Data Dynamic where
    gunfold = error "gunfold"
        :: (forall r. c (Str -> r) -> c r) -> (forall r . r -> c r) -> Constr -> c Dynamic
    toConstr = error "gfoldl"
    dataTypeOf = error "dataTypeOf"

type RuleClosure = () -- not supported yet
data RuleCapturing = CapturePos | CaptureNam !Name | NonCapture | Negated
    deriving (Show, Eq, Ord, Data, Typeable)
data RuleLaziness = Greedy | Lazy
    deriving (Show, Eq, Ord, Data, Typeable)
data RuleVar = VarPos !Int | VarNamed !Str
    deriving (Show, Eq, Ord, Data, Typeable)
data RuleModifier
    = ModifierIgnorecase
    | ModifierGlobal
    | ModifierPos !Int
    | ModifierOnce
    deriving (Show, Eq, Ord, Data, Typeable)

ruleTable :: OpTable Rule
ruleTable = mkOpTable
    [ noWs (op _Lit  Term wsLiteral)
   ++ noWs (op _Term Term ": :: ::: \\b \\B ^ ^^ $$ . \\d \\D \\s \\S \\w \\W \\n <commit>")
   ++ noWs (op (_Group CapturePos)   Circumfix "( )")
   ++ noWs (op (_Group NonCapture)   Circumfix "[ ]")
   ++ noWs (op (_Subrule CapturePos) Term "<"  wsSubrule)
   ++ noWs (op (_Subrule NonCapture) Term "<?" wsSubrule)
   ++ noWs (op (_Subrule Negated)    Term "<!" wsSubrule)
    , op _Quant Postfix "* + ?"
    , noWs $ op _Concat Infix AssocList ""
    , op _Conj   Infix AssocList "&" 
    , op _Altern Infix AssocList "|" 
    ]
    where
    isMetaChar x = isSpace x || (x `elem` "\\%*+?:|.^$@[]()<>{}#")
    isNewline = (`elem` "\x0a\x0d\x0c\x85\x2028\x2029")
    wsSubrule str
        | (pre, post) <- break (== '>') str = Just (pre, tail post)
        | otherwise = Nothing
    wsLiteral str
        | null str = Just (str, str)
        | head str == '#', res <- break isNewline str = Just res
        | head str == '\\', ch <- index str 1, isMetaChar ch = Just (splitAt 1 (tail str))
        | res@(pre, _) <- span isSpace str, not (null pre) = Just res
        | res@(pre, _) <- splitAt 1 str, not (isMetaChar (head pre)) = Just res
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
    _Quant "*" _ [x] = mk $ Quant (mk x) 0 QuantInf     Greedy
    _Quant "+" _ [x] = mk $ Quant (mk x) 1 QuantInf     Greedy
    _Quant "?" _ [RQuant q@(Quant{})] = mk q{ quantLaziness = Lazy }
    _Quant "?" _ [x] = mk $ Quant (mk x) 0 (QuantInt 1) Greedy -- XXX Lazify
    _Quant _   _ _   = error "unknown quant"
    _Altern, _Concat, _Conj :: DynMkMatch Rule
    _Altern _ xs = mk $ Altern (mk xs)
    _Conj   _ xs = mk $ Conj (mk xs)
    _Concat _ xs = mk $ Concat (mk xs)
    _Group, _Subrule :: RuleCapturing -> DynMkMatch Rule
    _Group c _ [x] = mk $ TermGroup c x
    _Group _ _ _   = error "impossible: multigroup"
    _Subrule c tok _
        | CapturePos <- c   = mk $ TermSubrule (CaptureNam nam) nam
        | otherwise         = mk $ TermSubrule c nam
        where
        nam = (tokStr tok)

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
    mk x = error ("downcast to term" ++ show x)

instance MkClass (Rule -> RuleQuant) where
    mk (RTerm x) = QuantOne x
    mk (RQuant x) = x
    mk (RConj (Conj [Concat [x]])) = x
    mk x = error ("downcast to quant" ++ show x)

instance MkClass (Rule -> RuleConcat) where
    mk (RTerm x) = Concat [QuantOne x]
    mk (RQuant x) = Concat [x]
    mk (RConj (Conj [x])) = x
    mk x = error ("downcast to concat" ++ show x)

instance MkClass (Rule -> RuleConj) where
    mk (RTerm x) = Conj [Concat [QuantOne x]]
    mk (RQuant x) = Conj [Concat [x]]
    mk (RConcat x) = Conj [x]
    mk (RConj x) = x
    mk x = error ("downcast to conj" ++ show x)

instance MkClass (a -> c) => MkClass ((b -> a) -> b -> c) where
    mk f x = mk (f x)

