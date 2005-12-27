{-# OPTIONS_GHC -O2 -fglasgow-exts -funbox-strict-fields #-}

-- A Haskell port of PGE::OPTable.

module Text.Parser.OpTable where
import Prelude hiding (length, lookup, null, drop, span)
import qualified Data.Map as NMap
import qualified Data.Seq as NSeq
import qualified Data.FastPackedString as NStr
import Data.Ratio
import Data.Char (isDigit)
import Data.List (find)
import Data.Seq (Seq, fromList)
import Data.Map (Map, insert, lookup, toAscList, (!))
import Data.FastPackedString (empty, pack, null, drop, dropSpace, length, isPrefixOf, span, append)

data Op
    = Infix         { str :: !Str, assoc :: !Assoc }
    | Prefix        { str :: !Str }
    | Postfix       { str :: !Str }
    | Term          { str :: !Str }
    | DynTerm       { str :: !Str, dynStr  :: !DynStr }
    | Ternary       { str :: !Str, str2 :: !Str }
    | Circumfix     { str :: !Str, str2 :: !Str }
    | PostCircumfix { str :: !Str, str2 :: !Str }
    | Close         { str :: !Str }
    deriving (Eq, Show, Ord)

-- newtype DynStr = MkDynStr (forall m. Monad m => Str -> m (Str, Op -> Seq Match))
type DynStr = Str -> Maybe DynResult

data DynResult
    = DynResultMatch
        { dynMatched    :: !Str
        , dynRemainder  :: !Str
        }
    | DynResultTrans
        { dynMatched    :: !Str
        , dynRemainder  :: !Str
        , dynOpTrans    :: !(forall r. OpTable r -> OpTable r)
        }

instance Eq DynStr where _ == _ = True
instance Ord DynStr where compare _ _ = EQ
instance Show DynStr where show _ = "<dyn>"

instance Eq (DynMkMatch r) where _ == _ = True
instance Ord (DynMkMatch r) where compare _ _ = EQ
instance Show (DynMkMatch r) where show _ = "<mkMatch>"

type DynMkMatch r = (Token r -> [r] -> r)
type Precedence = Ratio Integer
type Arity = Int

data Token r = MkToken
    { tokOp      :: !Op
    , tokPrec    :: !Precedence
    , tokArity   :: !Arity
    , tokClose   :: !(Maybe Op)
    , tokNull    :: !Bool -- null-width assertion
    , tokMkMatch :: !(DynMkMatch r)
    }
    deriving (Eq, Show)

instance Ord (Token r) where
    compare x y = compare (tokPrec x) (tokPrec y)

data Match = MkMatch
    { matchOp   :: !Op
    , matchArgs :: !(Seq Match)
    }
    deriving (Eq, Show, Ord)

data Assoc
    = AssocNon | AssocLeft | AssocRight | AssocChain | AssocList
    deriving (Eq, Show, Ord)

data Whitespace
    = AllowWhitespace
    | NoWhitespace
    deriving (Eq, Show, Ord)

data PrecRelation
    = DefaultPrec
    | SameAs        { relOp :: !Op }
    | TighterThan   { relOp :: !Op }
    | LooserThan    { relOp :: !Op }
    deriving (Eq, Show, Ord)

data OpTable r = MkOpTable
    { tableEntries   :: !(EntryMap r)
    , tableTerms     :: !(TokenMap r)
    , tableOpers     :: !(TokenMap r)
    , tableWsTerms   :: !(TokenMap r)
    , tableWsOpers   :: !(TokenMap r)
    }
    deriving (Eq, Show, Ord)

emptyTable :: OpTable r
emptyTable = MkOpTable NMap.empty NMap.empty NMap.empty NMap.empty NMap.empty

type Str = NStr.FastString
type EntryMap a = Map Op (Token a)
type TokenMap a = Map Term (Token a)

-- | Terms are ordered by descending length first.
newtype Term = MkTerm { termToStr :: Str } deriving (Eq, Show)

termLength :: Term -> Int
termLength = length . termToStr

instance Ord Term where
    compare (MkTerm x) (MkTerm y) = case compare (NStr.length y) (NStr.length x) of
        EQ -> compare x y
        o  -> o

addToken :: OpTable r -> Op -> DynMkMatch r -> PrecRelation -> Whitespace -> OpTable r
addToken table op mk rel ws = doCloseOp . doInsert $ table{ tableEntries = ents' }
    where
    ents  = tableEntries table
    ents' = doCloseEntry $ insert op tok ents
    tok   = MkToken
        { tokOp      = op
        , tokPrec    = calculatePrec rel ents
        , tokArity   = arityOf op
        , tokClose   = maybeOpClose
        , tokNull    = False
        , tokMkMatch = mk
        }
    doInsert = insertBy op tok ws
    (doCloseOp, doCloseEntry, maybeOpClose)
        | isClosing op =
            ( insertOp mkTokClose AllowWhitespace
            , insert opClose tok
            , Just opClose
            )
        | otherwise    = (id, id, Nothing)
    opClose = Close (str2 op)
    mkTokClose = MkToken
        { tokOp      = opClose
        , tokPrec    = tokPrec tok
        , tokArity   = 0
        , tokClose   = Nothing
        , tokNull    = False
        , tokMkMatch = mk
        }

arityOf :: Op -> Arity
arityOf Close{}         = 0
arityOf Ternary{}       = 3
arityOf Infix{}         = 2
arityOf PostCircumfix{} = 2
arityOf _               = 1

insertBy :: Op -> Token r -> Whitespace -> OpTable r -> OpTable r
insertBy Term{}           = insertTerm
insertBy DynTerm{}        = insertTerm
insertBy Prefix{}         = insertTerm
insertBy Circumfix{}      = insertTerm
insertBy _                = insertOp

isClosing :: Op -> Bool
isClosing Ternary{}         = True
isClosing Circumfix{}       = True
isClosing PostCircumfix{}   = True
isClosing _                 = False

insertTerm, insertOp :: Token r -> Whitespace -> OpTable r -> OpTable r
insertTerm tok NoWhitespace    table = table
    { tableTerms   = insertTok tok (tableTerms table)   }
insertTerm tok AllowWhitespace table = table
    { tableTerms   = insertTok tok (tableTerms table)
    , tableWsTerms = insertTok tok (tableWsTerms table) }
insertOp   tok NoWhitespace    table = table
    { tableOpers     = insertTok tok (tableOpers table)     }
insertOp   tok AllowWhitespace table = table
    { tableOpers     = insertTok tok (tableOpers table)
    , tableWsOpers   = insertTok tok (tableWsOpers table) }

insertTok :: Token r -> TokenMap r -> TokenMap r
insertTok tok tmap = insert key tok tmap
    where
    key = MkTerm (tokStr tok)

tokStr :: Token r -> Str
tokStr = str . tokOp

defaultPrec :: Precedence
defaultPrec = 1%1

calculatePrec :: PrecRelation -> EntryMap r -> Precedence
calculatePrec DefaultPrec _ = defaultPrec
calculatePrec rel toks = case rel of
    SameAs {}       -> prec
    LooserThan {}   -> prec - 1 % (denominator prec * 2)
    TighterThan {}  -> prec + 1 % (denominator prec * 2)
    where
    prec = tokPrec (toks ! (relOp rel))

type TokenStack r = [Token r]
type TermStack r  = [r]
type OperStack r  = [r]

type Parse r a = ( ?termStack  :: TermStack r
                 , ?tokenStack :: TokenStack r
                 , ?operStack  :: OperStack ([r] -> r)
                 , ?tbl        :: OpTable r
                 , ?str        :: Str
                 ) => a

opParse :: OpTable r -> Str -> r
opParse tbl str =
    let ?termStack  = []
        ?tokenStack = []
        ?operStack  = []
        ?tbl        = tbl 
        ?str        = str
     in expectTerm

expectTerm :: Parse r r
expectTerm
    | null ?str = nullTerm
    | otherwise = let ?str = str' in matchWith foundTerm nullTerm terms
    where
    str'  = dropSpace ?str
    terms = (if length str' == length ?str then tableTerms else tableWsTerms) ?tbl

matchWith :: Parse r (Parse r (Token r -> a) -> Parse r a -> TokenMap r -> a)
matchWith ok nok tmap = case find ((`isPrefixOf` ?str) . termToStr . fst) (toAscList tmap) of
    Just (term, token@MkToken{ tokOp = DynTerm{ dynStr = dyn } }) ->
        let str' = drop (termLength term) ?str in
            case dyn str' of
                Just res ->
                    let ok' = let ?str = dynRemainder res
                               in ok token{ tokOp = Term (termToStr term `append` dynMatched res) }
                     in case res of
                        DynResultTrans{} -> let ?tbl = dynOpTrans res ?tbl in ok'
                        _                -> ok'
                _                -> nok
    Just (term, token) -> let ?str = drop (termLength term) ?str in ok token
    _                  -> nok

isTerm :: Op -> Bool
isTerm Term{}    = True
isTerm DynTerm{} = True
isTerm _         = False

foundTerm :: Parse r (Token r -> r)
foundTerm token
    | isTerm (tokOp token) = pushTermStack token expectOper
    | otherwise            = operShift token

pushTermStack :: Parse r (Token r -> Parse r a -> Parse r a)
pushTermStack token p = let ?termStack = (tokMkMatch token token []: ?termStack) in p

pushOperStack :: Parse r (Token r -> Parse r a -> Parse r a)
pushOperStack token p = let ?operStack = (tokMkMatch token token: ?operStack) in p

pushTokenStack :: Parse r (Token r -> Parse r a -> Parse r a)
pushTokenStack token p = let ?tokenStack = (token: ?tokenStack) in p

mkMatch :: DynMkMatch Match
mkMatch token = MkMatch (tokOp token) . fromList

expectOper :: Parse r r
expectOper
    | null str' = endParse
    | otherwise = let ?str = str' in matchWith foundOper endParse opers
    where
    str'  = dropSpace ?str
    opers = (if length str' == length ?str then tableOpers else tableWsOpers) ?tbl

nullTerm :: Parse r r
nullTerm | (t@MkToken{ tokNull = True }:_) <- ?tokenStack = pushTermStack t expectOper
         | otherwise = error "Missing term"

foundOper :: Parse r (Token r -> Parse r r)
foundOper oper
    | (top:_) <- ?tokenStack = case tokOp top of
        Postfix{}                   -> operReduce oper
        topOp       | Close{} <- op -> if isClosing topOp
            then if str op == str2 topOp
                then operShift oper
                else endParse
            else operReduce oper
        Circumfix{}                 -> operShift oper
        PostCircumfix{}             -> operShift oper
        _           | oper > top    -> operShift oper
        Ternary{}                   -> case op of
            Ternary{}   -> error "Missing ternary close"
            _           -> operShift oper
        _           | oper < top    -> operReduce oper
        Infix{ assoc = AssocRight } -> operShift oper
        _                           -> operReduce oper
    | Close{} <- op          = endParse
    | otherwise              = operShift oper
    where
    op = tokOp oper

operShift :: Parse r (Token r -> Parse r r)
operShift token = pushTokenStack token (pushOperStack token (case tokOp token of
        Prefix{}        -> expectTerm
        Infix{}         -> expectTerm
        Ternary{}       -> expectTerm
        PostCircumfix{} -> expectTerm
        Circumfix{}     -> expectTerm
        Postfix{}       -> expectOper
        _ | (_:MkToken{tokOp=Ternary{}}:_) <- ?tokenStack
                        -> expectTerm
        _               -> expectOper
    ))

operReduce :: Parse r (Token r -> r)
operReduce oper = reduce (foundOper oper)

endParse :: Parse r r
endParse
    | [] <- ?tokenStack = head ?termStack
    | otherwise         = reduce endParse

reduce :: Parse r (Parse r r -> r)
reduce p = case ?tokenStack of
    (MkToken{tokOp=Close{}}:t:ts) ->
        let ?operStack  = tail ?operStack
            ?tokenStack = ts
         in reduce1 (tokArity t) p
    (t:ts)  -> let ?tokenStack = ts in reduce1 (tokArity t) p
    _       -> error "reducing an empty token stack"

reduce1 :: Parse r (Arity -> Parse r r -> r)
reduce1 arity p =
    let (op:opers)    = ?operStack
        (args, terms) = splitAt arity ?termStack
     in let ?operStack = opers
            ?termStack = (op (reverse args):terms) in p

mkOpTable :: [[(Whitespace, DynMkMatch r, Op)]] -> OpTable r
mkOpTable = fst . Prelude.foldl mkOps (emptyTable, DefaultPrec)
    where
    mkOps x [] = x
    mkOps (tbl, rel) [(ws, mk, op)] = (addToken tbl op mk rel ws, LooserThan op)
    mkOps (tbl, rel) ((ws, mk, op):xs) = mkOps (addToken tbl op mk rel ws, rel) xs

testTable :: OpTable Match
testTable = mkOpTable
    [ op mkMatch Circumfix       "( )"
    , op mkMatch Term            (span isDigit)
    , op mkMatch Infix           "* /"
    , op mkMatch Infix AssocLeft "+ -"
    ]

class MkClass a where op :: a

instance MkClass (a -> (Str -> Op) -> [Char] -> [(Whitespace, a, Op)]) where
    op mk op1 = Prelude.map (((,,) AllowWhitespace mk) . op1 . pack) . splitWords

instance MkClass (a -> (Str -> Str -> Op) -> [Char] -> [(Whitespace, a, Op)]) where
    op mk op2 = Prelude.map (((,,) AllowWhitespace mk) . uncurry op2) . pack2 . splitWords
        where
        pack2 (x:y:zs)  = ((pack x, pack y):pack2 zs)
        pack2 _         = []

instance MkClass (a -> (Str -> Assoc -> Op) -> [Char] -> [(Whitespace, a, Op)]) where
    op mk op1 = op mk op1 AssocLeft

instance MkClass (a -> (Str -> Assoc -> Op) -> Assoc -> [Char] -> [(Whitespace, a, Op)]) where
    op mk op1 assoc = Prelude.map (((,,) AllowWhitespace mk) . (`op1` assoc) . pack) . splitWords

instance (MkClass (a -> (Str -> Op) -> (Str -> (Str, Str)) -> [(Whitespace, a, Op)])) where
    op mk op1 f = [(AllowWhitespace, mk, DynTerm empty dyn)]
        where
        dyn str = let (pre, post) = f str in
            if null pre then Nothing else Just (DynResultMatch pre post)

splitWords :: String -> [String]
splitWords [] = [""]
splitWords x  = Prelude.words x
