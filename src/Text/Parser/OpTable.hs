{-# OpTIONS_GHC -fglasgow-exts -funbox-strict-fields #-}

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
        , dynOpTrans    :: !(OpTable -> OpTable)
        }

instance Eq DynStr where _ == _ = True
instance Ord DynStr where compare _ _ = EQ
instance Show DynStr where show _ = "<dyn>"

type Precedence = Ratio Int
type Arity = Int

data Token = MkToken
    { tokOp    :: !Op
    , tokPrec  :: !Precedence
    , tokArity :: !Arity
    , tokClose :: !(Maybe Op)
    , tokNull  :: !Bool -- null-width assertion
    }
    deriving (Eq, Show)

instance Ord Token where
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

data OpTable = MkOpTable
    { tableEntries   :: !EntryMap
    , tableTerms     :: !TokenMap
    , tableOpers     :: !TokenMap
    , tableWsTerms   :: !TokenMap
    , tableWsOpers   :: !TokenMap
    }
    deriving (Eq, Show, Ord)

emptyTable :: OpTable
emptyTable = MkOpTable NMap.empty NMap.empty NMap.empty NMap.empty NMap.empty

type Str = NStr.FastString
type EntryMap = Map Op Token
type TokenMap = Map Term Token

-- | Terms are ordered by descending length first.
newtype Term = MkTerm { termToStr :: Str } deriving (Eq, Show)

termLength :: Term -> Int
termLength = length . termToStr

instance Ord Term where
    compare (MkTerm x) (MkTerm y) = case compare (NStr.length y) (NStr.length x) of
        EQ -> compare x y
        o  -> o

addToken :: OpTable -> Op -> PrecRelation -> Whitespace -> OpTable
addToken table op rel ws = doCloseOp . doInsert $ table{ tableEntries = ents' }
    where
    ents  = tableEntries table
    ents' = doCloseEntry $ insert op tok ents
    tok   = MkToken
        { tokOp    = op
        , tokPrec  = calculatePrec rel ents
        , tokArity = arityOf op
        , tokClose = maybeOpClose
        , tokNull  = False
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
        { tokOp     = opClose
        , tokPrec   = tokPrec tok
        , tokArity  = 0
        , tokClose  = Nothing
        , tokNull   = False
        }

arityOf :: Op -> Arity
arityOf Close{}         = 0
arityOf Ternary{}       = 3
arityOf Infix{}         = 2
arityOf PostCircumfix{} = 2
arityOf _               = 1

insertBy :: Op -> Token -> Whitespace -> OpTable -> OpTable
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

insertTerm, insertOp :: Token -> Whitespace -> OpTable -> OpTable
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

insertTok :: Token -> TokenMap -> TokenMap
insertTok tok tmap = insert key tok tmap
    where
    key = MkTerm $ str (tokOp tok)

defaultPrec :: Precedence
defaultPrec = 1%1

calculatePrec :: PrecRelation -> EntryMap -> Precedence
calculatePrec DefaultPrec _ = defaultPrec
calculatePrec rel toks = case rel of
    SameAs {}       -> prec
    LooserThan {}   -> prec / 2
    TighterThan {}  -> prec / 2 * 3
    where
    prec = tokPrec (toks ! (relOp rel))

type TokenStack = [Token]
type TermStack  = [Match]
type OperStack  = [Match]

type Parse a = ( ?termStack :: TermStack, ?tokenStack :: TokenStack, ?operStack :: OperStack
               , ?tbl :: OpTable, ?str :: Str) => a

parse :: OpTable -> Str -> Match
parse tbl str = let ?termStack  = []
                    ?tokenStack = []
                    ?operStack  = []
                    ?tbl    = tbl 
                    ?str    = str in expectTerm

expectTerm :: Parse Match
expectTerm
    | null ?str = nullTerm
    | otherwise = let ?str = str' in matchWith foundTerm nullTerm terms
    where
    str'  = dropSpace ?str
    terms = (if length str' == length ?str then tableTerms else tableWsTerms) ?tbl

matchWith :: Parse (Parse (Token -> a) -> Parse a -> TokenMap -> a)
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

foundTerm :: Parse (Token -> Match)
foundTerm token
    | isTerm (tokOp token) = pushTermStack token expectOper
    | otherwise            = operShift token

pushTermStack :: Parse (Token -> Parse a -> Parse a)
pushTermStack token p = let ?termStack = (mkMatch token: ?termStack) in p

pushOperStack :: Parse (Token -> Parse a -> Parse a)
pushOperStack token p = let ?operStack = (mkMatch token: ?operStack) in p

pushTokenStack :: Parse (Token -> Parse a -> Parse a)
pushTokenStack token p = let ?tokenStack = (token: ?tokenStack) in p

mkMatch :: Token -> Match
mkMatch token = (MkMatch (tokOp token) NSeq.empty)

expectOper :: Parse Match
expectOper
    | null str' = endParse
    | otherwise = let ?str = str' in matchWith foundOper endParse opers
    where
    str'  = dropSpace ?str
    opers = (if length str' == length ?str then tableOpers else tableWsOpers) ?tbl

nullTerm :: Parse Match
nullTerm | (t@MkToken{ tokNull = True }:_) <- ?tokenStack = pushTermStack t expectOper
         | otherwise = error "Missing term"

foundOper :: Parse (Token -> Parse Match)
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

operShift :: Parse (Token -> Parse Match)
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

operReduce :: Token -> Parse Match 
operReduce oper = reduce (foundOper oper)

endParse :: Parse Match
endParse
    | [] <- ?tokenStack = head ?termStack
    | otherwise         = reduce endParse

reduce :: Parse (Parse Match -> Match)
reduce p = case ?tokenStack of
    (MkToken{tokOp=Close{}}:t:ts) ->
        let ?operStack  = tail ?operStack
            ?tokenStack = ts
         in reduce1 (tokArity t) p
    (t:ts)  -> let ?tokenStack = ts in reduce1 (tokArity t) p
    _       -> error "reducing an empty token stack"

reduce1 :: Parse (Arity -> Parse Match -> Match)
reduce1 arity p =
    let (op:opers)    = ?operStack
        (args, terms) = splitAt arity ?termStack
     in let ?operStack = opers
            ?termStack = (op{matchArgs = fromList (reverse args)}:terms) in p

mkOpTable :: [[(Whitespace, Op)]] -> OpTable
mkOpTable = fst . Prelude.foldl mkOps (emptyTable, DefaultPrec)
    where
    mkOps x [] = x
    mkOps (tbl, rel) [(ws, op)] = (addToken tbl op rel ws, LooserThan op)
    mkOps (tbl, rel) ((ws, op):xs) = mkOps (addToken tbl op rel ws, rel) xs

testTable :: OpTable
testTable = mkOpTable
    [ mk Circumfix       "( )"
    , mk Term            (span isDigit)
    , mk Infix           "* /"
    , mk Infix AssocLeft "+ -"
    ]

class MkClass a where mk :: a

instance MkClass ((Str -> Op) -> [Char]          -> [(Whitespace, Op)]) where
    mk op1 = Prelude.map (((,) AllowWhitespace) . op1 . pack) . Prelude.words

instance MkClass ((Str -> Str -> Op) -> [Char]   -> [(Whitespace, Op)]) where
    mk op2 = Prelude.map (((,) AllowWhitespace) . uncurry op2) . pack2 . Prelude.words
        where
        pack2 (x:y:zs)  = ((pack x, pack y):pack2 zs)
        pack2 _         = []

instance MkClass ((Str -> Assoc -> Op) -> [Char] -> [(Whitespace, Op)]) where
    mk op1 = mk op1 AssocLeft

instance MkClass ((Str -> Assoc -> Op) -> Assoc -> [Char] -> [(Whitespace, Op)]) where
    mk op1 assoc = Prelude.map (((,) AllowWhitespace) . (`op1` assoc) . pack) . Prelude.words

instance (MkClass ((Str -> Op) -> (Str -> (Str, Str)) -> [(Whitespace, Op)])) where
    mk op1 f = [(AllowWhitespace, DynTerm empty dyn)]
        where
        dyn str = let (pre, post) = f str in
            if null pre then Nothing else Just (DynResultMatch pre post)

