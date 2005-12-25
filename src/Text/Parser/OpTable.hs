{-# OpTIONS_GHC -fglasgow-exts -funbox-strict-fields #-}

-- A Haskell port of PGE::OPTable.

module Text.Parser.OpTable where
import Prelude hiding (length, lookup)
import Data.Ratio
import Data.Map as NMap
import Data.Seq as NSeq
import Data.FastPackedString as NStr

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

type DynStr = Str -> Maybe (Str, Op -> Seq Match)

instance Eq DynStr where _ == _ = False
instance Ord DynStr where compare _ _ = LT
instance Show DynStr where show _ = "<dyn>"

type Precedence = Ratio Int
type Arity = Int

data Token = MkToken
    { tokOp    :: !Op
    , tokPrec  :: !Precedence
    , tokArity :: !Arity
    , tokClose :: !(Maybe Op)
    }
    deriving (Eq, Show, Ord)

data Match = MkMatch
    { matchOp   :: !Op
    , matchSubs :: !(Seq Match)
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
    , tableOps       :: !TokenMap
    , tableWsTerms   :: !TokenMap
    , tableWsOps     :: !TokenMap
    }
    deriving (Eq, Show, Ord)

emptyTable :: OpTable
emptyTable = MkOpTable NMap.empty NMap.empty NMap.empty NMap.empty NMap.empty

type Str = NStr.FastString
type EntryMap = Map Op Token
type TokenMap = Map Term Token

-- | Terms are ordered by descending length first.
newtype Term = MkTerm Str deriving (Eq, Show)

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
    { tableOps     = insertTok tok (tableOps table)     }
insertOp   tok AllowWhitespace table = table
    { tableOps     = insertTok tok (tableOps table)
    , tableWsOps   = insertTok tok (tableWsOps table) }

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
    TighterThan {}  -> prec / 2
    LooserThan {}   -> prec / 2 * 3
    where
    prec = tokPrec (toks ! (relOp rel))

parse :: OpTable -> Str -> Match
parse = undefined

mkOpTable :: [[(Whitespace, Op)]] -> OpTable
mkOpTable = fst . Prelude.foldl mkOps (emptyTable, DefaultPrec)
    where
    mkOps x [] = x
    mkOps (tbl, rel) [(ws, op)] = (addToken tbl op rel ws, LooserThan op)
    mkOps (tbl, rel) ((ws, op):xs) = mkOps (addToken tbl op rel ws, rel) xs

testTable :: OpTable
testTable = mkOpTable
    [ mk Circumfix       "( )"
    , mk Term            "0 1 2 3 4 5 6 7 8 9"
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

