{-# OpTIONS_GHC -fglasgow-exts -funbox-strict-fields #-}

-- A Haskell port of PGE::OPTable.

module Text.Parser.OpTable where
import Prelude hiding (length)
import Data.Ratio
import Data.Map as NMap
import Data.Seq as NSeq
import Data.FastPackedString as NStr

data Op
    = Infix         { str :: !Str, assoc :: !Assoc }
    | Prefix        { str :: !Str }
    | Postfix       { str :: !Str }
    | Term          { str :: !Str }
    | Close         { str :: !Str }
    | Ternary       { str :: !Str, str2 :: !Str }
    | Circumfix     { str :: !Str, str2 :: !Str }
    | PostCircumfix { str :: !Str, str2 :: !Str }
    deriving (Eq, Show, Ord)

type Precedence = Ratio Int
type Arity = Int

data Token = MkToken
    { op    :: !Op
    , prec  :: !Precedence
    , ws    :: !Whitespace
    , arity :: !Arity
    }

data Match = MkMatch
    { matchOp   :: !Op
    , matchSubs :: !(Seq Match)
    }

data Assoc
    = !Non | !Left | !Right | !Chain | !List
    deriving (Eq, Show, Ord)

data Whitespace
    = !AllowWhitespace
    | !NoWhitespace
    deriving (Eq, Show, Ord)

data PrecRelation
    = DefaultPrec
    | SameAs        !Op
    | TighterThan   !Op
    | LooserThan    !Op
    deriving (Eq, Show, Ord)

data OpTable = MkOpTable
    { tokens    :: !(Map Op Token)
    , terms     :: !(Map Term Token)
    , ops       :: !(Map Term Token)
    , wsTerms   :: !(Map Term Token)
    , wsOps     :: !(Map Term Token)
    }

type Str = NStr.FastString
-- Terms are ordered by descending length first.
newtype Term = MkTerm Str deriving (Eq, Show)

instance Ord Term where
    compare (MkTerm x) (MkTerm y) = case compare (NStr.length y) (NStr.length x) of
        EQ -> compare x y
        o  -> o

addToken :: OpTable -> Op -> PrecRelation -> Whitespace -> OpTable
addToken table op prec ws = table -- { ... }

parse :: OpTable -> Str -> Match
parse = undefined
