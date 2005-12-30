{-# OPTIONS_GHC -O2 -fglasgow-exts -funbox-strict-fields #-}
module Text.Parser.PArrow.Prim (runParser, Res(..), PErr) where

import Text.Parser.PArrow.CharSet
import Text.Parser.PArrow.MD
import Data.FastPackedString (FastString(..), idx)
import Data.Seq ((|>))
import qualified Data.Seq as Seq
import qualified Data.FastPackedString as Str
import Data.IntMap (IntMap, singleton, empty, unionWith)
import Data.Generics

newtype ParseState = MkParseState { psInput :: FastString }
    deriving (Show, Eq, Ord, Typeable, Data)

data Res r
    = POk !ParseState r
    | PErr !PErr
    deriving (Show)
type PSF r = ParseState -> Res r
type PErr = IntMap Label

optM :: MD i o -> PSF o
optM = matcher

err :: ParseState -> MD i o -> Res o'
err i p = PErr (singleton (psIndex i) (label p))

psIndex :: ParseState -> Int
psIndex = idx . psInput

psEmptyMatch :: ParseState -> FastString
psEmptyMatch MkParseState{ psInput = (PS p s _) } = PS p s 0

matcher :: forall i o. MD i o -> PSF o
matcher p@(MNot x) i =
    case optM x i of
        POk _ _  -> err i p
        PErr{}   -> POk i undefined
matcher (MChoice l) i =
    let mm acc []     = PErr acc
        mm acc (c:cs) = case optM c i of
            POk s t -> POk s t
            PErr err -> mm (unionWith mappend acc err) cs
    in mm empty l
matcher p@(MEqual x)     i@MkParseState{psInput = s} = if Str.isPrefixOf x s
    then let (pre, post) = Str.splitAt (Str.length x) s in POk i{psInput = post} pre
    else err i p
matcher (MSeq a b)       i = case optM a i of
    POk s t -> case b of
        (MPure _ f) -> POk s (f t)
        _           -> optM b s
    PErr e -> PErr e
matcher (MEmpty)        i = POk i (error "result for empty")
matcher (MPure _ _)     i = POk i (error "result for pure")
matcher p@(MCSet _)   i@MkParseState{psInput = s} | Str.null s = err i p
matcher (MCSet CS_Any) i@MkParseState{psInput = s} = POk i{psInput = Str.tail s} (Str.take 1 s)
matcher p@(MCSet cs)   i@MkParseState{psInput = s} = if cs `containsChar` Str.head s
    then POk i{psInput=Str.tail s} (Str.take 1 s)
    else err i p
matcher (MParWire _ _)  _ = error "matcher on ParWire"
matcher (MJoin a b) i = case optM a i of
    POk s t -> case optM b s of
        POk s' t' -> POk s' (t,t')
        PErr e    -> maybe (PErr e) (\a' -> optM (MJoin a' b) i) (backtrack a i)
    PErr e -> PErr e
matcher (MGreedy min max x) i =
    let p = optM x
        sm st acc len | QuantInt len >= max = POk st (Right acc)
        sm st acc len = case p st of
            POk st' r -> sm st' (acc |> r) (succ len)
            PErr e | len < min -> PErr e
            _ -> POk st (if Seq.null acc
                then (Left (psEmptyMatch i))
                else (Right acc))
        in sm i Seq.empty 0
matcher (MLazy 0 _ _) i = POk i (Left (psEmptyMatch i))
matcher (MLazy min max x) i =
    let p = optM x
        sm st acc len | len >= min = POk st (Right acc)
        sm st acc len | QuantInt len >= max = POk st (Right acc)
        sm st acc len = case p st of
            POk st' r -> sm st' (acc |> r) (succ len)
            PErr e | len < min -> PErr e
            _ -> POk st (if Seq.null acc
                then (Left (psEmptyMatch i))
                else (Right acc))
        in sm i Seq.empty 0

backtrack :: MD i o -> ParseState -> Maybe (MD i o)
backtrack a@(MGreedy min _ x) i =
    let POk _ r = optM a i in
    either (const Nothing) (backup . Seq.length) r
    where
    backup n | n <= min  = Nothing
             | otherwise = Just (MGreedy min (QuantInt (pred n)) x)
backtrack a@(MLazy _ max x) i =
    let POk _ r = optM a i in
    either (const $ backup 0) (backup . Seq.length) r
    where
    backup n | QuantInt n >= max  = Nothing
             | otherwise = Just (MLazy (succ n) max x)
backtrack (MJoin a b) i = do
    b' <- backtrack b i
    return (MJoin a b')
backtrack (MSeq a b@(MPure{})) i = do
    a' <- backtrack a i
    return (MSeq a' b)
backtrack _ _ = Nothing

-- | Run a parser producing either a list of error messages or output.
runParser :: Show o => MD i o -> FastString -> Res o
runParser md input = optM md (MkParseState{psInput = input})
