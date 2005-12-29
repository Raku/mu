{-# OPTIONS_GHC -O2 -fglasgow-exts -funbox-strict-fields #-}
module Text.Parser.PArrow.Prim (runParser, Res(..), psInput) where

import Text.Parser.PArrow.CharSet
import Text.Parser.PArrow.MD
import Data.FastPackedString (FastString, idx, unpack, pack)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.FastPackedString as Str
import Data.IntMap (IntMap, insertWith, lookup, toAscList, elems, singleton, empty, union)

type UserState= ()
type Location = Int
data PS ustate = PS
    { psInput :: !FastString
    , psState :: !ustate
    }
data Res u r
    = POk !(PS u) r
    | PErr !PErr
type PSF u r = PS u -> Res u r
type PErr = IntMap Label

psIndex :: PS a -> Int
psIndex = idx . psInput

optM :: MD i o -> PSF UserState o
optM = matcher

err :: PS a -> MD i o -> Res u o
err i p = PErr (singleton (psIndex i) (label p))

matcher :: forall i o. MD i o -> PSF UserState o
matcher p@(MNot x)       i = case optM x i of
                              POk _ _  -> err i p
                              PErr{}   -> POk i undefined
matcher p@(MChoice l)    i = let mm acc []     = PErr acc
                                 mm acc (c:cs) = case optM c i of
                                              POk s t -> POk s t
                                              PErr err -> mm (acc `union` err) cs
                             in mm empty l
matcher p@(MEqual x)     i@(PS s u) = if Str.isPrefixOf x s
    then let (pre, post) = Str.splitAt (Str.length x) s in POk (PS post u) pre
    else err i p
matcher (MSeq a b)       i = case optM a i of
                              POk s t -> case b of
                                          (MPure _ f) -> POk s (f t)
                                          _           -> optM b s
                              PErr e -> PErr e
matcher (MStar x)        i = let p = optM x
                                 sm st acc = case p st of
                                              POk st' r -> sm st' (r:acc)
                                              PErr{}    -> POk st (reverse acc)
                             in sm i []
matcher (MEmpty)        i = POk i (error "result for empty")
matcher (MPure _ _)     i = POk i (error "result for pure")
matcher p@(MCSet _)   i@(PS s _) | Str.null s = err i p
matcher p@(MCSet CS_Any) (PS s u) = POk (PS (Str.tail s) u) (Str.take 1 s)
matcher p@(MCSet cs)   i@(PS s u) = if cs `containsChar` Str.head s
    then POk (PS (Str.tail s) u) (Str.take 1 s)
    else err i p
matcher (MParWire _ _)  _ = error "matcher on ParWire"
matcher (MJoin a b)     i = case optM a i of
                              POk s t -> case optM b s of
                                          POk s' t' -> POk s' (t,t')
                                          PErr e -> PErr e
                              PErr e -> PErr e
-- matcher _              (PS _ _ l) = PErr l ["unknown"]


-- | Run a parser producing either a list of error messages or output.
runParser :: MD i o -> FastString -> Res () o
runParser md input = optM md (PS input ())
