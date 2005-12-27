{-# OPTIONS_GHC -O2 -fglasgow-exts -funbox-strict-fields #-}
module Text.Parser.PArrow.Prim (runParser) where

import Text.Parser.PArrow.CharSet
import Text.Parser.PArrow.MD
import Data.FastPackedString (FastString)
import qualified Data.FastPackedString as Str

type UserState= ()
type Location = Int
data PS ustate = PS
    { psInput :: !FastString
    , psState :: !ustate
    , psIndex :: !Location
    }
data Res u r = POk !(PS u) r | PErr !Location ![String]
type PSF u r = PS u -> Res u r

optM :: MD i o -> PSF UserState o
optM = matcher

matcher :: forall i o. MD i o -> PSF UserState o
matcher (MNot x)         i = case optM x i of
                              POk _ _  -> PErr (psIndex i) ["not"]
                              PErr{}   -> POk i undefined
matcher (MChoice l)      i = let mm []     = PErr (psIndex i) ["no choice matches"]
                                 mm (c:cs) = case optM c i of
                                              POk s t -> POk s t
                                              _       -> mm cs
                             in mm l
matcher (MEqual{})      (PS s _ l) | l >= Str.length s = PErr l ["eof"]
matcher (MEqual c)      (PS s u l) = if (Str.index s l) == c then POk (PS s u (succ l)) c else PErr l ["expected "++[c]]
matcher (MSeq a b)       i = case optM a i of
                              POk s t -> case b of
                                          (MPure _ f) -> POk s (f t)
                                          _           -> optM b s
                              PErr l e -> PErr l e
matcher (MStar x)        i = let p = optM x
                                 sm st acc = case p st of
                                              POk st' r -> sm st' (r:acc)
                                              PErr{}    -> POk st (reverse acc)
                             in sm i []
matcher (MEmpty)        i = POk i (error "result for empty")
matcher (MPure _ _)     i = POk i (error "result for pure")
matcher (MCSet{})      (PS s _ l) | l >= Str.length s = PErr l ["eof"]
matcher (MCSet cs)     (PS s u l) = let x = (Str.index s l) in if x `elem` csetValue cs then POk (PS s u (succ l)) x else PErr l ["Expected "++show cs]
matcher (MParWire _ _)  _ = error "matcher on ParWire"
matcher (MJoin a b)     i = case optM a i of
                              POk s t -> case optM b s of
                                          POk s' t' -> POk s' (t,t')
                                          PErr l e -> PErr l e
                              PErr l e -> PErr l e
-- matcher _              (PS _ _ l) = PErr l ["unknown"]


-- | Run a parser producing either a list of error messages or output.
runParser :: MD i o -> FastString -> Either (Location, [String]) o
runParser md input = case optM md (PS input () 0) of
                      POk _ r  -> Right r
                      PErr l e -> Left (l, e)


