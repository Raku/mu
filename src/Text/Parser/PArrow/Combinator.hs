{-# OPTIONS_GHC -O2 -fglasgow-exts #-}
module Text.Parser.PArrow.Combinator where

import Control.Arrow
import Text.Parser.PArrow.MD
import Data.Seq (Seq, (<|), singleton)

-- | Match the empty string.
choice :: [MD i o] -> MD i o
choice = MChoice

-- | Match zero or more occurences of the given parser.
many :: MD i o -> MD i (MStarRes o)
many = MGreedy 0 QuantInf

-- | Match one or more occurences of the given parser.
many1 :: MD i o -> MD i (Seq o)
many1 x = MGreedy 1 QuantInf x >>^ fromRight
    where
    fromRight (Right x) = x
    fromRight _ = error "impossible"

-- | Match if the given parser does not match.
notFollowedBy :: MD i o -> MD i o
notFollowedBy = MNot

-- | Match one or more occurences of the given parser separated by the sepator.
sepBy1 :: MD i o -> MD i o' -> MD i (Seq o)
sepBy1 p s = (many (p &&& s >>^ fst) &&& p) >>^ (\(bs,b) -> either (const $ singleton b) (b <|) bs)

-- | Match the given parser n times.
count :: Int -> MD i o -> MD i [o]
count 1 prim  = prim >>^ (\x -> [x])
count k p = (p &&& count (k-1) p) >>^  (\(b,bs) -> (b:bs))

-- | Match the first, middle and last argument, returning the value from the middle one.
between :: MD i t -> MD t close -> MD t o -> MD i o
between open close real = open >>> (real &&& close) >>^ fst

-- | Match optionally.
optional :: MD i o -> MD i (Maybe o)
optional iarr = MChoice [iarr >>> MPure mempty Just, MPure mempty (const Nothing)]

-- | Sequence two parsers and return the result of the first one.
(>>!) :: (Arrow a) => a b c -> a b c' -> a b c
a >>! b = (a &&& b) >>^ fst
