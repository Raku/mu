{-# OPTIONS_GHC -O2 -fglasgow-exts #-}
module Text.Parser.PArrow.Combinator where

import Control.Arrow
import Text.Parser.PArrow.MD

-- | Match the empty string.
empty :: MD i o
empty = MEmpty

-- | Match the empty string.
choice :: [MD i o] -> MD i o
choice = MChoice

-- | Match zero or more occurences of the given parser.
many :: MD i o -> MD i [o]
many = MStar

-- | Match one or more occurences of the given parser.
many1 :: MD i o -> MD i [o]
many1 x = (x &&& MStar x) >>> pure (\(b,bs) -> (b:bs))

-- | Match if the given parser does not match.
notFollowedBy :: MD i o -> MD i o
notFollowedBy = MNot

-- | Match one or more occurences of the given parser separated by the sepator.
sepBy1 :: MD i o -> MD i o' -> MD i [o]
sepBy1 p s = (many (p &&& s >>^ fst) &&& p) >>^ (\(bs,b) -> bs++[b])

-- | Match the given parser n times.
count :: Int -> MD i o -> MD i [o]
count 1 prim  = prim >>^ (\x -> [x])
count k p = (p &&& count (k-1) p) >>^  (\(b,bs) -> (b:bs))

-- | Match the first, middle and last argument, returning the value from the middle one.
between :: MD i t -> MD t close -> MD t o -> MD i o
between open close real = open >>> (real &&& close) >>^ fst

-- | Match optionally.
optional :: MD i o -> MD i (Maybe o)
optional iarr = MChoice [iarr >>> MPure "Just" Just, MPure "const Nothing" (const Nothing)]

-- | Sequence two parsers and return the result of the first one.
(>>!) :: (Arrow a) => a b c -> a b c' -> a b c
a >>! b = (a &&& b) >>^ fst
