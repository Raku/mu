{-# OPTIONS_GHC -O2 -fglasgow-exts #-}
module Text.Parser.PArrow.Char where

import Text.Parser.PArrow.CharSet
import Text.Parser.PArrow.MD
import Data.FastPackedString (FastString, pack)

-- | Match a single given character and return it.
char :: Char -> MD i FastString
char    = MEqual . pack . (:[])

-- | Match a character.
anyChar :: MD i FastString
anyChar = MCSet CS_Any

-- | Match any character in the given list.
anyOf   :: [Char] -> MD i FastString
anyOf   = MChoice . (map char)

-- | Match a digit (0..9)
digit   :: MD i FastString
digit   = MCSet CS_Digit

-- | Match a letter.
letter  :: MD i FastString
letter  = MCSet CS_Alpha

-- | Match a letter or a digit.
alnum  :: MD i FastString
alnum   = MCSet CS_Alnum

-- | Match a 'word' character - (alnum or '_')
wordChar:: MD i FastString
wordChar= MCSet CS_Word

-- | Match a word consisting of wordChars.
{-
word   :: MD i FastStringing
word    = many1 wordChar

-- | Match a sequence of whitespace.
spaces :: MD i FastStringing
spaces  = many1 white
-}

-- | Match a single whitespace character.
white  :: MD i FastString
white   = MCSet (CS_Whitespace)

-- | Match a constant string.
string :: FastString -> MD i FastString
string = MEqual
