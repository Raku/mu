{-# OPTIONS_GHC -O2 -fglasgow-exts #-}
module Text.Parser.PArrow.Char where

import Text.Parser.PArrow.CharSet
import Text.Parser.PArrow.MD
import Data.ByteString.Char8 (ByteString, pack)

-- | Match a single given character and return it.
char :: Char -> MD i ByteString
char    = MEqual . pack . (:[])

-- | Match a character.
anyChar :: MD i ByteString
anyChar = MCSet CS_Any

-- | Match any character in the given list.
anyOf   :: [Char] -> MD i ByteString
anyOf   = MChoice . (map char)

-- | Match a digit (0..9)
digit   :: MD i ByteString
digit   = MCSet CS_Digit

-- | Match a letter.
letter  :: MD i ByteString
letter  = MCSet CS_Alpha

-- | Match a letter or a digit.
alnum  :: MD i ByteString
alnum   = MCSet CS_Alnum

-- | Match a 'word' character - (alnum or '_')
wordChar:: MD i ByteString
wordChar= MCSet CS_Word

-- | Match a word consisting of wordChars.
{-
word   :: MD i ByteStringing
word    = many1 wordChar

-- | Match a sequence of whitespace.
spaces :: MD i ByteStringing
spaces  = many1 white
-}

-- | Match a single whitespace character.
white  :: MD i ByteString
white   = MCSet (CS_Whitespace)

-- | Match a constant string.
string :: ByteString -> MD i ByteString
string = MEqual
