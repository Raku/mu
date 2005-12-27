{-# OPTIONS_GHC -O2 -fglasgow-exts #-}
module Text.Parser.PArrow.Char where

import Control.Arrow
import Text.Parser.PArrow.CharSet
import Text.Parser.PArrow.MD
import Text.Parser.PArrow.Combinator

-- | Match a single given character and return it.
char :: Char -> MD i Char
char    = MEqual

-- | Match a character.
anyChar :: MD i Char
anyChar = MCSet CS_Any

-- | Match any character in the given list.
anyOf   :: [Char] -> MD i Char
anyOf   = MChoice . (map char)

-- | Match a digit (0..9)
digit   :: MD i Char
digit   = MCSet CS_Digit

-- | Match a letter.
letter  :: MD i Char
letter  = MCSet CS_Alpha

-- | Match a letter or a digit.
alnum  :: MD i Char
alnum   = MCSet CS_Alnum

-- | Match a 'word' character - (alnum or '_')
wordChar:: MD i Char
wordChar= MCSet CS_Word

-- | Match a word consisting of wordChars.
word   :: MD i String
word    = many1 wordChar

-- | Match a sequence of whitespace.
spaces :: MD i String
spaces  = many1 white

-- | Match a single whitespace character.
white  :: MD i Char
white   = MCSet (CS_Whitespace)

-- | Match a constant string.
string :: String -> MD i String
string []  = pure (\_ -> "")
string str = pure (\_ -> ' ') >>> foldr1 (>>>) (map char str) >>> pure (\_ -> str)
