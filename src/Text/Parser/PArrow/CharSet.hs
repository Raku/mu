{-# OPTIONS_GHC -O2 -fglasgow-exts #-}
module Text.Parser.PArrow.CharSet where

import Data.Char
import Data.Generics
import qualified Data.FastPackedString as Str

-- | Character sets
data CharSet = CS_Any        -- ^ All characters
             | CS_Word       -- ^ alphanum and \'_\'
             | CS_Whitespace -- ^ whitespace
             | CS_Newline    -- ^ newline
             | CS_Digit      -- ^ digits
             | CS_Alpha      -- ^ Alphabetical
             | CS_Alnum      -- ^ alpha or digit
             | CS_Ascii      -- ^ <=127
             | CS_Lower      -- ^ Lower
             | CS_Upper      -- ^ Upper
             | CS_Enum !Str.FastString -- ^ Enumeration
             | CS_Negated !CharSet
            deriving (Eq, Ord, Data, Typeable)

instance Show CharSet where
    show CS_Any        = "."
    show CS_Word       = "\\w"
    show CS_Whitespace = "\\s"
    show CS_Digit      = "\\d"
    show CS_Newline    = "\\n"
    show CS_Alpha      = "{alpha}"
    show CS_Alnum      = "{alnum}"
    show CS_Ascii      = "{ascii}"
    show CS_Lower      = "{lower}"
    show CS_Upper      = "{upper}"
    show (CS_Enum s)   = "<[" ++ Str.unpack s ++ "]>"
    show (CS_Negated (CS_Enum s)) = "<-[" ++ Str.unpack s ++ "]>"
    show (CS_Negated (CS_Negated x)) = show x
    show (CS_Negated x)= map toUpper (show x)

containsChar :: CharSet -> Char -> Bool
containsChar CS_Any = const True
containsChar CS_Word = \x -> case x of
    '_' -> True
    _   -> isAlphaNum x
containsChar CS_Whitespace = isSpace
containsChar CS_Newline    = (== '\n')
containsChar CS_Digit      = isDigit
containsChar CS_Ascii      = (< 128) . ord
containsChar CS_Alpha      = isAlpha
containsChar CS_Alnum      = isAlphaNum
containsChar CS_Lower      = isLower
containsChar CS_Upper      = isUpper
containsChar (CS_Enum x)   = (`Str.elem` x)
containsChar (CS_Negated x)= not . containsChar x
