{-# OPTIONS_GHC -O2 -fglasgow-exts #-}
module Text.Parser.PArrow.CharSet where

import Data.Char

-- | Character sets
data CharSet = CS_Any        -- ^ All characters
             | CS_Word       -- ^ alphanum and \'_\'
             | CS_Whitespace -- ^ whitespace
             | CS_Digit      -- ^ digits
             | CS_Alpha      -- ^ Alphabetical
             | CS_Alnum      -- ^ alpha or digit
             | CS_Ascii      -- ^ <=127
             | CS_Lower      -- ^ Lower
             | CS_Upper      -- ^ Upper
               deriving(Eq)

instance Show CharSet where
    show CS_Any        = "."
    show CS_Word       = "\\w"
    show CS_Whitespace = "\\s"
    show CS_Digit      = "\\d"
    show CS_Alpha      = "{alpha}"
    show CS_Alnum      = "{alnum}"
    show CS_Ascii      = "{ascii}"
    show CS_Lower      = "{lower}"
    show CS_Upper      = "{upper}"


-- | List of the Chars contained in a CharSet.
csetValue :: CharSet -> [Char]
csetValue CS_Any = fmap chr [0..255]
csetValue CS_Word       = ('_':csetValue CS_Alnum)
csetValue CS_Whitespace = " \t\r\n"
csetValue CS_Digit      = "0123456789"
csetValue CS_Ascii      = fmap chr [0..127]
csetValue CS_Alpha      = filter isAlpha $ csetValue CS_Any
csetValue CS_Alnum      = csetValue CS_Digit ++ csetValue CS_Alpha
csetValue CS_Lower      = filter isLower $ csetValue CS_Alpha
csetValue CS_Upper      = filter isUpper $ csetValue CS_Alpha

