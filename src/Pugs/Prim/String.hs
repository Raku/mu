{-# OPTIONS_GHC -fglasgow-exts #-}

module Pugs.Prim.String where
import Data.Char

-- perform char quotation according to original Perl 5 quotemeta
-- have to return a string because of the quote, this requires
-- concat in quotemeta above.
toQuoteMeta :: Char -> String
toQuoteMeta c =
   if not (isLatin1 c) -- Ignore Unicode characters beyond the 256-th
      || isAsciiUpper c || isAsciiLower c || isDigit c || c == '_'
      then [ c ]
      else [ '\\', c ]

