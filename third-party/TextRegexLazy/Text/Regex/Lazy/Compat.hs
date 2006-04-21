-- | This is intended to be a drop in replacement for Text.Regex
-- module, using lazy Parsec instead of marshalling to and from
-- "regex.h" calls.  The module should be indistinguishable from
-- Text.Regex, except that it operates in a lazy manner on the input
-- string (which allows it to handle some infinite input strings or
-- input which may have errors further along the input if evaluated
-- strictly).
--
-- Note that the lazy property is not perfect, see the individual
-- functions for notes about this.  In particular the first character
-- of the the input stirng is always evaluated.
module Text.Regex.Lazy.Compat
  (StringRegex,StringInput,BoolMultiline,BoolCaseSensitive,StringSubgroups,StringSubPattern,StringBeforeMatch,StringOfMatch,StringAfterMatch,AboutMatch
  ,Regex,rnfRegex,mkRegex,mkRegexWithOpts
  ,matchRegex,matchRegexAll
  ,subRegex,splitRegex) where

{- By Chris Kuklewicz, 2006. BSD License, see the LICENSE file. -}

import Text.Regex.Lazy.Common
import qualified Text.Regex.Lazy.CompatReadRegex as CompatReadRegex (parseRegex)
import qualified Text.Regex.Lazy.CompatParsec as CompatParsec (makeCompatParsec)
import qualified Text.Regex.Lazy.All as All hiding (mkRegex,mkRegexWithOpts)
import qualified Data.IntMap as I
import Text.ParserCombinators.Parsec
import Numeric(readDec)
import Data.Char(isDigit)

-- | This is equivalent to 'mkRegexWithOpts' with both options 'True'
mkRegex :: StringRegex -> Regex
mkRegex s = mkRegexWithOpts s True True

-- | Turn a string into a regular expression.  This may fail, but then
-- it will call "error" with an informative string (thanks to using
-- Parsec to interpret the string).  Note that NUL characters are not
-- allowed (and '.' does not match a NUL character).
-- 
-- The syntax of regular expressions is otherwise that of egrep
-- (i.e. POSIX "extended" regular expressions).
mkRegexWithOpts :: StringRegex -- ^ The regular expression string
                -> BoolMultiline   -- ^ 'True' means multiline: ^ and $ match after/before newline, 
                          --   '.' does not match newline.
                -> BoolCaseSensitive   -- ^ 'True' means case-sensitive character matching
                -> Regex  -- ^ The compiled regular expression
mkRegexWithOpts s ml cs =
  case CompatReadRegex.parseRegex s of
    Left s -> error (show s)
    Right (pat,maxSubs) -> 
      let r0 = CompatParsec.makeCompatParsec
                 (defaultRegexOption 
                  {multiline=ml
                  ,caseSensitive=cs}) pat
          r1 = CompatParsec.makeCompatParsec
                 (defaultRegexOption 
                  {multiline=ml
                  ,caseSensitive=cs
                  ,captureGroups=False}) pat
          r2 = CompatParsec.makeCompatParsec
                 (defaultRegexOption 
                  {multiline=ml
                  ,caseSensitive=cs
                  ,strategy=Find_All}) pat
      in Regex {asString=s,asPattern=pat
               ,capture=r0,capture'=r0
               ,noCapture=r1,noCapture'=r1
               ,allMatches=r2
               ,groups=maxSubs}

matchRegex = All.matchRegex

matchRegexAll = All.matchRegexAll

subRegex = All.subRegex

splitRegex = All.splitRegex
