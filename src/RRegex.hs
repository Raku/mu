{-# OPTIONS -fglasgow-exts #-}

{-
    Regular expressions, based on PCRE.

    A king he was on carven throne
    In many-pillared halls of stone
    With golden roof and silver floor,
    And runes of power upon the door...
-}

module RRegex (
    -- * Regular expressions
    Regex,
    mkRegex,
    mkRegexWithOpts,
    matchRegex,
    matchRegexAll
  ) where

import Prelude
import RRegex.PCRE 

import System.IO.Unsafe
import RRegex.Syntax
import Array

-- | Makes a regular expression with the default options (multi-line,
-- case-sensitive).  The syntax of regular expressions is
-- otherwise that of @egrep@ (i.e. POSIX \"extended\" regular
-- expressions). Note: this is arguably the incorrect default. single line 
-- is the default everywhere else.

mkRegex :: String -> Regex

-- | Makes a regular expression, where the multi-line and
-- case-sensitve options can be changed from the default settings.
mkRegexWithOpts
   :: String  -- ^ The regular expression to compile
   -> Bool    -- ^ 'True' @\<=>@ @\'^\'@ and @\'$\'@ match the beginning and 
	      -- end of individual lines respectively, and @\'.\'@ does /not/
	      -- match the newline character.
   -> Bool    -- ^ 'True' @\<=>@ matching is case-sensitive
   -> Regex   -- ^ Returns: the compiled regular expression

-- | Match a regular expression against a string
matchRegex
   :: Regex	-- ^ The regular expression
   -> String	-- ^ The string to match against
   -> Maybe [String]	-- ^ Returns: @'Just' strs@ if the match succeeded
			-- (and @strs@ is the list of subexpression matches),
			-- or 'Nothing' otherwise.

-- | Match a regular expression against a string, returning more information
-- about the match.
matchRegexAll
   :: Regex	-- ^ The regular expression
   -> String	-- ^ The string to match against
   -> Maybe ( String, String, String, [String] )
		-- ^ Returns: 'Nothing' if the match failed, or:
		-- 
		-- >  Just ( everything before match,
		-- >         portion matched,
		-- >         everything after the match,
		-- >         subexpression matches )

mkRegex s = mkRegexWithOpts s False True
mkRegexWithOpts s single_line case_sensitive = unsafePerformIO $
            compile s (pcreUtf8 + newline + igcase) >>= \x -> case x of
                Left (i,err) -> fail $ "PCRE Regular Expression Error:\n" ++ s ++ "\n" ++ replicate i ' ' ++ "^ " ++ err
                Right p -> return p
      where
	newline | single_line = pcreMultiline
		| otherwise   = 0

	igcase  | case_sensitive = 0 
		| otherwise 	 = pcreCaseless

matchRegex p str = fmap f (str =~~ p) where
    f :: Array Int String -> [String]
    f a = tail (elems a)


matchRegexAll p str = fmap f (str =~~ p) where
    f MR { mrBefore = b, mrAfter = a, mrSubList = sl, mrMatch = m } = (b,m,a,sl)
