-- | This module simplifies importing Text.Regex.Lazy.Full and all the
-- machinery it depends on.
--
-- This is not compatible with Text.Regex, though the same interface
-- is defined by Text.Regex.Lazy.Full.  If you want compatability with
-- Text.Regex then you must import Text.Regex.Lazy.Compat instead.
module Text.Regex.Lazy {- exports everything -} where

{- By Chris Kuklewicz, 2006. BSD License, see the LICENSE file. -}

import Text.Regex.Lazy.Common
import Text.Regex.Lazy.Pattern
import Text.Regex.Lazy.ReadRegex
import Text.Regex.Lazy.RegexParsecState
import Text.Regex.Lazy.FullParsec
import Text.Regex.Lazy.Full
