module Text.Regex.Lazy.Common where

import Text.Regex.Lazy.Pattern(Pattern)

import Text.ParserCombinators.Parsec (GenParser)

import Data.IntMap(IntMap)

-- | 'RegexOption' control whether the pattern is multiline or
-- case-sensitive like Text.Regex and whether to capture the subgroups
-- (\1, \2, etc).
data RegexOption = RegexOption {multiline :: Bool
                               ,caseSensitive :: Bool
                               ,captureGroups :: Bool
                               ,strategy::RegexOptionStrategy}

data RegexOptionStrategy = Find_LongestMatch 
                         | Find_FirstLeft 
                         | Find_FirstRight 
                         | Find_All

-- | This is a convenience value of RegexOption with multiline,
-- caseSensitive, and captureGroups all True and longestMatch False.
defaultRegexOption = RegexOption {multiline = True
                                 ,caseSensitive = True
                                 ,captureGroups = True
                                 ,strategy = Find_LongestMatch
                                 }

-- | 'FullState' is the opaque data type used to hold the RegexParser
-- state and a user defined state.
data FullState userStateType = FullState {userState :: !userStateType
                                         ,accepted :: !Int
                                         ,openSub :: !(IntMap (String,Int))
                                         ,closedSub :: !MatchedStrings}

-- | 'MatchedStrings' is an IntMap where the keys are PatternIndex
-- numbers and the values are completed substring captures.
type MatchedStrings = IntMap String

-- | 'RegexParser' is the type of CharParser that uses the state from this module.
type RegexParser userState = GenParser Char (FullState userState)


type RegexP = RegexParser () [MatchedStrings]
type RegexPS s = RegexParser s [MatchedStrings]
type BoolMultiline = Bool
type BoolCaseSensitive = Bool
type StringRegex = String
type StringInput = String
type StringBeforeMatch = String
type StringOfMatch = String
type StringAfterMatch = String
type StringSubgroups = String
type StringSubPattern = String
type AboutMatch = (StringBeforeMatch,StringOfMatch,StringAfterMatch,[StringSubgroups])

-- | Datatype memo-izing the parsed regular expression and meta-data
data Regex = Regex {asString::StringRegex,asPattern::Pattern
                   ,capture::RegexP,capture'::RegexPS ShowS
                   ,noCapture::RegexP,noCapture'::RegexPS ([String]->[String])
                   ,allMatches::RegexP
                   ,groups::Int}

-- | Since parsing is done lazily and may call error, it might help to
-- force Regex.  This simplifies the forcing.
rnfRegex :: Regex -> Regex
rnfRegex r@(Regex a1 a2 a3 a4 a5 a6 a7 a8) = a1!a2!a3!a4!a5!a6!a7!a8!r where (!) = seq
