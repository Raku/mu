-- | This module transforms a Text.Regex.Lazy.Pattern into a parsec
-- parser that is compatible with Text.Regex.  This means that lazy
-- and possesive patterns are errors and that and back-references are
-- treated as just escaped numbers matching the number.
--
-- The primary export is patternToParsec
module Text.Regex.Lazy.CompatParsec(RegexOption(..),RegexOptionStrategy(..),defaultRegexOption
                                   ,makeCompatParsec,makeCompatParsecCont) where

{- By Chris Kuklewicz, 2006. BSD License, see the LICENSE file. -}

import Text.Regex.Lazy.Pattern(simplify,Pattern(..),PatternIndex(..))
import Text.Regex.Lazy.Common
import Text.Regex.Lazy.CompatReadRegex(parseRegex,decodePatternSet)
import Text.Regex.Lazy.RegexParsecState
import Text.ParserCombinators.Parsec
import Data.Char(toLower,toUpper)
import Data.List(nub,sort,maximumBy)
import Control.Monad(liftM,when,replicateM_)
import Control.Monad.Fix(fix)
import qualified Data.Set as Set
import qualified Data.IntMap as I

-- | Create a Text.Regex compatible parsec parser from Pattern.  The
-- may call "error" if the pattern is unsuitable (has PLazy or
-- PProgressive or contains a NUL character or has an empty POr)
makeCompatParsec :: RegexOption -> Pattern -> RegexParser userState [MatchedStrings]
makeCompatParsec opt p = initState >> makeCompatParsecCont opt (simplify p) (finalState >>= return . (:[]))


-- | Worker function for makeCompatParsec, takes an additional future
-- Parsec continuation.
makeCompatParsecCont (RegexOption {multiline=multi,caseSensitive=sensitive}) = reflectParsec
 where
  reflectParsec :: Pattern -> RegexParser userState b -> RegexParser userState b
  reflectParsec pIn cont =
    case pIn of
         PEmpty -> cont
         PCarat -> if multi
                     then do col <- liftM sourceColumn getPosition
                             when (1/=col) (unexpected "Not anchored at start of line")
                             cont
                     else do pos <- getPosition
                             let (line,col) = (sourceLine pos,sourceColumn pos)
                             when (1/=line || 1/=col) (unexpected "Not anchored at start of input")
                             cont
         PDollar -> if multi then (lookAhead ((char '\n' >> return ()) <|> eof)) >> cont
                             else eof >> cont
         PGroup i p -> startSub i >> reflectParsec p (stopSub i >> cont)
         POr [] -> error "Empty POr Pattern"
-- Need to make a longest-match version of POr
--       POr ps -> msum $ map (\branch -> try (reflectParsec branch cont)) ps
         POr ps -> let branches = map (\p->reflectParsec p cont) ps in longestMatch branches
         PConcat ps -> foldr reflectParsec cont ps
         -- Greedy is the default
         PQuest p -> greedyOpt p cont
         PPlus p -> reflectParsec p (greedy p)
         PStar p -> greedy p
         PBound 0 Nothing p -> greedy p
         PBound i Nothing p | i>0 -> exact i p (greedy p)
                            | otherwise -> error $ "PBound with invalude parameters: "++show i++" and Nothing"
         PBound i (Just j) p | i==j -> exact i p cont
                             | i<j -> exact i p (greedyTo p (j-i))
                             | otherwise -> error $ "PBound with invalude parameters: "++show i++" and "++show j
         -- Lazy
         PLazy _ -> error "Lazy patterns are not compatible with Text.Regex"
         -- Possessive
         PPossessive _ -> error "Lazy patterns are not compatible with Text.Regex"
-- The operations below actually check the input for a match, accept
-- valid characters, and advance the state
         PDot -> if multi then parseChar (noneOf "\n\NUL") >> cont
                          else parseChar (noneOf "\NUL") >> cont
         PAny patset -> if sensitive
                          then let all = Set.toList . decodePatternSet $ patset
                                in if '\NUL' `elem` all then error "NUL in Pattern"
                                   else parseChar (oneOf all) >> cont
                          else let all = nub . sort $ concatMap ($ Set.toList (decodePatternSet patset)) [map toLower,map toUpper]
                               in if '\NUL' `elem` all then error "NUL in Pattern"
                                  else parseChar (oneOf all) >> cont
         PAnyNot patset -> if sensitive
                             then let all = Set.toList . decodePatternSet $ patset
                                   in if '\NUL' `elem` all then error "NUL in Pattern"
                                      else parseChar (noneOf all) >> cont
                           else let all = nub . sort $ concatMap ($ Set.toList (decodePatternSet patset)) [map toLower,map toUpper]
                                in if '\NUL' `elem` all then error "NUL in Pattern"
                                   else parseChar (noneOf all) >> cont
         PEscape '\NUL' -> error "NUL in Pattern"
         PEscape c -> acceptChar c >> cont
         PBack _ -> error "Cannot handle back references"
         PChar '\NUL' -> error "NUL in Pattern"
         PChar c -> acceptChar c >> cont
         PString s | '\NUL' `elem` s -> error "NUL in Pattern"
         PString s -> acceptString s >> cont
    where
      -- Define longestMatch so that POr works like it is supposed to
      howFar branch = lookAhead (do result <- try branch
                                    len <- lookupAccepted
                                    saveGame <- getParserState
                                    return (Just len,(saveGame,result)))
                      <|> return (Nothing,undefined)
      longestMatch branches = do
        all <- mapM howFar branches
        let best = foldl maxFst (Nothing,undefined) all
            maxFst a b = if (fst a) >= (fst b) then a else b
        case best of (Nothing,_) -> pzero
                     (Just len,(saveGame,result)) -> do setParserState saveGame
                                                        return result
      (<||>) a b = longestMatch [a,b]
      -- combinators for repetition
      exact i p cont' = foldr reflectParsec cont' (replicate i p)         -- lazy or greedy
      greedyOpt p c = reflectParsec p c <||> cont                         -- p?
      greedy p = fix safeOpt        -- fix (greedyOpt p)                  -- p*
        where safeOpt c = (do before <- lookupAccepted
                              let check = do after <- lookupAccepted
                                             if before < after then c else cont
                              reflectParsec p check) <||> cont
      greedyTo p n = foldr ($) cont (replicate n (greedyOpt p))           -- p{0,n}

      -- Do bookeeping when advancing, case sensitivity
      parseChar :: RegexParser userState Char -> RegexParser userState ()
      parseChar c = c >> incState

      acceptChar :: Char -> RegexParser userState ()
      acceptChar c = if sensitive then char c >> incState
                                  else charCase c >> incState

      acceptString :: String -> RegexParser userState ()
      acceptString s = if (not sensitive) && (map toLower s /= map toUpper s)
                         then sequence (map charCase s) >> plusState (length s)
                         else string s >> plusState (length s)

      charCase :: Char -> RegexParser userState Char
      charCase c | toLower c /= toUpper c = oneOf [toLower c, toUpper c]
                 | otherwise              = char c
