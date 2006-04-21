-- | This module is similar to CompatParsec, but produces a parser
-- with a configurable strategy.  CompatParsec takes all branches and
-- finds the longest match, but FullParsec can also take the branches
-- in left to right order and stop on the first successful match to
-- the full pattern.  This choice is made via the longestMatch field
-- of RegexOption.  To help control the the parser, this module
-- accepts lazy and possessive modifiers to help guide matching.
--
-- Unlike Text.Regex or Text.Regex.Lazy.Compat, NUL characters get no
-- special treatement and are permitted in the string form of regular
-- expressions and in the input to be matched.
--
-- Repetitions of a sub-pattern that accepts an empty string are
-- detected to prevent inifinite looping.  These checks for accepting
-- an empty string are not done if the sub-pattern can be proven to
-- never accept and empty string.
--
-- Capturing sub-group strings is all or nothing at the moment and is
-- controlled by RegexOption.  In neither case is the whole string
-- (group 0) captured.  That can be added by calling initState and
-- finalState before and after the parser returned by patternToParsec.
module Text.Regex.Lazy.AllParsec 
    (patternToParsec
    ,patternToParsecCont) where

{- By Chris Kuklewicz, 2006. BSD License, see the LICENSE file. -}

import Text.Regex.Lazy.Pattern(Pattern(..),PatternIndex,simplify,cannotMatchNull)
import Text.Regex.Lazy.Common
import Text.Regex.Lazy.ReadRegex(parseRegex,decodePatternSet)
import Text.Regex.Lazy.RegexParsecState -- import everything, since most of it is used
import Text.ParserCombinators.Parsec
import Data.Char(toLower,toUpper)
import Data.List(nub,sort)
import Control.Monad(msum,liftM,when,replicateM_)
import Control.Monad.Fix(fix)
import qualified Data.Set as Set

-- | This applies 'simplify' to the provided pattern and wraps the
-- parsec parser in initState and finalState so that the whole
-- matching string is assigned to group 0.
--
-- The returned parser does nothing to the user state.
--
-- For ill-formed patterns this may call 'error', such as for PBound
-- values with negative mino or max or with min > max.  It is also an
-- error if the Pattern contains back references but the captureGroups
-- RegexOption is set to False.  It is also an error if PLazy is
-- applied to anything but PQuest, PPlus, PStar, or PBound.
patternToParsec :: RegexOption -> Pattern -> RegexParser userState [MatchedStrings]
patternToParsec opt p = initState >> patternToParsecCont opt (simplify p) (finalState >>= return.(:[]))

-- | This takes an option structure and a Pattern and parser to act as
-- the continuation of the parser created from the Pattern.  This is
-- used to build patternToParsec.
--
-- The returned parser does nothing to the user state.
--
-- For ill-formed patterns this may call 'error', such as for PBound
-- values with negative mino or max or with min > max.  It is also an
-- error if the Pattern contains back references but the captureGroups
-- RegexOption is set to False.  It is also an error if PLazy is
-- applied to anything but PQuest, PPlus, PStar, or PBound.
patternToParsecCont :: RegexOption
		    -> Pattern
		    -> RegexParser userState [b]
		    -> RegexParser userState [b]
patternToParsecCont (RegexOption {multiline=multi
                                 ,caseSensitive=sensitive
                                 ,captureGroups=capture
                                 ,strategy=find}) = reflectParsec
 where
  reflectParsec :: Pattern -> RegexParser userState [b] -> RegexParser userState [b]
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
      PGroup i p -> if capture then startSub i >> reflectParsec p (stopSub i >> cont)
                               else reflectParsec p cont
-- There should be no empty POr patterns in a well-formed Pattern, but
-- 'error' is hard to catch so let it slide.
--    POr [] -> error "Empty POr Pattern"
      POr [] -> cont
-- Need to make a longest-match version of POr
      POr ps -> case find of
                  Find_LongestMatch -> let branches = map (\p -> reflectParsec p cont) ps 
                                       in longestMatch branches
                  Find_FirstLeft -> let branches = map (\p -> try (reflectParsec p cont)) ps
                                    in msum branches
                  Find_FirstRight -> let branches = map (\p -> try (reflectParsec p cont)) (reverse ps)
                                     in msum branches
                  Find_All -> let branches = map (\p -> reflectParsec p cont) ps 
                              in allBranches branches
-- There should be no empty PConcat patterns in a well-formed Pattern, but
-- 'error' is hard to catch so let it slide
--    PConcat [] -> error "Error PConcat Pattern"
      PConcat [] -> cont
      PConcat ps -> foldr reflectParsec cont ps
      -- Greedy is the default
      PQuest p -> greedyOpt p cont
      PPlus p -> reflectParsec p (greedy p)
      PStar p -> greedy p
      PBound 0 Nothing p -> greedy p
      PBound i Nothing p | i>0 -> exact i p (greedy p)
                         | otherwise -> error $ "PBound with invalude parameters: "++show i++" and Nothing"
      PBound i (Just j) p | i==j -> exact i p cont
                          | 0<=i && i<j  -> exact i p (greedyTo p (j-i))
                          | otherwise -> error $ "PBound with invalude parameters: "++show i++" and "++show j
      -- Lazy
      PLazy (PQuest p) -> lazyOpt p cont
      PLazy (PPlus p) -> reflectParsec p (lazy p)
      PLazy (PStar p) -> lazy p
      PLazy (PBound 0 Nothing p) -> lazy p
      PLazy (PBound i Nothing p) | i>0 -> exact i p (lazy p)
                                 | otherwise -> error $ "PBound with invalude parameters: "++show i++" and Nothing"
      PLazy (PBound i (Just j) p) | i==j -> exact i p cont
                                  | 0<=i && i<j  -> exact i p (lazyTo p (j-i))
                                  | otherwise -> error $ "PBound with invalude parameters: "++show i++" and "++show j
-- Applying PLazy to non-repeating patterns makes no sense and is an error
      PLazy err -> error $ "PLazy applied to invalid pattern : "++show err
      -- Possessive
      PPossessive (PQuest p) -> possessiveOpt p
      PPossessive (PPlus p) -> reflectParsec p (possessive p)
      PPossessive (PStar p) -> possessive p
      PPossessive (PBound 0 Nothing p) -> possessive p
      PPossessive (PBound i Nothing p) | i>0 -> exactPos i p (possessive p)
                                       | otherwise -> error $ "PBound with invalude parameters: "++show i++" and Nothing"
      PPossessive (PBound i (Just j) p) | i==j -> exactPos i p cont
                                        | 0<=i && i<j  -> exactPos i p (possessiveTo p (j-i))
                                        | otherwise -> error $ "PBound with invalude parameters: "++show i++" and "++show j
-- Applying PPossessive to other patterns makes sense, so instead of
-- an error...
--    PPossessive err -> error $ "PPossessive applied to invalid pattern : "++show err
-- ...the pattern is handled by giving a (reZero) continuation.
-- This will prevent backtracking to any 'try' statements created by
-- reflectParsec p once all of p matches.
      PPossessive p -> reflectParsec p (reZero) >> cont


-- The operations below actually check the input for a match, accept
-- valid characters, and advance the state
      PDot -> if multi then parseChar (noneOf "\n") >> cont
                       else parseChar anyChar >> cont
      PAny patset -> if sensitive
                       then let all = Set.toList . decodePatternSet $ patset
                            in parseChar (oneOf all) >> cont
                       else let all = nub . sort $ concatMap ($ Set.toList (decodePatternSet patset)) [map toLower,map toUpper]
                            in parseChar (oneOf all) >> cont
      PAnyNot patset -> if sensitive
                          then let all = Set.toList . decodePatternSet $ patset
                               in parseChar (noneOf all) >> cont
                          else let all = nub . sort $ concatMap ($ Set.toList (decodePatternSet patset)) [map toLower,map toUpper]
                               in parseChar (noneOf all) >> cont
      PEscape c -> acceptChar c >> cont
      PBack i -> if capture
                   then do maybeSub <- lookupSub i
                           case maybeSub of Nothing -> unexpected ("Cannot find subexpression \\"++show i)
                                            Just sub -> acceptString sub >> cont
                   else error "Pattern with back reference used with RegexOption captureGroups set to False"
      PChar c -> acceptChar c >> cont
      PString s -> acceptString s >> cont

    where
      -- Define longestMatch for Find_LongestMatch
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
      -- Define allBranches for Find_All
      maybeBranch branch = lookAhead (try branch) <|> return []
      allBranches branches = liftM concat (mapM maybeBranch branches)
      (<||>) = case find of
                 Find_LongestMatch -> \a b -> longestMatch [a,b]
                 Find_FirstLeft -> (<|>)
                 Find_FirstRight -> (<|>)
                 Find_All -> \a b -> allBranches [a,b]
      -- Provide shortcut to 'cont' when 'cps' matches zero characters and same Subs are in effect
      whenNull cps c = do before <- lookupAccepted
                          beforeSubs <- lookupSubs
                          cps (do after <- lookupAccepted
                                  if after > before 
                                    then c
                                    else do afterSubs <- lookupSubs
                                            if afterSubs == beforeSubs then cont -- shortcut
                                                                       else c)
      -- p{i} p{i,i}  There is no attempt to short-circuit accepting "" here
      exact i p cont' = foldr reflectParsec cont' (replicate i p)
      exactPos i p cont' = let p' = reflectParsec p (reZero)
                           in replicateM_ i p' >> cont'
      -- main p? p?* p?+ when you don't worry about accepting ""
      greedyOpt      p c = try (reflectParsec p c) <||> cont
      lazyOpt        p c = try cont                <||> (reflectParsec p c)
      possessiveOpt  p   = optional (reflectParsec p (reZero)) >> cont
      possessiveOpt' p c = (try (reflectParsec p (reZero)) >> c) <||> cont
      -- helper p? p?* p?+ when you are worried about accepting ""
      greedySafe     p c = try (whenNull (reflectParsec p) c) <||> cont
      lazySafe       p c = try cont <||> whenNull (reflectParsec p) c
      possessiveSafe p c = whenNull (try (reflectParsec p (reZero)) >>) c
      -- p* p*? p*+
      greedy     p = if cannotMatchNull p then fix (greedyOpt p)      else fix (greedySafe p)
      lazy       p = if cannotMatchNull p then fix (lazyOpt p)        else fix (lazySafe p)
      possessive p = if cannotMatchNull p then fix (possessiveOpt' p) else fix (possessiveSafe p)
      -- p{0,n} p{0,n}? p{0,n}+
      useTo n use = foldr ($) cont (replicate n use)
      greedyTo     p n = useTo n $ if cannotMatchNull p then greedyOpt p else greedySafe p
      lazyTo       p n = useTo n $ if cannotMatchNull p then lazyOpt p else lazySafe p
      possessiveTo p n = useTo n $ if cannotMatchNull p then possessiveOpt' p else possessiveSafe p
      -- Do bookeeping when advancing, check for case sensitivity option
      parseChar :: RegexParser userState Char -> RegexParser userState ()
      parseChar c = c >> incState
      acceptChar :: Char -> RegexParser userState ()
      acceptChar c = if sensitive then char c >> incState
                                  else foo c >> incState
      acceptString :: String -> RegexParser userState ()
      acceptString s = if (not sensitive) && (map toLower s /= map toUpper s)
                       then sequence (map foo s) >> plusState (length s)
                       else string s  >> plusState (length s)
      foo :: Char -> RegexParser userState Char
      foo c | toLower c /= toUpper c = oneOf [toLower c, toUpper c]
            | otherwise              = char c
      reZero = return []