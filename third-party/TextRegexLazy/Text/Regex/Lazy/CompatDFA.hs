module Text.Regex.Lazy.CompatDFA
    (RegexDFA(..)
    ,mkRegexDFA
    ,matchRegexDFA
    ,matchRegexAllDFA
    ,subRegexDFA
    ,splitRegexDFA
    ,makeCompatDFA
    ,makeCompatDFACont) where

{- By Chris Kuklewicz, 2006. BSD License, see the LICENSE file. -}

import Text.Regex.Lazy.Common(StringBeforeMatch,StringRegex,StringInput,BoolMultiline,BoolCaseSensitive,StringSubgroups,StringSubPattern)

import Data.Maybe(fromJust)
import Text.Regex.Lazy.CompatReadRegex(decodePatternSet,parseRegex)
import Text.Regex.Lazy.DFAEngine
import Text.Regex.Lazy.Pattern
import qualified Data.Set as Set
import qualified Debug.Trace as T
trace a b = T.trace a b

data RegexDFA = RegexDFA StringRegex Pattern Regexp

mkRegex s = let Just (RegexDFA _ _ r) = mkRegexDFA s in r
-- This will return Nothing if there is a parse error or if the result
-- passed to dfaClean is False
mkRegexDFA :: StringRegex ->  Maybe (RegexDFA)
mkRegexDFA s = case parseRegex s of
                 Left pe -> Nothing
                 Right (p,_) -> if dfaClean p
                                  then Just (RegexDFA s p (makeCompatDFA p))
                                  else Nothing

matchRegexDFA :: RegexDFA       -- ^ The 'Regex' from 'mkRegex' or 'mkRegexWithOpts'
              -> StringInput    -- ^ The input string to be matched (lazily)
              -> Maybe StringOfMatch -- ^ Nothing if there is no match or Just String of the whole match
matchRegexDFA r input = case matchRegexAllDFA r input of
                          (_,Nothing) -> Nothing
                          (_,Just (match,_)) -> Just match

matchRegexAllDFA :: RegexDFA    -- ^ The 'Regex' from 'mkRegex' or 'mkRegexWithOpts'
                 -> StringInput -- ^ The input string to be matched (lazily)
                 -> (StringBeforeMatch,Maybe(StringOfMatch,StringAfterMatch)) -- ^ The result 
matchRegexAllDFA (RegexDFA _ _ r) input =
  let (~prefix,~mMatch) = findRegexS r input
      match = case mMatch of
                Nothing -> Nothing
                Just (fromHere,n,~rest) -> Just (take n fromHere,rest)
  in (prefix,match)

-- | Back references in the replacement string are ignored
--
-- If it ever accepts 0 character, the rest of the string will be
-- returned unprocessed.
subRegexDFA :: RegexDFA -- ^ The regex from mkRegexDFA
            -> StringInput   -- ^ The input to process
            -> StringSubPattern   -- ^ The replacement string
            -> String   -- ^ The output string
subRegexDFA (RegexDFA _ _ r) input subpat =
  let sub = if null subpat then id 
            else (subpat ++)
      loop s = let (prev,~mMatch) = findRegexS r s
                   next = case mMatch of
                            Nothing -> []
                            Just (_,0,~rest) -> rest
                            Just (_,_,~rest) -> sub ( loop rest )
               in  prev ++ next
  in loop input

-- | If it ever accepts 0 character, the rest of the string will be
-- returned unprocessed.
splitRegexDFA :: RegexDFA -> StringInput -> [String]
splitRegexDFA (RegexDFA _ _ r) input =
  let loop s = let (prefix,~mMatch) = findRegexS r s
                   next = case mMatch of
                            Nothing -> []
                            Just (_,0,~rest) -> [rest]
                            Just (_,_,~rest) -> loop rest
               in prefix : next             
  in loop input

-- If (dfaClean pat) is True then (makeCompatDFA pat) should not
-- throw an error.
makeCompatDFA :: Pattern -> Regexp
makeCompatDFA pIn = makeCompatDFACont pIn emptyOp

-- If (dfaClean pat) is True then (makeCompatDFACont pat) should not
-- throw an error.
makeCompatDFACont :: Pattern -> Regexp -> Regexp
makeCompatDFACont = reflectDFA
 where
  reflectDFA :: Pattern -> Regexp -> Regexp
  reflectDFA pIn cont =
    case pIn of
      PEmpty -> cont
-- The DFAEngine has no anchor support at the moment
      PCarat -> die
-- The DFAEngine has no anchor support at the moment
      PDollar -> die
      PGroup _ p -> reflectDFA p cont
      POr [] -> cont
      POr ps -> orRE (map (\p->reflectDFA p cont) ps)
      PConcat [] -> id
      PConcat ps -> foldr ($) cont (map reflectDFA ps)
      PQuest p -> quest (reflectDFA p emptyOp) cont
      PPlus p -> plus (reflectDFA p emptyOp) cont
      PStar p -> star (reflectDFA p emptyOp) cont
-- Handle PBound by reduction to simpler Pattern forms
      PBound 0 Nothing p -> reflectDFA (PStar p) cont
      PBound i Nothing p | 0<i -> reflectDFA (PConcat ((replicate i p)++[PStar p])) cont
                         | otherwise -> die
      PBound 0 (Just 0) p -> id
      PBound 0 (Just 1) p -> reflectDFA (PQuest p) cont
      PBound 0 (Just j) p | j >0 -> reflectDFA (PQuest (PConcat [p,PBound 0 (Just (pred j)) p])) cont
                          | otherwise -> die
      PBound i (Just j) p | 0<i && i == j -> reflectDFA (PConcat (replicate i p)) cont
                          | 0<i && i < j  -> reflectDFA (PConcat ((replicate i p)++[PBound 0 (Just (j-i)) p])) cont
                          | otherwise -> die
-- With longest match Lazy means the same as greedy, but throw an error anyway
      PLazy p -> die
-- With longest match the semantics of possessive can't be honored
      PPossessive _ -> die
      PDot -> allChar +> cont
      PAny patset -> let all = Set.toList . decodePatternSet $ patset
                     in alt all +> cont
      PAnyNot patset -> let all = Set.toList . decodePatternSet $ patset
                        in altNot all +> cont
      PEscape c -> char c +> cont
-- There is no capture, therefore there are no back references
      PBack _ -> die
      PChar c -> char c +> cont
      PString s -> string s +> cont
    where
      die = error (show pIn)
