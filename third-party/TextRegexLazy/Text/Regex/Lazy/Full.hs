-- | 'Full' is similar to 'Compat', as it exports functions with the
-- same names and signatures.  But it uses the left-to-right
-- first-matching strategy instead of longest-match, and handles more
-- complicated regular expressions.
--
-- In particular 'Full' implements lazy and possessive modifiers and
-- back references.
--
-- Patterns that accept empty strings are detected to prevent looping
-- or ridiculous results with substitution and splitting.
module Text.Regex.Lazy.All
    (StringRegex,StringInput,BoolMultiline,BoolCaseSensitive,StringSubgroups,StringSubPattern,StringBeforeMatch,StringOfMatch,StringAfterMatch,AboutMatch
    ,Regex,rnfRegex,mkRegex,mkRegexWithOpts,mkRegexWithOptions
    ,matchRegex,matchRegexAll
    ,subRegex,subRegex'
    ,splitRegex,splitRegex'
    ) where

{- By Chris Kuklewicz, 2006. BSD License, see the LICENSE file. -}

import Text.Regex.Lazy.Pattern(Pattern)
import Text.Regex.Lazy.Common
import Text.Regex.Lazy.ReadRegex(parseRegex)
import Text.Regex.Lazy.RegexParsecState(newState,updateUserState,getUserState)
import Text.Regex.Lazy.AllParsec(patternToParsec)
import qualified Data.IntMap as I (findWithDefault,lookup)
import Text.ParserCombinators.Parsec
import Numeric(readDec)
import Data.Char(isDigit)
import Control.Monad(liftM)
import Control.Monad.Identity(runIdentity)

-- | This is a thin wrapper around mkRegexWithOpts which sets multiline=True and caseSensitive=True
--
-- May call error.
mkRegex :: StringRegex -> Regex
mkRegex s = mkRegexWithOpts s True True

-- | This is a wrapper around patternToParsec.  It creates two
-- versions internally, one with captureGroups=True and one with
-- captureGroups=False.
--
-- May call error.
mkRegexWithOpts :: StringRegex -- ^ The string form of the 
                -> BoolMultiline   -- ^ multiline if True
                -> BoolCaseSensitive   -- ^ case-sensitive if True
                -> Regex  -- ^ Opaque parsed regular expression
mkRegexWithOpts s ml cs = runIdentity $ 
  mkRegexWithOptions (defaultRegexOption {multiline=ml
                                         ,caseSensitive=cs
                                         ,captureGroups=True
                                         ,strategy=Find_LongestMatch}) s

mkRegexWithOptions :: (Monad m) =>
                      RegexOption
                   -> StringRegex
                   -> m Regex
mkRegexWithOptions options s =
  case parseRegex s of
    Left parseError -> fail (show parseError)
    Right (pat,maxSubs) ->
      let r0  = patternToParsec (options {captureGroups=True})  pat
          r1  = patternToParsec (options {captureGroups=False}) pat
          r2  = patternToParsec (options {strategy=Find_All})   pat
      in return $ Regex {asString=s,asPattern=pat
                        ,capture=r0,capture'=r0
                        ,noCapture=r1,noCapture'=r1
                        ,allMatches=r2
                        ,groups=maxSubs}

-- | Matches a regular expression against a string.
matchRegex :: Regex -> StringInput -> Maybe [StringSubgroups]
matchRegex (Regex {capture=regex,groups=max}) s = 
  let result = runParser pOne (newState ()) "parsing with matchRegex" s
      pOne = (try regex) <|> (anyChar >> pOne)
  in case result of Left _ -> Nothing
                    Right [] -> Nothing -- impossible ?
                    Right xs -> Just (getElems max (head xs))

-- | Match a regular expression against a string, returning more
-- information about the match.
matchRegexAll :: Regex -> StringInput -> Maybe AboutMatch
matchRegexAll (Regex {capture=regex,groups=max}) start = 
  let result = runParser (loop 0) (newState ()) "matchRegexAll" start
  in either (const Nothing) Just result
  where
    -- Matches without having to be at the first position
    -- This is very sub-optimal for "^foo" patterns which are not multiline
    loop :: Int -> RegexParser () AboutMatch
    loop n | seq n True = try (match n) <|> (anyChar >> (loop (succ n)))
    match :: Int -> RegexParser () AboutMatch
    match n = do 
      subs <- regex
      if null subs then pzero -- impossible?
        else let subs' = head subs
                 (Just all) = I.lookup 0 subs'
             in do remaining <- getParserState
                   return (take n start,all,stateInput remaining,getElems max subs')

-- getElems is an internal function which helps mimic Text.Regex
getElems :: Int -> MatchedStrings -> [StringSubgroups]
getElems 0 _ = []
getElems n m = map (\k -> I.findWithDefault "" k m) [1..n]

-- | Replaces every occurance of the given regexp with the replacement
-- string. In the replacement string, "\1" refers to the first
-- substring; "\2" to the second, etc; and "\0" to the entire
-- match. "\\" will insert a literal backslash.
--
-- If the regex matches "" then it return the input unchanged
--
-- The input is evaluated lazily
subRegex :: Regex -> StringInput -> StringSubPattern -> String
subRegex r@(Regex {capture=regex,groups=max}) input subst = 
  let go s = either (error . show) id (runParser pSub (newState ()) "subRegex" s)
      pSub = option Nothing (do subs <- try regex
                                if null subs then return Nothing -- impossible?
                                  else let new = sub subst max (head subs)
                                       in do remaining <- getParserState
                                             return (Just (new,remaining)))
      loop :: StringInput -> String
      loop [] = []
      loop s@(x:xs) = case go s of 
                        Nothing -> x:loop xs
                        Just (new,r) -> new (loop (stateInput r))
  in if testAdvancing r then loop input else input

-- | This is identical to subRegex, but this is strict in its input
subRegex' :: Regex -> StringInput -> StringSubPattern -> String
subRegex' r@(Regex {capture'=regex,groups=max}) input subst = 
  let go :: String -> String
      go s = either (error . show) id (runParser (loop s 0) (newState id) "subRegex'" s)
      loop :: String -> Int -> RegexParser (String->String) String
      loop start n = (do subs <- try regex
                         if null subs then liftM ($ start) getUserState -- impossible?
                           else let new = sub subst max (head subs)
                                in do updateUserState (\f -> f . (takeOnto n start) . new)
                                      start' <- getInput
                                      loop start' 0)
               <|> (anyChar >> loop start (succ n))
               <|> (eof >> liftM ($ start) getUserState)
  in if testAdvancing r then go input else input

-- The usual take & append operation:
takeOnto :: Int -> String -> ShowS
takeOnto 0 _  e = e
takeOnto _ [] e = e
takeOnto n (x:xs) e = x:takeOnto (pred n) xs e

-- Do the replacement.  The semantics are from observing
-- Text.Regex.subRegex
sub :: StringSubPattern -> Int -> MatchedStrings -> ShowS
sub [] _ _ = id
sub ('\\':x:xs) max subs | x == '\\' = (x:) . (sub xs max subs)
                         | not (isDigit x) = (\e -> '\\': x:e). (sub xs max subs)
                         | otherwise =
  let ((n,xs'):_) = readDec (x:xs)
  in if n > max then error "index too large!"
       else ((I.findWithDefault "" n subs) ++) . (sub xs' max subs)
sub (x:xs) max subs = (x :) . (sub xs max subs)

-- | Splits a string based on a regular expression. The regular
-- expression should identify one delimiter.
--
-- The input is evaluated lazily
splitRegex :: Regex -> StringInput -> [String]
splitRegex r@(Regex {noCapture=regex}) input = 
  let go s = either (error . show) id (runParser pDel (newState ()) "splitRegex" s)
      pDel = option Nothing (do try regex
                                remaining <- getParserState
                                return (Just remaining))
      loop :: StringInput -> [String] -- not null
      loop [] = [[]]
      loop s@(x:xs) = case go s of Nothing -> let (bs:bss) = loop xs in (x:bs):bss
                                   Just r -> []:loop (stateInput r)
  in if testAdvancing r then loop input else [input]

-- | Splits a string based on a regular expression. The regular
-- expression should identify one delimiter.
--
-- This is strict in its input
splitRegex' :: Regex -> StringInput -> [String]
splitRegex' r@(Regex {noCapture'=regex}) input = 
  let go :: String -> [String]
      go s = either (error . show) id (runParser (loop s 0) (newState id) "splitRegex'" s)
      loop :: String -> Int -> RegexParser ([String]->[String]) [String]
      loop start n = (do try regex
                         updateUserState (\f -> f . (take n start :))
                         start' <- getInput
                         loop start' 0)
               <|> (anyChar >> loop start (succ n))
               <|> (eof >> liftM ($ [take n start]) getUserState)
  in if testAdvancing r then go input else [input]

-- Checks for a regex that accepts an empty string, return False if it does
testAdvancing :: Regex -> Bool
testAdvancing (Regex {noCapture=parser}) = 
    either (const True) (const False) $ runParser parser (newState ()) "testAdvancing" ""
