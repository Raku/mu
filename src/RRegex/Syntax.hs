{-# OPTIONS -cpp -fglasgow-exts  #-}

-- arch-tag: 1498020a-9de2-44e3-ae69-14bb1febb77e

{- |

basic usage:

string =~ 'regular expression' returns different things depending on context


type - what it evaluates to
---------------------------
Int - number of times the regular expression matches
String -  matching portion of string
(String,String,String) - (text before match, matching text, text after match)
[Either String String] - list of matching and nonmatching strings, if concated, the original string results.  Left = notmatching, Right = matching.
Bool - whether the string matches
() - always returns () (useful in monad context, see below)
[String] - list of matches
Array Int String - list of substring matches for first match 
(String, Array Int String) - full matching text and substring matches
[(String, Array Int String)] - all matches, full match plus substrings
[Array Int String] - all substrings from all matches


also, there is the monadic version (=~~) which always behaves exactly the same
as (=~) except when the match fails, instead of returning a default value, the
monad fails. 

regular expressions:

these may be strings, which are interpreted as regular expressions, or Regex's
from the Text.Regex module. or any other instance of the RegexLike class. 

when using strings, you may prefix the regex by "(?flags)" where flags is one
of 'i' for a case insensitive match and 'm' means a multi-line match. other 
flags may be available depending on your implementation

advanced features:

not just strings can be matched, but rather lists of anything a matcher is
defined for.  RegexLikeImp data class can be used for in-place code generated
by template haskell for compile-time checked regular expresions


-}


module RRegex.Syntax(
    RegexLike(..), 
    RegexContext(..), 
    (!~), 
    MatchResult(..)) where

import Array
import RRegex.PCRE as PCRE
--import RRegex
import Maybe
import Monad
import System.IO.Unsafe


-- | instances of this class may be used as regular expressions with this syntax.

class RegexLike r a | r -> a where
    -- | Test whether the regex matches at all
    matchTest :: r -> [a] -> Bool
    -- | Count the number of times the regex matches
    matchCount :: r -> [a] -> Int
    -- | return all matches
    matchAll  :: r -> [a] -> [(Array Int (Int,Int))]
    -- | match once
    matchOnce :: r  -- ^ Regular Expression
            -> [a]  -- ^ String to match
            -> Bool -- ^ Whether we are at the begining of the string (as opposed to continuing a previous match)
            -> Maybe (Array Int (Int,Int))  -- ^ array of matched expression
    matchShow :: r -> String  -- ^ Used for error messages
    matchTest r xs = isJust (matchOnce r xs True)
    matchCount r xs = length (matchAll r xs)
    matchAll r xs = f 0 xs where
        f t xs = case matchOnce r xs (t == 0) of 
            Nothing -> []
            Just a  | l == 0 -> [na]
                    | otherwise -> na:f (t + x) (drop x xs) where
                (o,l) = a!0
                x = o + l  
                na = fmap (adj t) a
        adj t (x,y) = ((,) $! x + t) $! y
    matchShow _ = "Unknown"

data MatchResult a = MR {
    mrBefore :: [a],
    mrMatch  :: [a],
    mrAfter  :: [a],
    mrSubList :: [[a]],
    mrSubs   :: Array Int [a]
}   

instance RegexLike PCRE.Regex Char where
    matchOnce re cs bol = unsafePerformIO (PCRE.execute re cs (if not bol then pcreNotbol else 0))
    matchShow _ = "PCRE Regex"

class RegexContext x a where
    -- | match a list against a regular expression, changing its behavior based on its result type.
    (=~) :: RegexLike r x => [x] -> r -> a
    -- | Monadic version of (=~). behaves identically, except it causes the monad to fail when the expression does not match, rather than returning a default value.
    (=~~) :: (Monad m, RegexLike r x) => [x] -> r -> m a

-- | check if regular expression does not match
(!~) :: RegexLike r x => [x] -> r -> Bool
s !~ re = not (s =~ re)

regexFailed re =  fail $ "regex failed to match: " ++ matchShow re

-- | return number of expressions matched
instance  RegexContext x Int where
    s =~ re =  matchCount re s 
    s =~~ re = case (s =~ re) of
        0 -> regexFailed re
        xs -> return $  xs

instance RegexContext x ([x],[x],[x]) where
    s =~ re = maybe (s,[],[]) id (s =~~ re)
    s =~~ re =  case matchOnce re s True of 
        Nothing -> regexFailed re 
        Just a  -> let (o,l) = a!o in return (take o s,take l (drop o s),drop (o + l) s)

instance RegexContext x ([x],[x],[x],Array Int [x]) where
    s =~ re = maybe (s,[],[], listArray (1,0) []) id (s =~~ re)
    s =~~ re =  case s =~~ re of 
        Nothing -> regexFailed re 
        Just z -> return (mrBefore z,mrMatch z, mrAfter z, mrSubs z)

instance RegexContext x (MatchResult x) where
    s =~ re = maybe MR {mrBefore = s,mrMatch = [],mrAfter = [],mrSubs = listArray (1,0) [], mrSubList = []} id (s =~~ re)
    s =~~ re =  case matchOnce re s True of 
        Nothing -> regexFailed re 
        Just z ->  return $ MR {mrBefore = take o s, mrAfter = drop (o + l) s, mrMatch = a!0, mrSubs = a, mrSubList = tail (elems a) } where
            a = fmap f z
            f (o,l) = take l (drop o s)
            (o,l) = z!0 

{-
instance RegexContext x [Either [x] [x]] where
    s =~ re = map f $  matchAll re s where
        f (Left s) = Left s
        f (Right (x,_)) = Right x
    s =~~ re = case (s =~ re) of
        [Left _] -> regexFailed re
        xs -> return $  xs
-}

extract s (x,y) = take y (drop x s)

instance RegexContext x [x] where
    s =~ re = case s =~~ re of 
        Nothing -> [] 
        Just z -> mrMatch z
    s =~~ re = liftM mrMatch (s =~~ re)

-- useful in non-monad context
instance RegexContext x Bool where 
    s =~ re = matchTest re s
    s =~~ re = case s =~ re of
        False -> regexFailed re
        True -> return True
-- useful in monad context        
instance RegexContext x () where  
    _ =~ _ = ()
    s =~~ re = case s =~ re of
        False -> regexFailed re
        True -> return ()

instance RegexContext x [[x]] where
    s =~ re =  [extract s (a!0) | a <- matchAll re s]
    s =~~ re = case (s =~ re) of
        [] -> regexFailed re
        xs -> return  xs

instance RegexContext x [Array Int [x]] where 
    s =~ re =  [fmap (extract s) x | x <- matchAll re s]
    s =~~ re = case (s =~ re) of
        [] -> regexFailed re
        xs -> return  xs
{-
instance RegexContext x [Array Int (Int,Int)] where 
    s =~ re =  matchAll re s
    s =~~ re = case (s =~ re) of
        [] -> regexFailed re
        xs -> return  xs
-}
instance RegexContext x [Array Int ([x],(Int,Int))] where 
    s =~ re =  [fmap (\z -> (extract s z,z)) x | x <- matchAll re s]
    s =~~ re = case (s =~ re) of
        [] -> regexFailed re
        xs -> return  xs

instance RegexContext x (Array Int [x]) where 
    s =~ re = maybe (listArray (1,0) []) id (s =~~ re)
    s =~~ re = case s =~~ re of 
        Nothing -> regexFailed re
        Just z -> return $ mrSubs z


instance RegexLike String Char where
    matchOnce re xs bol = matchOnce (c re) xs bol where
        c s = unsafePerformIO $ 
            PCRE.compile s 0 >>= \x -> case x of
                Left (i,err) -> fail $ "PCRE Regular Expression Error:\n" ++ re ++ "\n" ++ replicate i ' ' ++ "^ " ++ err 
                Right p -> return p
    matchShow s = s

{-
instance RegexLike Regex Char where 
    matchOnce re xs = fmap f (matchRegexAll re xs) where
        f (x,y,z,ls) = MR { mrBefore = x,mrMatch = y ,mrAfter = z,mrSubs = listArray (1,length ls) ls}
    matchShow _ = "Regex"
    
instance RegexLike String Char where 
    --matchOnce re xs = fmap f (matchRegexAll (mr re) xs) where
    --    f (x,y,z,ls) = (x,y,z,listArray (1,length ls) ls)
    matchOnce re xs = fmap f (matchRegexAll (mr re) xs) where
        f (x,y,z,ls) = MR { mrBefore = x,mrMatch = y ,mrAfter = z,mrSubs = listArray (1,length ls) ls}
        mr ('i':'/':re) = mkRegexWithOpts re True False
        mr ('s':'/':re) = mkRegexWithOpts re False True
        mr ('i':'s':'/':re)  = mkRegexWithOpts re False False
        mr ('s':'i':'/':re)  = mkRegexWithOpts re False False
        mr ('/':re) = mkRegex re 
        mr (re) = mkRegex re 
    matchShow re = re
-}

