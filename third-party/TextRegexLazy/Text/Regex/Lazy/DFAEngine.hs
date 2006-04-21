{-
By Chris Kuklewicz (haskell (at) list (dot) mightyreason (dot) com), 2006.

This file is licensed under the LGPL (version 2, see the LICENSE
file), because it is a derivative work.  This DFAEngine takes the lazy
transition table from Manuel Chakravarty's lexer in CTK Light, but
uses it for simpler purposes.  The original CTK code can be found here
http://www.cse.unsw.edu.au/~chak/haskell/ctk/

Don Stewart (http://www.cse.unsw.edu.au/~dons/contact.html) also
contributed to this code.

I want the freedom to alter the types a bit, so this is a separate
module.

The CTK / DFA code can be thought of as three partsL

1) The ability to compose Regexp combinators which will lazily
   assemble a DFA.  This is mainly bound up in the Cont type and the
   internal functions that merge it (exported as >||<).

2) The interface of how to specify "Failure" and "Success".  This was
   bound up in LexAction holding an function and is now
   lexAccept/lesFailure/lexContinue.

3) The traversal engine.  At each longer and longer match the last
   seen match is updated.  Different traversals keep track of
   different levels of detail.

As a descendent of the regex-dna entry at
http://shootout.alioth.debian.org/gp4/benchmark.php?test=regexdna&lang=all
, this module has contributions from Don Stewart, Alson Kemp, and
Chris Kuklewicz.

-}

module Text.Regex.Lazy.DFAEngine 
    (Lexer(..),LexAction(..),Regexp,Cont(Done)
    ,emptyOp,char,alt,altNot,allChar,string
    ,(>|<),orRE,(+>),concatRE,quest,star,plus
    ,failure,accept,(>||<)
    ,findRegex,matchesHere,countRegex,findRegexS
    ,peek,inBounds,lexFailure,lexContinue,lexAccept
    ) where

import Data.Array (Array, (!), assocs, accumArray)
import qualified Data.Map as M
import Data.Map(Map)
import Data.List(sort)

--------------------------------------------------------------------------------
-- * Types and supporting functions for the DFA (taken form CTK Light)
--------------------------------------------------------------------------------

-- | a regular expression
type Regexp = Lexer -> Lexer

-- | tree structure used to represent the lexer table
--
-- each node in the tree corresponds to a of the lexer; the
-- associated actions are those that apply when the corresponding
-- is reached
data Lexer = Lexer !LexAction Cont deriving (Show)

failure :: Lexer
failure = Lexer lexFailure Done

-- | This is interface between the DFA table and the traversal engine,
-- and is simpler than the original CTK version.
newtype LexAction = LexAction Int deriving (Eq,Ord,Show)
lexFailure  = LexAction (-1)
lexContinue = LexAction 0
lexAccept   = LexAction 1
-- The above allows the definition joinActions = max

-- | 'Done' or a table-like-thing to associate the next character with a Lexer
data Cont = Dense {bounds :: !BoundsNum
                  ,transArray :: Array Char (Lexer)
                  ,otherTrans :: Lexer}
          | Sparse {bounds :: !BoundsNum
                   ,transMap :: Map Char (Lexer)
                   ,otherTrans :: Lexer}
          | Done deriving (Show)

-- | estimates the number of (non-'otherTrans') elements and the
-- bounds of a DFA transition table
data BoundsNum = B !Int !Char !Char deriving (Show)

--------------------------------------------------------------------------------
-- * Regexp matching functions and combinators (taken from CTK Light)
--------------------------------------------------------------------------------

----------------------------------------
-- | Fixity declarations
----------------------------------------

infixr 4 `quest`, `star`, `plus`
infixl 3 +>
infixl 2 >|<, >||<

----------------------------------------
-- | These create Regexp
----------------------------------------

-- | Empty lexeme (noop)
emptyOp :: Regexp
{-# INLINE emptyOp #-}
emptyOp = id

-- | One character regexp
char :: Char -> Regexp
char c = \l -> Lexer lexContinue (Sparse (B 1 c c) (M.singleton c l) failure)

-- | accepts a list of alternative characters
-- Equiv. to `(foldr1 (>|<) . map char) cs', but much faster
alt :: [Char] -> Regexp
alt [] = \l -> failure
alt cs = let scs = sort cs
             (len,end) = lengthAndLast scs
             bnds = B len (head scs) end
         in \l -> Lexer lexContinue (aggregateConts bnds [(c, l) | c <- sort cs] failure)

-- | accepts an inverted list of alternative characters
-- Equiv. to `(foldr1 (>|<) . map char) cs', but much faster
altNot :: [Char] -> Regexp
altNot [] = allChar
altNot cs = let scs = sort cs
                (len,end) = lengthAndLast scs
                bnds = B len (head scs) end
            in \l -> Lexer lexContinue (aggregateConts bnds [(c, failure) | c <- sort cs] l)

-- | accepts any character
allChar :: Regexp
allChar = let bnds = B 0 maxBound minBound
          in \l -> Lexer lexContinue (Sparse bnds M.empty l)

-- | accept a character sequence
string :: String -> Regexp
string "" = emptyOp
string cs = (foldr1 (+>) . map char) cs


-- Helper function for alt and altNot to efficiently compute the bounds
lengthAndLast :: [a] -> (Int,a)
lengthAndLast = helper 1
  where helper _ [] = (0,undefined)
        helper i [x] = (i,x)
        helper i (x:xs) = let i' = succ i in seq i' $ helper i' xs

----------------------------------------
-- | These combine two Regexp's
----------------------------------------

-- | Concatenation of regexps is just concatenation of functions
-- x +> y corresponds to xy
(+>) :: Regexp -> Regexp -> Regexp
{-# INLINE (+>) #-}
(+>)  = (.) 

concatRE :: [Regexp] -> Regexp
{-# INLINE concatRE #-}
concatRE [] = emptyOp
concatRE rs = foldr1 (+>) rs

-- | disjunctive combination of two regexps, corresponding to x|y.
-- 
-- This will find the longest match
(>|<) :: Regexp -> Regexp -> Regexp
{-# INLINE (>|<) #-}
re1 >|< re2  = \l -> re1 l >||< re2 l

orRE :: [Regexp] -> Regexp
{-# INLINE orRE #-}
orRE [] = emptyOp
orRE rs = foldl1 (>|<) rs

-- | x `quest` y corresponds to the regular expression x?y
quest :: Regexp -> Regexp -> Regexp
{-# INLINE quest #-}
quest re1 re2  = (re1 +> re2) >|< re2

-- | x `plus` y corresponds to the regular expression x+y
plus :: Regexp -> Regexp -> Regexp
{-# INLINE plus #-}
plus re1 re2  = re1 +> (re1 `star` re2)

--
-- The definition used below can be obtained by equational reasoning from this
-- one (which is much easier to understand): 
--
--   star re1 re2 = let self = (re1 +> self >|< emptyOp) in self +> re2
--
-- However, in the above, `self' is of type `Regexp s t' (ie, a functional),
-- whereas below it is of type `Lexer s t'.  Thus, below we have a graphical
-- body (finite representation of an infinite structure), which doesn't grow
-- with the size of the accepted lexeme - in contrast to the definition using
-- the functional recursion.

-- | x `star` y corresponds to the regular expression x*y
star :: Regexp -> Regexp -> Regexp
star re1 re2  = \l -> let self = re1 self >||< re2 l in self

--------------------------------------------------------------------------------
-- * Converting Regexp into Lexer and combinind lexers
--------------------------------------------------------------------------------

-- | Have a match to Regexp be consider a success
accept :: Regexp -> Lexer
{-# INLINE accept #-}
accept re = re (Lexer lexAccept Done)

-- | disjunctive combination of two lexers (longest match, right biased)
(>||<) :: Lexer -> Lexer -> Lexer
(Lexer a c) >||< (Lexer a' c')  = Lexer (joinActions a a') (joinConts c c')

-- internal
joinActions :: LexAction -> LexAction -> LexAction
joinActions = max

-- internal, combine two disjunctive continuations
joinConts :: Cont -> Cont -> Cont
joinConts Done c'   = c'
joinConts c    Done = c
joinConts c    c'   = let (bn , cls , other ) = listify c
                          (bn', cls', other') = listify c'
                      -- note: `addsBoundsNum' can, at this point, only
                      --       approx. the number of *non-overlapping* cases;
                      --       however, the bounds are correct 
                      in aggregateConts (addBoundsNum bn bn') (fuse cls other cls' other') (other >||< other')
  where -- listify converts the array or map into an ascending list
        listify :: Cont -> (BoundsNum,[(Char,Lexer)],Lexer)
        listify (Dense  n arr other) = (n, assocs arr, other)
        listify (Sparse n cls other) = (n, M.toAscList cls, other)

-- Combine two ascending lists with defaults into a new ascending list
fuse :: [(Char,Lexer)] -> (Lexer) -> [(Char,Lexer)] -> (Lexer) -> [(Char, Lexer)]
{-# INLINE fuse #-}
fuse [] xo y  _  = map (\(yc,ya) -> (yc,xo >||< ya)) y
fuse x  _  [] yo = map (\(xc,xa) -> (xc,xa >||< yo)) x
fuse x@((xc,xa):xs) xo y@((yc,ya):ys) yo = 
    case compare xc yc of
      LT -> (xc,xa >||< yo) : fuse xs xo y  yo
      EQ -> (xc,xa >||< ya) : fuse xs xo ys yo
      GT -> (yc,xo >||< ya) : fuse x  xo ys yo

-- Take a new BoundsNum, a new ascending list, and new default
aggregateConts :: BoundsNum -> [(Char, Lexer)] -> Lexer -> Cont
{-# INLINE aggregateConts #-}
aggregateConts bn@(B n lc hc) cls other
  | n >= denseMin = Dense  bn (accumArray (\old new -> new) other (lc, hc) cls) other
  | otherwise     = Sparse bn (M.fromAscList cls) other

-- we use the dense representation if a table has an upper bound of at
-- least this number of (non-error) elements
denseMin :: Int
denseMin = 20

-- combine two bounds.  Note that n,n',newN are upper bounds on the
-- number of characters.
addBoundsNum :: BoundsNum -> BoundsNum -> BoundsNum
{-# INLINE addBoundsNum #-}
addBoundsNum (B 0 _ _ ) b = b
addBoundsNum b (B 0 _ _ ) = b
addBoundsNum (B n lc hc) (B n' lc' hc')  = let newLc = min lc lc'
                                               newHc = max hc hc'
                                               newN = min (n + n') (fromEnum newHc - fromEnum newLc + 1)
                                           in B newN newLc newHc

--------------------------------------------------------------------------------
-- * Matching engine
--------------------------------------------------------------------------------

-- | This is the ultra-lazy matching engine.  It returns the longest match.
--
-- This will not examine any more of the input than needed, checking
-- and returning a character at a time. Once a character is read that
-- leads to no possibility of future match it does not evaluate any
-- deeper.
--
-- When a match is found, the input past match is not examined at all.
--
-- In the extreme case of the input string being (error _) this will
-- still succeed if the Regexp matches only an empty string since the
-- input will not be demanded at all.  The "input before matching" in
-- this case will be [] and its length is 0, and the length of the
-- match is 0, which the input at start of match and the input past
-- the match will both be (error _).
--
-- This loops over 'matchHere' to find the first match
findRegex :: Regexp            -- ^ The regular expression to match
          -> String            -- ^ The input string to scan along, looking for a match
          -> (String           -- ^ The input string before the match
             ,Int              -- ^ The length of the string before the match
             ,Maybe            -- ^ Nothing if there was no match
              (String          -- ^ The input string at the start of the match
              ,Int             -- ^ The length of the match
              ,String))        -- ^ The input string starting just past the match
findRegex re input = 
  let lexer = accept re
      loop :: String -> Int -> (String,Int,Maybe(String,Int,String))
      loop s i = case matchHere lexer s of
                   ((-1),_) -> let ~(~rest,~len,~result) = loop (tail s) $! (succ i)
                               in if null s then ([],i,Nothing)
                                            else (head s : rest,len,result)
                   (n,~leftover) -> ([],i,Just (s,n,leftover))
  in loop input 0


-- | This returns (-1,[]) if there was no match
matchHere :: Lexer      -- ^ (accept regexp) to match
          -> String     -- ^ The input string
          -> (Int       -- ^ The length 'n' of the prefix of input that matched (take n input)
             , String)  -- ^ The input starting past the match (drop n input)
{-# INLINE matchHere #-}
matchHere l s = applyHere l s ((-1),[]) 0

-- internal
applyHere :: Lexer -> String -> (Int,String) -> Int -> (Int,String)
{-# INLINE applyHere #-}
applyHere (Lexer action cont) input last here | here `seq` True =
  let last' = if action == lexAccept then (here,input) else last
  in case seq last' cont of
       Done -> last'
       _ -> case input of
              [] -> last'
              _ -> let lexer'@(Lexer action' _) = peek cont (head input)
                   in if action' == lexFailure then last'
                      else applyHere lexer' (tail input) last' (succ here)

-- Do the lookup of the current character in the DFA transition table.
peek :: Cont -> Char -> Lexer
{-# INLINE peek #-}
peek (Dense bn arr other)  c | c `inBounds` bn = arr ! c
                             | otherwise = other
peek (Sparse bn cls other) c | c `inBounds` bn = M.findWithDefault other c cls
                             | otherwise = other
-- check whether a character is in the bounds
inBounds :: Char -> BoundsNum -> Bool
{-# INLINE inBounds #-}
inBounds c (B 0 _  _ ) = False
inBounds c (B _ lc hc) = c >= lc && c <= hc

-- | This counts the number of matches to regex in the string, (it
-- checks each possible starting position).  This should be the same
-- as ((length (splitRegex re input))-1) but more efficient
countRegex :: (Lexer -> Lexer) -> [Char] -> Int
countRegex re input =
  let lexer = accept re
      loop s i | seq i True = if matchesHere lexer s
                                then if null s then succ i else loop (tail s) (succ i)
                                else if null s then i else loop (tail s) i
  in loop input 0

-- | This searches the input string for a match to the regex
matchesRegex :: Regexp -> [Char] -> Bool
matchesRegex re input =
  let lexer = accept re
      loop s = if matchesHere lexer input then True
               else loop (tail s) 
  in loop input

-- | This checks for a match to the regex starting at the beginning of the input
matchesHere :: Lexer -> [Char] -> Bool
matchesHere (Lexer action cont) input =
  if action == lexAccept then True
  else case cont of
         Done -> False
         _ -> if null input then False
              else let lexer'@(Lexer action' _) = peek cont (head input)
                   in if action' == lexFailure then False
                      else matchesHere lexer' (tail input)

-- | This is a version of findRegex that does not compute the length of the prefix
findRegexS :: (Lexer -> Lexer) -> String -> (String, Maybe (String, Int, String))
findRegexS re input = 
  let lexer = accept re
      loop :: String -> (String,Maybe(String,Int,String))
      loop s = case matchHere lexer s of
                   ((-1),_) -> let ~(~rest,~result) = loop (tail s)
                               in if null s then ([],Nothing)
                                            else (head s : rest,result)
                   (n,~leftover) -> ([],Just (s,n,leftover))
  in loop input

--------------------------------------------------------------------------------
-- * Simple tests of lazyness
--------------------------------------------------------------------------------

testS = "This will work"

testre = string testS

test s = findRegex testre s
test1 s = let (f,_,_) = findRegex testre s in f
test2 s = case findRegex testre s of
            (_,_,Just (sk,nk,_)) -> nk
            _ -> (-1)
testE s = seq (findRegex testre s) "No Error"

empty = ""
err = error "X"
unmatching k = take k (cycle ['0'..'9'])
matching k = take k (cycle testS)
unErr k = unmatching k ++ error "X"
matErr k = matching k ++ error "X"

-- These should all succeed and print out without an error

tests0 = case findRegex emptyOp err of
           (pk,ik,Just (_,nk,_)) -> (pk,ik,nk) -- others are (error "X")

tests1 = [ (test $ unmatching i,take i $ test1 $ unErr i, test $ matching i) | i <- [1..3]]

tests2 = concat [ [ test $ matching i
                  , test $ unmatching 5 ++ matching i
                  , test $ matching i ++ unmatching 5
                  , test $ unmatching 5 ++ matching i ++ unmatching 5 ] | i <- [13,14,15] ]
tests3 = [ test2 $ matErr 14 
        , test2 $ unmatching 5 ++ matErr 14
        , test2 $ matching 14 ++ unErr 5
        , test2 $ unmatching 5 ++ matching 14 ++ unErr 5 ]
