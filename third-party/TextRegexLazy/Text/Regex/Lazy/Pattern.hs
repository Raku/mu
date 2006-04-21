-- | This 'Text.Regex.Lazy.Pattern' module provides the 'Pattern' data
-- type and its subtypes.  This 'Pattern' type is used to represent
-- the parsed form of a Regular Expression.  
--
-- It is possible to construct values of 'Pattern' that are invalid
-- regular expressions.
module Text.Regex.Lazy.Pattern 
    (Pattern(..)
    ,PatternSet(..)
    ,PatternSetCharacterClass(..),PatternSetCollatingElement(..),PatternSetEquivalenceClass(..)
    ,PatternIndex
    ,showPattern
    ,foldPattern,dfsPattern
    ,simplify
    ,backReferences,dfaClean
    ,alwaysOnlyMatchNull,cannotMatchNull
    ,hasFrontCarat,hasBackDollar) where

{- By Chris Kuklewicz, 2006. BSD License, see the LICENSE file. -}

import Data.List(intersperse,partition)
import qualified Data.Set as Set
import Data.Set(Set)

data Pattern = PEmpty | PCarat | PDollar
             | PGroup  PatternIndex Pattern
             | POr     [Pattern]
             | PConcat [Pattern]
             | PQuest  Pattern
             | PPlus   Pattern
             | PStar   Pattern
             | PBound  Int (Maybe Int) Pattern
             -- | PLazy indicates the pattern should find the shortest match first
             | PLazy   Pattern    -- non-greedy wrapper (for ?+*{} followed by ?)
             -- | PPossessive indicates the pattern can only find the longest match
             | PPossessive Pattern -- possessive modifier (for ?+*{} followed by +)
             | PDot               -- Any character (newline?) at all
             | PAny    PatternSet -- Square bracketed things
             | PAnyNot PatternSet -- Inverted square bracketed things
             | PEscape Char       -- Backslashed Character
             | PBack   PatternIndex -- Backslashed digits as natural number
             | PChar   Char       -- Specific Character
               -- After simplify / mergeCharToString, adjacent PChar are merge'd into PString
             | PString String
               deriving (Eq,Show)

showPattern :: Pattern -> String
showPattern p =
  case p of
    PEmpty -> "()"
    PCarat -> "^"
    PDollar -> "$"
    PGroup _ p -> ('(':showPattern p)++")"
    POr ps -> concat $ intersperse "|" (map showPattern ps)
    PConcat ps -> concatMap showPattern ps
    PQuest p -> (showPattern p)++"?"
    PPlus p -> (showPattern p)++"+"
    PStar p -> (showPattern p)++"*"
    PLazy p | isPostAtom p -> (showPattern p)++"?"
            | otherwise -> "<Cannot print PLazy of "++show p++">"
    PPossessive p | isPostAtom p -> (showPattern p)++"+"
                  | otherwise -> "<Cannot print PPossessive of "++show p++">"
    PBound i (Just j) p | i==j -> showPattern p ++ ('{':show i)++"}"
    PBound i mj p -> showPattern p ++ ('{':show i) ++ maybe ",}" (\j -> ',':show j++"}") mj
    PDot -> "."
    PAny (PatternSet s scc sce sec) ->
        let (special,normal) = maybe ("","") ((partition (`elem` "]-")) . Set.toAscList) s
            charSpec = (if ']' `elem` special then (']':) else id) (byRange normal)
            scc' = maybe "" ((concatMap (\s -> "[:"++unSCC s++":]")) . Set.toList) scc
            sce' = maybe "" ((concatMap (\s -> "[."++unSCE s++".]")) . Set.toList) sce
            sec' = maybe "" ((concatMap (\s -> "[="++unSEC s++"=]")) . Set.toList) sec
        in concat ['[':charSpec,scc',sce',sec',if '-' `elem` special then "-]" else "]"]
    PAnyNot (PatternSet s scc sce sec) ->
        let (special,normal) = maybe ("","") ((partition (`elem` "]-")) . Set.toAscList) s
            charSpec = (if ']' `elem` special then (']':) else id) (byRange normal)
            scc' = maybe "" ((concatMap (\s -> "[:"++unSCC s++":]")) . Set.toList) scc
            sce' = maybe "" ((concatMap (\s -> "[."++unSCE s++".]")) . Set.toList) sce
            sec' = maybe "" ((concatMap (\s -> "[="++unSEC s++"=]")) . Set.toList) sec
        in concat ["[^",charSpec,scc',sce',sec',if '-' `elem` special then "-]" else "]"]
    PEscape c -> '\\':c:[]
    PBack i -> '\\':(show i)
    PChar c -> [c]
    PString s -> s
  where byRange all@(x:xs) | length all <=3 = all
                           | otherwise = groupRange x 1 xs
        groupRange x n (y:ys) = if (fromEnum y)-(fromEnum x) == n then groupRange x (succ n) ys
                                else (if n <=3 then take n [x..]
                                      else x:'-':(toEnum (pred n+fromEnum x)):[]) ++ groupRange y 1 ys
        groupRange x n [] = if n <=3 then take n [x..]
                            else x:'-':(toEnum (pred n+fromEnum x)):[]


data PatternSet = PatternSet (Maybe (Set Char))  (Maybe (Set (PatternSetCharacterClass)))
   (Maybe (Set PatternSetCollatingElement)) (Maybe (Set PatternSetEquivalenceClass)) deriving (Eq,Show)

newtype PatternSetCharacterClass   = PatternSetCharacterClass   {unSCC::String} deriving (Eq,Ord,Show) -- [: :]
newtype PatternSetCollatingElement = PatternSetCollatingElement {unSCE::String} deriving (Eq,Ord,Show) -- [. .]
newtype PatternSetEquivalenceClass = PatternSetEquivalenceClass {unSEC::String} deriving (Eq,Ord,Show) -- [= =]

-- | PatternIndex is for indexing submatches from  parenthesized groups (PGroup)
type PatternIndex = Int

-- helper function
isPostAtom p = case p of
                 PQuest _ -> True
                 PPlus _ -> True
                 PStar _ -> True
                 PBound _ _ _ -> True
                 _ -> False
       
-- -- -- Transformations on Pattern

simplify = dfsPattern simplify'

foldPattern :: (Pattern -> a -> a) -> a -> Pattern -> a
foldPattern f = foldP
  where foldP a pIn = let unary p = f pIn (f p a) in
          case pIn of
            POr ps -> f pIn (foldr f a ps)
            PConcat ps -> f pIn (foldr f a ps)
            PGroup i p -> unary p
            PQuest p -> unary p
            PPlus p -> unary p
            PStar p -> unary p
            PLazy p -> unary p
            PPossessive p -> unary p
            PBound i mi p -> unary p
            _ -> f pIn a

-- | Apply a Pattern transfomation function depth first
dfsPattern :: (Pattern -> Pattern)  -- ^ The transformation function
           -> Pattern               -- ^ The Pattern to transform
           -> Pattern               -- ^ The transformed Pattern
dfsPattern f = dfs
 where unary c = f . c . dfs
       dfs pattern = case pattern of
                       PGroup i p -> unary (PGroup i) p
                       POr ps -> f (POr (map dfs ps))
                       PConcat ps -> f (PConcat (map dfs ps))
                       PQuest p -> unary PQuest p
                       PPlus p -> unary PPlus p
                       PStar p -> unary PStar p
                       PLazy p -> unary PLazy p
                       PPossessive p -> unary PPossessive p
                       PBound i mi p -> unary (PBound i mi) p
                       _ -> f pattern


-- | Function to flatten nested POr or nested PConcat applicataions.
-- Other patterns are returned unchanged
flatten :: Pattern -> [Pattern]
flatten (POr ps) = (concatMap (\x -> case x of
                                       POr ps' -> ps'
                                       p -> [p]) ps)

flatten (PConcat ps) = (concatMap (\x -> case x of
                                           PConcat ps' -> ps'
                                           p -> [p]) ps)
flatten other = []

-- | Function to transform a pattern into an equivalent, but less
-- redundant form
simplify' x@(POr _) = 
  let ps' = flatten x
  in case ps' of
       [] -> PEmpty
       [p] -> p
       _ -> POr ps'

simplify' x@(PConcat _) =
  let ps'' = mergeCharToString (filter notPEmpty (flatten x))
      notPEmpty PEmpty = False
      notPEmpty _ = True
      mergeCharToString :: [Pattern] -> [Pattern]
      mergeCharToString ps = merge ps
        where merge ((PChar c1):(PChar c2):xs) = merge $ (PString [c1,c2]):xs
              merge ((PString s):(PChar c):xs) = merge $ (PString (s++[c])):xs
              merge ((PString s1):(PString s2):xs) = merge $ (PString (s1++s2)):xs
              merge ((PChar c):(PString s):xs) = merge $ (PString (c:s)):xs
              merge (x:xs) = x:merge xs
              merge [] = []
  in case ps'' of
       [] -> PEmpty
       [p] -> p
       _ -> PConcat ps''

simplify' (PBound _ (Just 0) _) = PEmpty

simplify' other = other

-- -- Analyze Pattern

-- | This provides an unordered list of the PatternIndex values that
-- have back references in the pattern.  This does not mean the
-- pattern will have these captured substrings, just that the pattern
-- referes to these indices.
backReferences :: Pattern -> [PatternIndex]
backReferences = foldPattern f []
  where f (PBack x) xs = (x:xs)
        f _ xs = xs

-- | Determines if pIn will always accept "" and never accept any characters
-- Treat PCarat and PDollar as False, since they do not always accept ""
-- Trest PBack as False since the capture may not always be ""
alwaysOnlyMatchNull :: Pattern -> Bool
alwaysOnlyMatchNull pIn =
  case pIn of
    PEmpty -> True
    PCarat -> False
    PDollar -> False
    PGroup _ p -> alwaysOnlyMatchNull p
    POr [] -> True
    POr ps -> all alwaysOnlyMatchNull ps
    PConcat [] -> True
    PConcat ps -> all alwaysOnlyMatchNull ps
    PLazy p -> alwaysOnlyMatchNull p
    PPossessive p -> alwaysOnlyMatchNull p
    PQuest p -> alwaysOnlyMatchNull p
    PPlus p -> alwaysOnlyMatchNull p
    PStar p -> alwaysOnlyMatchNull p
    PBound _ (Just 0) _ -> True
    PBound _ _ p -> alwaysOnlyMatchNull p
    PBack _ -> False
    _ ->False

-- | If 'cannotMatchNull' returns 'True' then it is known that the
-- 'Pattern' will never accept an empty string.  If 'cannotMatchNull'
-- returns 'False' then it is possible but not definite that the
-- 'Pattern' could accept an empty string.  'PBack' is 'False' since
-- it may sometimes be "".
cannotMatchNull :: Pattern -> Bool
cannotMatchNull pIn =
  case pIn of
    PEmpty -> False
    PCarat -> False
    PDollar -> False
    PGroup _ p -> cannotMatchNull p
    POr [] -> False
    POr ps -> all cannotMatchNull ps
    PConcat [] -> False
    PConcat ps -> any cannotMatchNull ps
    PLazy p -> cannotMatchNull p
    PPossessive p -> cannotMatchNull p
    PQuest _ -> False
    PPlus p -> cannotMatchNull p
    PStar _ -> False
    PBound 0 _ _ -> False
    PBound _ _ p -> cannotMatchNull p
    PBack _ -> False
    _ -> True

-- | Determines if pIn is always anchored at the front with PCarat
hasFrontCarat pIn =
  case pIn of
    PCarat -> True
    POr [] -> False
    POr ps -> all hasFrontCarat ps
    PConcat [] -> False
    PConcat ps -> case dropWhile alwaysOnlyMatchNull ps of
                    [] -> False
                    (p:_) -> hasFrontCarat p
    _ -> False

-- | Determines if pIn is always anchored at then back with PDollar
hasBackDollar pIn =
  case pIn of
    PDollar -> True
    POr [] -> False
    POr ps -> all hasBackDollar ps
    PConcat [] -> False
    PConcat ps -> case dropWhile alwaysOnlyMatchNull (reverse ps) of
                    [] -> False
                    (p:_) -> hasBackDollar p
    _ -> False

-- | Determines if the pattern has no lazy modifiers, possessive
-- modifiers, carats, dollars, or back references.
-- 
-- If so, then the pattern could be tranformed into a simple DFA.
dfaClean pIn = foldPattern f True pIn
  where f p b = b && case p of
                       PLazy _ -> False
                       PPossessive _ -> False
                       PCarat -> False
                       PDollar -> False
                       PBack _ -> False
                       _ -> True
