-- | 'ReadRegex' parses string regular expressions with all the
-- extensions : lazy and possessive modifiers and back-references.
--
-- "man re_format" on OS X 10.4.4 was the guide for most of the format
-- (the new extended format, not the old basic format).  Currently,
-- only word-boundaries are not recognized and handled.
--
-- 
module Text.Regex.Lazy.ReadRegex
  (PatternIndex
  ,parseRegex
  ,legalCharacterClasses
  ,decodePatternSet
  ,p_regex) where

import Text.Regex.Lazy.Pattern
import Text.Regex.Lazy.Common(StringRegex)
import Control.Monad(liftM,when,guard)
import Text.ParserCombinators.Parsec
import Data.List(intersperse,partition)
import Data.Set(Set)
import qualified Data.Set as Set

-- BracketElement is internal only
data BracketElement = BEChar Char | BEChars String | BEColl String | BEEquiv String | BEClass String

-- | This runs the p_regex parser on the input string (with initial
-- state 0), and returns (Left ParseError) on failure or Right
-- (Pattern,PatternIndex) where the PatternIndex is the number of
-- parenthesized subgroups in the Pattern (i.e. the final state of the
-- parser).  If the entire input is not consumed then 'parseRegex'
-- returns a parse error.  If you need to parse a regular expression
-- that is not the entire input string then you will need to utilize
-- p_regex.
parseRegex :: StringRegex -> Either ParseError (Pattern,PatternIndex)
parseRegex x = runParser (do pat <- p_regex
                             eof
                             subs <- getState
                             return (pat,subs)) 0 x x

-- | This is the Parsec parser that performs the interpretation. The
-- state is a PatternIndex which is incremented and read (in that
-- order) to assign a unique 'PatternIndex' to each parenthesized
-- group.  It does not check that the regex is followed by the end of
-- the input.
p_regex :: GenParser Char PatternIndex Pattern
p_regex = liftM POr $ sepBy1 p_branch (char '|')

-- man re_format helps alot, it says one-or-more pieces so this uses
-- many1 instead of many.  Use "()" to indicate an empty piece.
p_branch = liftM PConcat $ many1 p_piece

p_piece = (p_anchor <|> p_atom) >>= p_post_atom

p_anchor = (char '^' >> return PCarat)
       <|> (char '$' >> return PDollar)
       <?> "empty () or anchor ^ or $"

p_atom =  p_empty <|> p_group <|> p_bracket <|> p_char <?> "an atom"

p_empty = try (do string "()" 
                  index <- updateState (+1) >> getState
                  return $ PGroup index PEmpty)

p_group = lookAhead (char '(') >> do
  index <- updateState (+1) >> getState
  liftM (PGroup index) $ between (char '(') (char ')') p_regex

-- p_post_atom takes the previous atom as a parameter
p_post_atom atom = ( ( (char '?' >> return (PQuest atom))
                   <|> (char '+' >> return (PPlus  atom))
                   <|> (char '*' >> return (PStar  atom))
                   <|> p_bound atom) >>= p_post_atom_ng) <|> (return atom)

p_bound atom = try $ between (char '{') (char '}') (p_bound_spec atom)

p_bound_spec atom = do lowS <- many1 digit
                       let lowI = read lowS
                       highMI <- option (Just lowI) $ try $ do 
                                   char ','
                                   highS <- many digit
                                   if null highS then return Nothing -- no upper bound
                                     else do let highI = read highS
                                             guard (lowI <= highI)
                                             return (Just (read highS))
                       return (PBound lowI highMI atom)

-- Look for another '?' to make into a Lazy matcher (non-greedy)
p_post_atom_ng atom = ((char '?' <?> "lazy ? modifier") >> return (PLazy atom))
                  <|> ((char '+' <?> "possessive + modifier") >> return (PPossessive atom))
                  <|> return atom

p_char = p_dot <|> p_left_brace <|> p_escaped <|> p_other_char
    where p_dot = char '.' >> return PDot
          p_left_brace = try $ (char '{' >> notFollowedBy digit >> return (PChar '{'))
          p_escaped = char '\\' >> (liftM (PBack . read) (many1 digit) <|> liftM PEscape anyChar)
          p_other_char = liftM PChar (noneOf specials)
          specials  = "^.[$()|*+?{\\"

-- parse [bar] and [^bar] sets of characters
p_bracket = (char '[') >> ( (char '^' >> p_set True) <|> (p_set False) )

-- p_set does not support [.ch.] or [=y=] or [:foo:]
p_set :: Bool -> GenParser Char st Pattern
p_set invert = do initial <- (option "" ((char ']' >> return "]") <|> (char '-' >> return "-")))
                  values <- many1 p_set_elem
                  char ']'
                  let chars = maybe'set $ initial ++ [c | BEChar c <- values ] ++ concat [s | BEChars s <- values ] 
                      colls = maybe'set [PatternSetCollatingElement coll | BEColl coll <- values ]
                      equivs = maybe'set [PatternSetEquivalenceClass equiv | BEEquiv equiv <- values]
                      class's = maybe'set [PatternSetCharacterClass a'class | BEClass a'class <- values]
                      maybe'set x = if null x then Nothing else Just (Set.fromList x)
                      sets = PatternSet chars class's colls equivs
                  sets `seq` return $ if invert then PAnyNot sets else PAny sets

p_set_elem = p_set_elem_class <|> p_set_elem_equiv <|> p_set_elem_coll
         <|> p_set_elem_range <|> p_set_elem_char <?> "Failed to parse bracketed string"

p_set_elem_class = liftM BEClass $
  try (between (string "[:") (string ":]") (many1 $ noneOf ":]"))

p_set_elem_equiv = liftM BEEquiv $
  try (between (string "[=") (string "=]") (many1 $ noneOf "=]"))

p_set_elem_coll =  liftM BEColl $
  try (between (string "[.") (string ".]") (many1 $ noneOf ".]"))

p_set_elem_range = try $ do 
  start <- noneOf "]-"
  char '-'
  end <- noneOf "]"
  return (BEChars [start..end])

p_set_elem_char = do 
  c <- noneOf "]"
  when (c == '-') $ do
    atEnd <- (lookAhead (char ']') >> return True) <|> (return False)
    when (not atEnd) (unexpected "A dash is in the wrong place in a bracket")
  return (BEChar c)

-- | This takes a 'PatternSet' and returns 'Set' 'Char'.  The
-- character classes in legalCharacterClasses are expanded to their
-- members.
--
-- 'decodePatternSet' ignores collating element and treats equivalence
-- classes as just their defining character(s).
decodePatternSet (PatternSet msc mscc _ msec) =
  let baseMSC = maybe Set.empty id msc
      withMSCC = foldl (flip Set.insert) baseMSC  (maybe [] (concatMap decodeCharacterClass . Set.toList) mscc)
      withMSEC = foldl (flip Set.insert) withMSCC (maybe [] (concatMap unSEC . Set.toList) msec)
  in withMSEC

-- | This is the list of [:foo:] character classes that are recognized
-- by decodePatternSet.  This is the whole list including the
-- "word" extension which is "alnum" and the underscore '_' character.
--
-- The NUL character is included as part of the "cntrl" character class.
legalCharacterClasses = ["alnum","digit","punct","alpha","graph"
  ,"space","blank","lower","upper","cntrl","print","xdigit","word"]

decodeCharacterClass (PatternSetCharacterClass s) =
  case s of
    "alnum" -> ['0'..'9']++['a'..'z']++['A'..'Z']
    "digit" -> ['0'..'9']
    "punct" -> ['\33'..'\47']++['\58'..'\64']++['\91'..'\95']++"\96"++['\123'..'\126']
    "alpha" -> ['a'..'z']++['A'..'Z']
    "graph" -> ['\41'..'\126']
    "space" -> "\t\n\v\f\r "
    "blank" -> "\t "
    "lower" -> ['a'..'z']
    "upper" -> ['A'..'Z']
    "cntrl" -> ['\0'..'\31']++"\127"
    "print" -> ['\32'..'\126']
    "xdigit" -> ['0'..'9']++['a'..'f']++['A'..'F']
    "word" -> ['0'..'9']++['a'..'z']++['A'..'Z']++"_"
    _ -> []
