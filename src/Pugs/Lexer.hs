{-# OPTIONS_GHC -fglasgow-exts #-}
{-# OPTIONS_GHC -#include "UnicodeC.h" #-}

{-|
    Lexical analyzer.

>   No words were laid on stream or stone
>   When Durin woke and walked alone.
>   He named the nameless hills and dells;
>   He drank from yet untasted wells...
-}

module Pugs.Lexer (
    wordAlpha, wordAny, isWordAlpha, isWordAny,
    maybeParens, parens, whiteSpace, lexeme, identifier,
    braces, brackets, angles, balanced, balancedDelim, decimal,

    ruleQualifiedIdentifier, ruleWhiteSpaceLine,

    symbol, interpolatingStringLiteral, escapeCode,

    rule, verbatimRule, literalRule,
    tryRule, tryVerbatimRule,
    tryChoice,

    ruleScope, ruleTrait, ruleTraitName, ruleBareTrait, ruleType,
    verbatimParens,
) where
import Pugs.Internals
import Pugs.AST
import Pugs.Rule
import Pugs.Rule.Language
import Pugs.Types
import Pugs.Parser.Types
import qualified Pugs.Rule.Token as P

perl6Def  :: LanguageDef st
perl6Def  = javaStyle
          { P.commentStart   = [] -- "=pod"
          , P.commentEnd     = [] -- "=cut"
          , P.commentLine    = "#"
          , P.nestedComments = False
          , P.identStart     = wordAlpha
          , P.identLetter    = wordAny
          , P.caseSensitive  = False
          }

wordAlpha   :: GenParser Char st Char
wordAny     :: GenParser Char st Char
wordAlpha   = satisfy isWordAlpha <?> "alphabetic word character"
wordAny     = satisfy isWordAny <?> "word character"

isWordAny   :: Char -> Bool
isWordAlpha :: Char -> Bool
isWordAny x = (isAlphaNum x || x == '_')
isWordAlpha x = (isAlpha x || x == '_')

perl6Lexer :: P.TokenParser st
perl6Lexer = P.makeTokenParser perl6Def

maybeParens :: CharParser st a -> CharParser st a
maybeParens p = choice [ parens p, p ]

parens     :: CharParser st a -> CharParser st a
parens     = P.parens     perl6Lexer
whiteSpace :: CharParser st ()
whiteSpace = P.whiteSpace perl6Lexer
lexeme     :: CharParser st a -> CharParser st a
lexeme     = P.lexeme     perl6Lexer
identifier :: CharParser st String
identifier = P.identifier perl6Lexer
braces     :: CharParser st a -> CharParser st a
braces     = P.braces     perl6Lexer
brackets   :: CharParser st a -> CharParser st a
brackets   = P.brackets   perl6Lexer
angles     :: CharParser st a -> CharParser st a
angles     = P.angles     perl6Lexer
balanced   :: CharParser st String
balanced   = P.balanced   perl6Lexer
balancedDelim :: Char -> Char
balancedDelim = P.balancedDelim perl6Lexer
decimal    :: CharParser st Integer
decimal    = P.decimal    perl6Lexer

ruleQualifiedIdentifier :: GenParser Char st String
ruleQualifiedIdentifier = verbatimRule "qualified identifier" $ do
    chunks  <- ruleVerbatimIdentifier `sepBy1` (try $ string "::")
    return $ concat (intersperse "::" chunks)

ruleVerbatimIdentifier :: GenParser Char st String
ruleVerbatimIdentifier = (<?> "identifier") $ do
    c  <- identStart perl6Def
    cs <- many (identLetter perl6Def)
    return (c:cs)

ruleWhiteSpaceLine :: GenParser Char st ()
ruleWhiteSpaceLine = do
    many $ satisfy (\x -> isSpace x && x /= '\n')
    ruleEndOfLine

ruleEndOfLine :: GenParser Char st ()
ruleEndOfLine = choice [ do { char '\n'; return () }, eof ]

symbol :: String -> GenParser Char st String
symbol s
    | isWordAny (last s) = try $ do
        rv <- string s
        choice [ eof >> return ' ', lookAhead (satisfy (aheadWord $ last s)) ]
        whiteSpace
        return rv
    | otherwise          = try $ do
        rv <- string s
        -- XXX Wrong - the correct solution is to lookahead as much as possible
        -- in the expression parser below
        choice [ eof >> return ' ', lookAhead (satisfy (aheadSym $ last s)) ]
        whiteSpace
        return rv
    where
    aheadWord x  '=' = not $ x `elem` (decodeUTF8 "xY¥")
    aheadWord _  y   = not $ isWordAny y
    aheadSym '-' '>' = False -- XXX hardcode
    aheadSym '!' '~' = False -- XXX hardcode
    aheadSym x   '=' = not (x `elem` "!~+-*&/|.%^")
    aheadSym '?' y   = not (y `elem` "&|^?")
    aheadSym '+' y   = not (y `elem` "&|^<>+")
    aheadSym '~' y   = not (y `elem` "&|^<>~")
    aheadSym '^' y   = not (y `elem` ".")
    aheadSym x   y   = y `elem` ";!" || x /= y

interpolatingStringLiteral :: RuleParser String    -- ^ Opening delimiter
			      -> RuleParser String   -- ^ Closing delimiter 
                              -> RuleParser Exp -- ^ Interpolator
                              -> RuleParser Exp -- ^ Entire string
                                                -- ^ (without delims)
interpolatingStringLiteral startrule endrule interpolator = do
    list <- stringList 0
    return . Cxt (CxtItem $ mkType "Str") $ homogenConcat list
    where
    homogenConcat :: [Exp] -> Exp
    homogenConcat [] = Val (VStr "")
    homogenConcat [v@(Val (VStr _))] = v
    homogenConcat (Val (VStr x):Val (VStr y):xs)
        = homogenConcat (Val (VStr (x ++ y)) : xs)
    homogenConcat (x:xs)
        = App (Var "&infix:~") Nothing [x, homogenConcat xs]
    
    stringList :: Int -> RuleParser [Exp]
    stringList i = try (do
	       ch <- endrule
	       if i == 0 then return []
			 else do rest <- stringList (i-1)
				 return (Val (VStr ch):rest))
	   <|> try (do
	       ch <- startrule
	       rest <- stringList (i+1)
	       return (Val (VStr ch):rest))
	   <|> do
	       parse <- interpolator
	       rest  <- stringList i
	       return (parse:rest)
	   <|> do
	       char <- anyChar
	       rest <- stringList i
	       return (Val (VStr [char]):rest)

-- | backslahed nonalphanumerics (except for \^) translate into themselves
escapeCode      :: GenParser Char st Char
escapeCode      = charEsc <|> charNum <|> charAscii <|> charControl <|> anyChar
                <?> "escape code"

charControl :: GenParser Char st Char
charControl     = do{ char '^'
                    ; code <- upper
                    ; return (toEnum (fromEnum code - fromEnum 'A'))
                    }

charNum :: GenParser Char st Char                    
charNum         = do{ code <- decimal 
                              <|> do{ char 'o'; number 8 octDigit }
                              <|> do{ char 'x'; number 16 hexDigit }
                              <|> do{ char 'd'; number 10 digit }
                    ; return (toEnum (fromInteger code))
                    }

number :: Integer -> GenParser tok st Char -> GenParser tok st Integer
number base baseDigit
    = do{ digits <- many1 baseDigit
        ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
        ; seq n (return n)
        }          

charEsc         :: GenParser Char st Char
charEsc         = choice (map parseEsc escMap)
                where
                  parseEsc (c,code)     = do{ char c; return code }
                  
charAscii       :: GenParser Char st Char
charAscii       = choice (map parseAscii asciiMap)
                where
                  parseAscii (asc,code) = try (do{ string asc; return code })


-- escape code tables
escMap          :: [(Char, Char)]
escMap          = zip ("abfnrtv\\\"\'") ("\a\b\f\n\r\t\v\\\"\'")
asciiMap        :: [(String, Char)]
asciiMap        = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2) 

ascii2codes     :: [String]
ascii2codes     = ["BS","HT","LF","VT","FF","CR","SO","SI","EM",
                   "FS","GS","RS","US","SP"]
ascii3codes     :: [String]
ascii3codes     = ["NUL","SOH","STX","ETX","EOT","ENQ","ACK","BEL",
                   "DLE","DC1","DC2","DC3","DC4","NAK","SYN","ETB",
                   "CAN","SUB","ESC","DEL"]

ascii2          :: [Char]
ascii2          = ['\BS','\HT','\LF','\VT','\FF','\CR','\SO','\SI',
                   '\EM','\FS','\GS','\RS','\US','\SP']
ascii3          :: [Char]
ascii3          = ['\NUL','\SOH','\STX','\ETX','\EOT','\ENQ','\ACK',
                   '\BEL','\DLE','\DC1','\DC2','\DC3','\DC4','\NAK',
                   '\SYN','\ETB','\CAN','\SUB','\ESC','\DEL']

rule :: String -> CharParser st a -> GenParser Char st a
rule name action = (<?> name) $ lexeme $ action

verbatimRule :: String -> GenParser tok st a -> GenParser tok st a
verbatimRule name action = (<?> name) $ action

literalRule :: String -> GenParser Char st a -> GenParser Char st a
literalRule name action = (<?> name) $ postSpace $ action

tryRule :: String -> GenParser Char st a -> GenParser Char st a
tryRule name action = (<?> name) $ lexeme $ try action

tryVerbatimRule :: String -> GenParser tok st a -> GenParser tok st a
tryVerbatimRule name action = (<?> name) $ try action

ruleScope :: RuleParser Scope
ruleScope = tryRule "scope" $ do
    scope <- choice $ map symbol scopes
    return (readScope scope)
    where
    scopes = map (map toLower) $ map (tail . show) $ enumFrom ((toEnum 1) :: Scope)
    readScope s
        | (c:cs)    <- s
        , [(x, _)]  <- reads ('S':toUpper c:cs)
        = x
        | otherwise
        = SGlobal

postSpace :: GenParser Char st a -> GenParser Char st a
postSpace rule = try $ do
    rv <- rule
    notFollowedBy wordAny
    whiteSpace
    return rv

ruleTrait :: GenParser Char st String
ruleTrait = rule "trait" $ do
    symbol "is" <|> symbol "does"
    trait <- do
        optional $ string "::" -- XXX Bad Hack
        ruleQualifiedIdentifier
    return trait

ruleTraitName :: String -> GenParser Char st String
ruleTraitName trait = rule "named trait" $ do
    symbol "is"
    symbol trait
    ruleQualifiedIdentifier

ruleBareTrait :: String -> GenParser Char st String
ruleBareTrait trait = rule "bare trait" $ do
    choice [ ruleTraitName trait
           , do symbol trait
                str <- ruleQualifiedIdentifier
                -- Hierarchical types like Hash of Str -- not yet recognised
                many . try $ do { whiteSpace; symbol "of"; ruleQualifiedIdentifier }
                return str
           ]

ruleType :: GenParser Char st String
ruleType = literalRule "context" $ do
    lead    <- upper
    rest    <- many (wordAny <|> oneOf ":&|")
    return (lead:rest)

tryChoice :: [GenParser tok st a] -> GenParser tok st a
tryChoice = choice . map try

verbatimParens :: GenParser Char st a -> GenParser Char st a
verbatimParens = between (lexeme $ char '(') (char ')')

