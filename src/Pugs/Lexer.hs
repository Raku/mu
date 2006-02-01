{-# OPTIONS_GHC -fglasgow-exts -fvia-C -optc-w #-}
{-# OPTIONS_GHC -#include "../UnicodeC.h" #-}

{-|
    Lexical analyzer.

>   No words were laid on stream or stone
>   When Durin woke and walked alone.
>   He named the nameless hills and dells;
>   He drank from yet untasted wells...
-}

module Pugs.Lexer (
    wordAlpha, wordAny, isWordAlpha, isWordAny,
    maybeParens, parens, whiteSpace, mandatoryWhiteSpace, lexeme, identifier,
    braces, brackets, angles, balanced, balancedDelim, decimal,

    ruleDelimitedIdentifier, ruleQualifiedIdentifier, ruleWhiteSpaceLine,

    symbol, interpolatingStringLiteral, escapeCode,

    rule, verbatimRule, literalRule,
    tryRule, tryVerbatimRule,
    tryChoice, ruleComma,

    ruleScope, ruleTrait, ruleTraitName, ruleBareTrait, ruleType,
    verbatimParens, verbatimBrackets, verbatimBraces,
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
mandatoryWhiteSpace :: CharParser st ()
mandatoryWhiteSpace = skipMany1 (oneOf " \t\n")  -- XXX unicode and whatnot
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

{-|
Match one or more identifiers, separated internally by the given delimiter.

Returns a list of the identifiers matched, discarding the delimiters.  You
can always recreate them using \"@concat $ intersperse delim@\" if you want,
or else use 'ruleQualifiedIdentifier'.
-}
ruleDelimitedIdentifier :: String -- ^ Delimiter (e.g. \'@::@\')
                        -> GenParser Char st [String]
ruleDelimitedIdentifier delim = verbatimRule "delimited identifier" $ do
    -- Allowing the leading delim actually leads to subtle oddness with things
    -- like `use jsan:.Foo` and `use pugs:::Foo`, so I took it out.
    --option "" (try $ string delim) -- leading delimiter
    ruleVerbatimIdentifier `sepBy1` (try $ string delim)

ruleQualifiedIdentifier :: GenParser Char st String
ruleQualifiedIdentifier = verbatimRule "qualified identifier" $ do
    chunks <- ruleDelimitedIdentifier "::"
    return $ concat (intersperse "::" chunks)

ruleVerbatimIdentifier :: GenParser Char st String
ruleVerbatimIdentifier = (<?> "identifier") $ do
    c  <- identStart perl6Def
    cs <- many (identLetter perl6Def)
    return (c:cs)

{-|
Match any amount of whitespace (not including newlines), followed by a newline
(as matched by 'ruleEndOfLine').
-}
ruleWhiteSpaceLine :: GenParser Char st ()
ruleWhiteSpaceLine = do
    many $ satisfy (\x -> isSpace x && x /= '\n')
    ruleEndOfLine

{-|
Match either a single newline, or EOF (which constitutes the termination of a
line anyway).
-}
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
    aheadSym '+' y   = not (y `elem` "&|^+")
    aheadSym '~' y   = not (y `elem` "&|^~")
    aheadSym '^' y   = not (y `elem` "^.")
    aheadSym x   y   = y `elem` ";!" || x /= y

interpolatingStringLiteral :: RuleParser String -- ^ Opening delimiter
                           -> RuleParser String -- ^ Closing delimiter 
                           -> RuleParser Exp    -- ^ Interpolator
                           -> RuleParser Exp    -- ^ Entire string
                                                --     (without delims)
interpolatingStringLiteral startrule endrule interpolator = do
    list <- stringList 0
    return $ Ann (Cxt (CxtItem $ mkType "Str")) (homogenConcat list)
    where
    homogenConcat :: [Exp] -> Exp
    homogenConcat [] = Val (VStr "")
    homogenConcat [v@(Val (VStr _))] = v
    homogenConcat (Val (VStr x):Val (VStr y):xs)
        = homogenConcat (Val (VStr (x ++ y)) : xs)
    homogenConcat (x:xs)
        = App (Var "&infix:~") Nothing [x, homogenConcat xs]
    
    stringList :: Int -> RuleParser [Exp]
    stringList i = tryChoice
        [ do
            parse <- interpolator
            rest  <- stringList i
            return (parse:rest)
        , do
            ch <- endrule
            if i == 0
                then return []
                else do
                    rest <- stringList (i-1)
                    return (Val (VStr ch):rest)
        , do
            ch <- startrule
            rest <- stringList (i+1)
            return (Val (VStr ch):rest)
        , do
            char <- anyChar
            rest <- stringList i
            return (Val (VStr [char]):rest)
        ]

-- | Backslashed non-alphanumerics (except for @\^@) translate into themselves.
escapeCode      :: GenParser Char st String
escapeCode      = ch charEsc <|> charNum <|> ch charAscii <|> ch charControl <|> ch anyChar
                <?> "escape code"
    where
    ch = fmap (:[])

charControl :: GenParser Char st Char
charControl     = do{ char 'c'
                    ; code <- upper <|> char '@'
                    ; return (toEnum (fromEnum code - fromEnum '@'))
                    }

-- This is currently the only escape that can return multiples.
charNum :: GenParser Char st String
charNum = do
    codes <- choice
        [ fmap (:[]) decimal 
        , based 'o'  8 octDigit
        , based 'x' 16 hexDigit
        , based 'd' 10 digit
        ]
    return $ map (toEnum . fromInteger) codes
    where
    based ch num p = do
        char ch
        choice [ verbatimBrackets (number num p `sepEndBy1` ruleComma)
               , fmap (:[]) (number num p)
               ]

ruleComma :: GenParser Char st ()
ruleComma = do
    lexeme (char ',')
    return ()

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
    scope <- ruleScopeName
    return $ readScope scope
    where
    readScope "state"   = SState
    readScope "my"      = SMy
    readScope "our"     = SOur
    readScope "let"     = SLet
    readScope "temp"    = STemp
    readScope "env"     = SEnv
    readScope _         = SGlobal

ruleScopeName :: RuleParser String
ruleScopeName = choice . map symbol . map (map toLower) . map (tail . show)
    $ [SState .. SOur]

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
    -- XXX: For now, we *only* *parse* "is export(...)". The arguments are
    -- thrown away. So module writers can give detailed export lists now,
    -- as otherwise
    -- <Aankhen``> Otherwise, it'll be a pain to go back to every module and
    --             change it all once the proper behaviour is implemented.
    optional $ verbatimParens $ many $ satisfy (/= ')')
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
    -- Valid type names: Foo, Bar::Baz, ::Grtz, ::?CLASS, but not :Foo
    lead    <- count 1 wordAlpha <|> string "::"
    rest    <- many (wordAny <|> oneOf ":&|?")
    return (lead ++ rest)

{-|
Attempt each of the given parsers in turn until one succeeds, but if one of
them fails we backtrack (i.e. retroactively consume no input) before trying
the next one.
-}
tryChoice :: [GenParser tok st a] -- ^ List of candidate parsers
          -> GenParser tok st a
tryChoice = choice . map try

{-|
Match '@(@', followed by the given parser, followed by '@)@'.
-}
verbatimParens :: GenParser Char st a -> GenParser Char st a
verbatimParens = between (lexeme $ char '(') (char ')')

{-|
Match '@\[@', followed by the given parser, followed by '@\]@'.
-}
verbatimBrackets :: GenParser Char st a -> GenParser Char st a
verbatimBrackets = between (lexeme $ char '[') (char ']')

{-|
Match '@{@', followed by the given parser, followed by '@}@'.
-}
verbatimBraces :: GenParser Char st a -> GenParser Char st a
verbatimBraces = between (lexeme $ char '{') (char '}')
