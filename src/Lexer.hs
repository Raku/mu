{-# OPTIONS -fglasgow-exts #-}

{-
    Lexical analyzer.

    No words were laid on stream or stone
    When Durin woke and walked alone.
    He named the nameless hills and dells;
    He drank from yet untasted wells...
-}

module Lexer where
import Internals
import AST
import qualified Text.ParserCombinators.Parsec.Token as P

type RuleParser a = GenParser Char Env a

perl6Def  = javaStyle
          { P.commentStart   = "=pod"
          , P.commentEnd     = "=cut"
          , P.commentLine    = "#"
          , P.nestedComments = False
          , P.identStart     = wordAlpha
          , P.identLetter    = wordAny
          , P.caseSensitive  = False
          }

wordAlpha   = satisfy isWordAlpha <?> "alphabetic word character"
wordAny     = satisfy isWordAny <?> "word character"

isWordAny x = (isAlphaNum x || x == '_')
isWordAlpha x = (isAlpha x || x == '_')

perl6Lexer = P.makeTokenParser perl6Def
whiteSpace = P.whiteSpace perl6Lexer
parens     = P.parens perl6Lexer
lexeme     = P.lexeme perl6Lexer
identifier = P.identifier perl6Lexer
braces     = P.braces perl6Lexer
brackets   = P.brackets perl6Lexer
angles     = P.angles perl6Lexer

symbol s
    | isWordAny (last s) = try $ postSpace $ string s
    | otherwise          = try $ lexeme $ string s

stringLiteral = choice
    [ P.stringLiteral  perl6Lexer
    , singleQuoted
    ]

naturalOrRat  = do
        b <- lexeme sign
        n <- lexeme natRat
        return $ if b
            then n
            else case n of
                Left x -> Left $ -x
                Right y -> Right $ -y
    <?> "number"
    where
    natRat = do
            char '0'
            zeroNumRat
        <|> decimalRat
                      
    zeroNumRat = do
            n <- hexadecimal <|> octal <|> binary
            return (Left n)
        <|> decimalRat
        <|> fractRat 0
        <|> return (Left 0)                  
                      
    decimalRat = do
        n <- decimal
        option (Left n) (try $ fractRat n)

    fractRat n = do
            fract <- try fraction
            expo  <- option (1%1) expo
            return (Right $ ((n % 1) + fract) * expo) -- Right is Rat
        <|> do
            expo <- expo
            if expo < 1
                then return (Right $ (n % 1) * expo)
                else return (Right $ (n % 1) * expo)

    fraction = do
            char '.'
            try $ do { char '.'; unexpected "dotdot" } <|> return ()
            digits <- many digit <?> "fraction"
            return (digitsToRat digits)
        <?> "fraction"
        where
        digitsToRat d = digitsNum d % (10 ^ length d)
        digitsNum d = foldl (\x y -> x * 10 + (toInteger $ digitToInt y)) 0 d 

    expo :: GenParser Char st Rational
    expo = do
            oneOf "eE"
            f <- sign
            e <- decimal <?> "exponent"
            return (power (if f then e else -e))
        <?> "exponent"
        where
        power e | e < 0      = 1 % (10^abs(e))
                | otherwise  = (10^e) % 1

    -- sign            :: CharParser st (Integer -> Integer)
    sign            =   (char '-' >> return False) 
                    <|> (char '+' >> return True)
                    <|> return True

    nat             = zeroNumber <|> decimal
        
    zeroNumber      = do{ char '0'
                        ; hexadecimal <|> octal <|> decimal <|> return 0
                        }
                      <?> ""       

    decimal         = number 10 digit        
    hexadecimal     = do{ char 'x'; number 16 hexDigit }
    octal           = do{ char 'o'; number 8 octDigit  }
    binary          = do{ char 'b'; number 2 (oneOf "01")  }

    -- number :: Integer -> CharParser st Char -> CharParser st Integer
    number base baseDigit
        = do{ digits <- many1 baseDigit
            ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
            ; seq n (return n)
            }          


singleQuoted = lexeme (
                      do{ str <- between (char '\'')
                                         (char '\'' <?> "end of string")
                                         (many singleStrChar)
                        ; return (foldr (id (:)) "" str)
                        }
                      <?> "literal string")

singleStrChar = try quotedQuote <|> noneOf "'"

quotedQuote = do
    string "\\'"
    return '\''

rule name action = (<?> name) $ lexeme $ action

literalRule name action = (<?> name) $ postSpace $ action

tryRule name action = (<?> name) $ lexeme $ try $ action

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

postSpace rule = try $ do
    rv <- rule
    notFollowedBy wordAny
    whiteSpace
    return rv

ruleTrait trait = do
    symbol "is"
    symbol trait
    identifier

ruleBareTrait trait = do
    choice [ ruleTrait trait
           , do { symbol trait ; identifier }
           ]

ruleContext = literalRule "context" $ do
    lead    <- upper
    rest    <- many1 wordAny
    return (lead:rest)

ruleVarName = literalRule "variable name" $ do
    sigil   <- oneOf "$@%&"
    caret   <- option "" $ choice [ string "^", string "*" ]
    name    <- many1 wordAny
    return $ (sigil:caret) ++ name

tryChoice = choice . map try
