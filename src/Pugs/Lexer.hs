{-# OPTIONS_GHC -fglasgow-exts -fvia-C -optc-w #-}

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
    tryChoice, ruleComma, ruleWs,

    ruleScope, ruleTrait, ruleTraitName, ruleBareTrait, ruleType,
    verbatimParens, verbatimBrackets, verbatimBraces,
) where
import Pugs.Internals
import Pugs.AST
import Pugs.Rule
import Pugs.Types
import Pugs.Parser.Types
import qualified Text.ParserCombinators.Parsec.Char as C (satisfy)

identStart  = satisfy isWordAlpha
identLetter = satisfy isWordAny

wordAlpha   :: RuleParser Char
wordAny     :: RuleParser Char
wordAlpha   = satisfy isWordAlpha <?> "alphabetic word character"
wordAny     = satisfy isWordAny <?> "word character"

isWordAny   :: Char -> Bool
isWordAlpha :: Char -> Bool
isWordAny x = (isAlphaNum x || x == '_')
isWordAlpha x = (isAlpha x || x == '_')

maybeParens :: RuleParser a -> RuleParser a
maybeParens p = choice [ parens p, p ]

parens p        = between (symbol "(") (symbol ")") p
braces p        = between (symbol "{") (symbol "}") p
angles p        = between (symbol "<") (symbol ">") p
brackets p      = between (symbol "[") (symbol "]") p

mandatoryWhiteSpace :: RuleParser ()
mandatoryWhiteSpace = skipMany1 (simpleSpace <|> comment)

balancedDelim :: Char -> Char
balancedDelim c = case c of
    '\x0028' -> '\x0029'; '\x003C' -> '\x003E'; '\x005B' -> '\x005D';
    '\x007B' -> '\x007D'; '\x00AB' -> '\x00BB'; '\x0F3A' -> '\x0F3B';
    '\x0F3C' -> '\x0F3D'; '\x169B' -> '\x169C'; '\x2039' -> '\x203A';
    '\x2045' -> '\x2046'; '\x207D' -> '\x207E'; '\x208D' -> '\x208E';
    '\x2208' -> '\x220B'; '\x2209' -> '\x220C'; '\x220A' -> '\x220D';
    '\x2215' -> '\x29F5'; '\x223C' -> '\x223D'; '\x2243' -> '\x22CD';
    '\x2252' -> '\x2253'; '\x2254' -> '\x2255'; '\x2264' -> '\x2265';
    '\x2266' -> '\x2267'; '\x2268' -> '\x2269'; '\x226A' -> '\x226B';
    '\x226E' -> '\x226F'; '\x2270' -> '\x2271'; '\x2272' -> '\x2273';
    '\x2274' -> '\x2275'; '\x2276' -> '\x2277'; '\x2278' -> '\x2279';
    '\x227A' -> '\x227B'; '\x227C' -> '\x227D'; '\x227E' -> '\x227F';
    '\x2280' -> '\x2281'; '\x2282' -> '\x2283'; '\x2284' -> '\x2285';
    '\x2286' -> '\x2287'; '\x2288' -> '\x2289'; '\x228A' -> '\x228B';
    '\x228F' -> '\x2290'; '\x2291' -> '\x2292'; '\x2298' -> '\x29B8';
    '\x22A2' -> '\x22A3'; '\x22A6' -> '\x2ADE'; '\x22A8' -> '\x2AE4';
    '\x22A9' -> '\x2AE3'; '\x22AB' -> '\x2AE5'; '\x22B0' -> '\x22B1';
    '\x22B2' -> '\x22B3'; '\x22B4' -> '\x22B5'; '\x22B6' -> '\x22B7';
    '\x22C9' -> '\x22CA'; '\x22CB' -> '\x22CC'; '\x22D0' -> '\x22D1';
    '\x22D6' -> '\x22D7'; '\x22D8' -> '\x22D9'; '\x22DA' -> '\x22DB';
    '\x22DC' -> '\x22DD'; '\x22DE' -> '\x22DF'; '\x22E0' -> '\x22E1';
    '\x22E2' -> '\x22E3'; '\x22E4' -> '\x22E5'; '\x22E6' -> '\x22E7';
    '\x22E8' -> '\x22E9'; '\x22EA' -> '\x22EB'; '\x22EC' -> '\x22ED';
    '\x22F0' -> '\x22F1'; '\x22F2' -> '\x22FA'; '\x22F3' -> '\x22FB';
    '\x22F4' -> '\x22FC'; '\x22F6' -> '\x22FD'; '\x22F7' -> '\x22FE';
    '\x2308' -> '\x2309'; '\x230A' -> '\x230B'; '\x2329' -> '\x232A';
    '\x23B4' -> '\x23B5'; '\x2768' -> '\x2769'; '\x276A' -> '\x276B';
    '\x276C' -> '\x276D'; '\x276E' -> '\x276F'; '\x2770' -> '\x2771';
    '\x2772' -> '\x2773'; '\x2774' -> '\x2775'; '\x27C3' -> '\x27C4';
    '\x27C5' -> '\x27C6'; '\x27D5' -> '\x27D6'; '\x27DD' -> '\x27DE';
    '\x27E2' -> '\x27E3'; '\x27E4' -> '\x27E5'; '\x27E6' -> '\x27E7';
    '\x27E8' -> '\x27E9'; '\x27EA' -> '\x27EB'; '\x2983' -> '\x2984';
    '\x2985' -> '\x2986'; '\x2987' -> '\x2988'; '\x2989' -> '\x298A';
    '\x298B' -> '\x298C'; '\x298D' -> '\x298E'; '\x298F' -> '\x2990';
    '\x2991' -> '\x2992'; '\x2993' -> '\x2994'; '\x2995' -> '\x2996';
    '\x2997' -> '\x2998'; '\x29C0' -> '\x29C1'; '\x29C4' -> '\x29C5';
    '\x29CF' -> '\x29D0'; '\x29D1' -> '\x29D2'; '\x29D4' -> '\x29D5';
    '\x29D8' -> '\x29D9'; '\x29DA' -> '\x29DB'; '\x29F8' -> '\x29F9';
    '\x29FC' -> '\x29FD'; '\x2A2B' -> '\x2A2C'; '\x2A2D' -> '\x2A2E';
    '\x2A34' -> '\x2A35'; '\x2A3C' -> '\x2A3D'; '\x2A64' -> '\x2A65';
    '\x2A79' -> '\x2A7A'; '\x2A7D' -> '\x2A7E'; '\x2A7F' -> '\x2A80';
    '\x2A81' -> '\x2A82'; '\x2A83' -> '\x2A84'; '\x2A8B' -> '\x2A8C';
    '\x2A91' -> '\x2A92'; '\x2A93' -> '\x2A94'; '\x2A95' -> '\x2A96';
    '\x2A97' -> '\x2A98'; '\x2A99' -> '\x2A9A'; '\x2A9B' -> '\x2A9C';
    '\x2AA1' -> '\x2AA2'; '\x2AA6' -> '\x2AA7'; '\x2AA8' -> '\x2AA9';
    '\x2AAA' -> '\x2AAB'; '\x2AAC' -> '\x2AAD'; '\x2AAF' -> '\x2AB0';
    '\x2AB3' -> '\x2AB4'; '\x2ABB' -> '\x2ABC'; '\x2ABD' -> '\x2ABE';
    '\x2ABF' -> '\x2AC0'; '\x2AC1' -> '\x2AC2'; '\x2AC3' -> '\x2AC4';
    '\x2AC5' -> '\x2AC6'; '\x2ACD' -> '\x2ACE'; '\x2ACF' -> '\x2AD0';
    '\x2AD1' -> '\x2AD2'; '\x2AD3' -> '\x2AD4'; '\x2AD5' -> '\x2AD6';
    '\x2AEC' -> '\x2AED'; '\x2AF7' -> '\x2AF8'; '\x2AF9' -> '\x2AFA';
    '\x2E02' -> '\x2E03'; '\x2E04' -> '\x2E05'; '\x2E09' -> '\x2E0A';
    '\x2E0C' -> '\x2E0D'; '\x2E1C' -> '\x2E1D'; '\x3008' -> '\x3009';
    '\x300A' -> '\x300B'; '\x300C' -> '\x300D'; '\x300E' -> '\x300F';
    '\x3010' -> '\x3011'; '\x3014' -> '\x3015'; '\x3016' -> '\x3017';
    '\x3018' -> '\x3019'; '\x301A' -> '\x301B'; '\x301D' -> '\x301E';
    '\xFD3E' -> '\xFD3F'; '\xFE17' -> '\xFE18'; '\xFE35' -> '\xFE36';
    '\xFE37' -> '\xFE38'; '\xFE39' -> '\xFE3A'; '\xFE3B' -> '\xFE3C';
    '\xFE3D' -> '\xFE3E'; '\xFE3F' -> '\xFE40'; '\xFE41' -> '\xFE42';
    '\xFE43' -> '\xFE44'; '\xFE47' -> '\xFE48'; '\xFE59' -> '\xFE5A';
    '\xFE5B' -> '\xFE5C'; '\xFE5D' -> '\xFE5E'; '\xFF08' -> '\xFF09';
    '\xFF1C' -> '\xFF1E'; '\xFF3B' -> '\xFF3D'; '\xFF5B' -> '\xFF5D';
    '\xFF5F' -> '\xFF60'; '\xFF62' -> '\xFF63'; other    -> other

-- balanced: parses an open/close delimited expression of any non-alphanumeric character
balanced :: RuleParser String
balanced = do
    notFollowedBy alphaNum
    opendelim <- anyChar 
    contents <- many $ satisfy (/= balancedDelim opendelim)
    char $ balancedDelim opendelim
    return contents

-- The \b rule.
ruleWordBoundary :: RuleParser ()
ruleWordBoundary = do
    prev <- getPrevCharClass
    case prev of
        SpaceClass -> return ()
        _  -> do
            curr <- getCurrCharClass
            guard (prev /= curr)

-- The <ws> rule.
ruleWs :: RuleParser ()
ruleWs = do
    prev <- getPrevCharClass
    case prev of
        SpaceClass -> whiteSpace
        _  -> do
            curr <- getCurrCharClass
            if prev == curr then mandatoryWhiteSpace else case curr of
                SpaceClass  -> whiteSpace
                _           -> return ()

{-|
Match one or more identifiers, separated internally by the given delimiter.

Returns a list of the identifiers matched, discarding the delimiters.  You
can always recreate them using \"@concat $ intersperse delim@\" if you want,
or else use 'ruleQualifiedIdentifier'.
-}
ruleDelimitedIdentifier :: String -- ^ Delimiter (e.g. \'@::@\')
                        -> RuleParser [String]
ruleDelimitedIdentifier delim = verbatimRule "delimited identifier" $ do
    -- Allowing the leading delim actually leads to subtle oddness with things
    -- like `use jsan:.Foo` and `use pugs:::Foo`, so I took it out.
    --option "" (try $ string delim) -- leading delimiter
    ruleVerbatimIdentifier `sepBy1` (try $ string delim)

ruleQualifiedIdentifier :: RuleParser String
ruleQualifiedIdentifier = verbatimRule "qualified identifier" $ do
    chunks <- ruleDelimitedIdentifier "::"
    return $ concat (intersperse "::" chunks)

ruleVerbatimIdentifier :: RuleParser String
ruleVerbatimIdentifier = (<?> "identifier") $ do
    c  <- identStart
    cs <- many identLetter
    return (c:cs)

{-|
Match any amount of whitespace (not including newlines), followed by a newline
(as matched by 'ruleEndOfLine').
-}
ruleWhiteSpaceLine :: RuleParser ()
ruleWhiteSpaceLine = do
    many $ satisfy (\x -> isSpace x && x /= '\n')
    ruleEndOfLine

{-|
Match either a single newline, or EOF (which constitutes the termination of a
line anyway).
-}
ruleEndOfLine :: RuleParser ()
ruleEndOfLine = choice [ do { char '\n'; return () }, eof ]

symbol :: String -> RuleParser String
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
    aheadWord x  '=' = not $ x `elem` "xY\xA5" -- ¥
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
interpolatingStringLiteral startRule endRule interpolator = do
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
    stringList i = choice
        [ do
            parse <- interpolator
            rest  <- stringList i
            return (parse:rest)
        , do
            ch    <- try endRule
            if i == 0
                then return []
                else do
                    rest <- stringList (i-1)
                    return (Val (VStr ch):rest)
        , do
            ch   <- try startRule
            rest <- stringList (i+1)
            return (Val (VStr ch):rest)
        , do
            char <- anyChar
            rest <- stringList i
            return (Val (VStr [char]):rest)
        ]

-- | Backslashed non-alphanumerics (except for @\^@) translate into themselves.
escapeCode      :: RuleParser String
escapeCode      = ch charEsc <|> charNum <|> ch charAscii <|> ch charControl <|> ch anyChar
                <?> "escape code"
    where
    ch = fmap (:[])

charControl :: RuleParser Char
charControl     = do{ char 'c'
                    ; code <- upper <|> char '@'
                    ; return (toEnum (fromEnum code - fromEnum '@'))
                    }

-- This is currently the only escape that can return multiples.
charNum :: RuleParser String
charNum = do
    codes <- choice
        [ many1 digit >>= \ds -> do
            trace ("Warning: Escape sequence \\" ++ ds ++ " is invalid; write \\d" ++ ds ++ " instead") $
                return [read ds]
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

ruleComma :: RuleParser ()
ruleComma = do
    lexeme (char ',')
    return ()

number :: Integer -> RuleParser Char -> RuleParser Integer
number base baseDigit
    = do{ digits <- many1 baseDigit
        ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
        ; seq n (return n)
        }          

charEsc         :: RuleParser Char
charEsc         = choice (map parseEsc escMap)
                where
                  parseEsc (c,code)     = do{ char c; return code }
                  
charAscii       :: RuleParser Char
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

rule :: String -> RuleParser a -> RuleParser a
rule name = (<?> name) . lexeme

verbatimRule :: String -> RuleParser a -> RuleParser a
verbatimRule name = (<?> name)

literalRule :: String -> RuleParser a -> RuleParser a
literalRule name = (<?> name) . postSpace

tryRule :: String -> RuleParser a -> RuleParser a
tryRule name = (<?> name) . lexeme . try

tryVerbatimRule :: String -> RuleParser a -> RuleParser a
tryVerbatimRule name = (<?> name)

ruleScope :: RuleParser Scope
ruleScope = rule "scope" $ do
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

postSpace :: RuleParser a -> RuleParser a
postSpace rule = try $ do
    rv <- rule
    notFollowedBy wordAny
    whiteSpace
    return rv

ruleTrait :: [String] -> RuleParser (String, String)
ruleTrait auxs = rule "trait" $ do
    aux <- choice $ map symbol auxs
    trait <- do
        optional $ string "::" -- XXX Bad Hack
        ruleQualifiedIdentifier
    -- XXX: For now, we *only* *parse* "is export(...)". The arguments are
    -- thrown away. So module writers can give a sensible export lists now,
    -- as otherwise
    -- <Aankhen``> Otherwise, it'll be a pain to go back to every module and
    --             change it all once the proper behaviour is implemented.
    optional $ verbatimParens $ many $ satisfy (/= ')')
    return (aux, trait)

ruleTraitName :: String -> RuleParser String
ruleTraitName trait = rule "named trait" $ do
    symbol "is"
    symbol trait
    ruleQualifiedIdentifier

ruleBareTrait :: String -> RuleParser String
ruleBareTrait trait = rule "bare trait" $ do
    choice [ ruleTraitName trait
           , do symbol trait
                str <- ruleQualifiedIdentifier
                -- Hierarchical types like Hash of Str -- not yet recognised
                many . try $ do { whiteSpace; symbol "of"; ruleQualifiedIdentifier }
                return str
           ]

ruleType :: RuleParser String
ruleType = literalRule "context" $ do
    -- Valid type names: Foo, Bar::Baz, ::Grtz, ::?CLASS, but not :Foo
    lead    <- count 1 wordAlpha <|> (string "::" >> return [])
    rest    <- many (wordAny <|> oneOf ":&|?")
    return (lead ++ rest)

{-|
Attempt each of the given parsers in turn until one succeeds, but if one of
them fails we backtrack (i.e. retroactively consume no input) before trying
the next one.
-}
tryChoice :: [RuleParser a] -- ^ List of candidate parsers
          -> RuleParser a
tryChoice = choice . map try

{-|
Match '@(@', followed by the given parser, followed by '@)@'.
-}
verbatimParens :: RuleParser a -> RuleParser a
verbatimParens = between (lexeme $ char '(') (char ')')

{-|
Match '@\[@', followed by the given parser, followed by '@\]@'.
-}
verbatimBrackets :: RuleParser a -> RuleParser a
verbatimBrackets = between (lexeme $ char '[') (char ']')

{-|
Match '@{@', followed by the given parser, followed by '@}@'.
-}
verbatimBraces :: RuleParser a -> RuleParser a
verbatimBraces = between (lexeme $ char '{') (char '}')


-----------------------------------------------------------
-- Chars & Strings
-----------------------------------------------------------
-----------------------------------------------------------
-- Numbers
-----------------------------------------------------------
-- naturalOrFloat :: CharParser st (Either Integer Double)
naturalOrFloat  = lexeme (natFloat) <?> "number"

float           = lexeme floating   <?> "float"
integer         = lexeme int        <?> "integer"
natural         = lexeme nat        <?> "natural"


-- floats
floating        = do{ n <- decimal 
                    ; fractExponent n
                    }


natFloat        = do{ char '0'
                    ; zeroNumFloat
                    }
                    <|> decimalFloat
                    
zeroNumFloat    =  do{ n <- hexadecimal <|> octal
                        ; return (Left n)
                        }
                <|> decimalFloat
                <|> fractFloat 0
                <|> return (Left 0)                  
                    
decimalFloat    = do{ n <- decimal
                    ; option (Left n) 
                                (fractFloat n)
                    }

fractFloat n    = do{ f <- fractExponent n
                    ; return (Right f)
                    }
                    
fractExponent n = do{ fract <- fraction
                    ; expo  <- option 1.0 exponent'
                    ; return ((fromInteger n + fract)*expo)
                    }
                <|>
                    do{ expo <- exponent'
                    ; return ((fromInteger n)*expo)
                    }

fraction        = do{ char '.'
                    ; digits <- many1 digit <?> "fraction"
                    ; return (foldr op 0.0 digits)
                    }
                    <?> "fraction"
                where
                    op d f    = (f + fromIntegral (digitToInt d))/10.0
                    
exponent'       = do{ oneOf "eE"
                    ; f <- sign'
                    ; e <- decimal <?> "exponent"
                    ; return (power (f e))
                    }
                    <?> "exponent"
                where
                    power e  | e < 0      = 1.0/power(-e)
                            | otherwise  = fromInteger (10^e)


-- integers and naturals
int             = nat 
{-do{ f <- lexeme sign
                    ; n <- nat
                    ; return (f n)
                    }
                    -}
                    
-- sign            :: CharParser st (Integer -> Integer)
sign'           =   (char '-' >> return negate) 
                <|> (char '+' >> return id)     
                <|> return id

nat             = zeroNumber <|> decimal
    
zeroNumber      = do{ char '0'
                    ; hexadecimal <|> octal <|> decimal <|> return 0
                    }
                    <?> ""       

decimal         = number 10 digit        
hexadecimal     = do{ oneOf "xX"; number 16 hexDigit }
octal           = do{ oneOf "oO"; number 8 octDigit  }

-----------------------------------------------------------
-- Identifiers & Reserved words
-----------------------------------------------------------
identifier = lexeme $ try $ ident
    
ident           
    = do{ c <- identStart
        ; cs <- many identLetter
        ; return (c:cs)
        }
    <?> "identifier"

-----------------------------------------------------------
-- White space & symbols
-----------------------------------------------------------
lexeme p       
    = do{ x <- p; whiteSpace; return x  }
    
    
--whiteSpace    
whiteSpace = skipMany (simpleSpace <|> comment)

comment = do
    char '#' <?> "comment"
    pos <- getPosition
    if sourceColumn pos /= 2 then multiLineComment <|> skipToLineEnd else do
    -- Beginning of line - parse #line directive
    isPlain <- (<|> return True) $ try $ do
        string "line"
        many1 $ satisfy (\x -> isSpace x && x /= '\n')
        return False
    if isPlain then skipToLineEnd else do
    line <- decimal
    file <- (<|> return Nothing) $ try $ do
        many1 $ satisfy (\x -> isSpace x && x /= '\n')
        fileNameQuoted <|> fileNameBare
    if file == Just Nothing then skipToLineEnd else do
    many $ satisfy (/= '\n')
    setPosition $ pos
        `setSourceLine`     (fromInteger line - 1)
        `setSourceColumn`   1
        `setSourceName`     maybe (sourceName pos) fromJust file
    return ()

fileNameQuoted = try $ do
    char '"'
    file <- many (satisfy (/= '"'))
    char '"'
    many $ satisfy (\x -> isSpace x && x /= '\n')
    lookAhead (satisfy (== '\n'))
    return $ Just $ Just file

fileNameBare = try $ do
    file <- many1 $ satisfy (not . isSpace)
    many $ satisfy (\x -> isSpace x && x /= '\n')
    (<|> return (Just Nothing)) $ try $ do
        lookAhead (satisfy (== '\n'))
        return $ Just $ Just file

skipToLineEnd = do
    skipMany (satisfy (/= '\n'))
    return ()
        
simpleSpace =
    skipMany1 (satisfy isSpace)    
    
-- XXX - nesting
multiLineComment = do
    openOne <- satisfy (\x -> balancedDelim x /= x)
    more    <- many (char openOne)
    let closeOne = balancedDelim openOne
        openAll  = string (openOne:more)
        closeAll = string (replicate (1 + length more) (balancedDelim openOne))
        scanOne  = do
            c <- anyChar
            if c == closeOne then return () else do
            if c == openOne then scanOne >> scanOne else do
            scanOne
        scanAll  = choice
            [ do { try closeAll; return () }
            , do { try openAll; scanAll; scanAll }
            , do { anyChar; scanAll }
            ]
    if null more then scanOne else scanAll
