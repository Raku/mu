{-# OPTIONS_GHC -fglasgow-exts #-}

module PIL.Native.Parser where
import PIL.Native.Types
import PIL.Native.Coerce
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P

parseNativeLang :: Monad m => String -> m [NativeLangExpression]
parseNativeLang src = case ( runParser program () "-" src ) of
    Left err    -> fail (show err)
    Right exp   -> return exp
    where
    program = between bof eof (semiColonSep expression)
    bof = return ()

expression :: Parser NativeLangExpression
expression = (<?> "expression") $ do
    obj <- choice
        [ parens expression
        , fmap NL_Lit literal
        , fmap (NL_Var . mkStr) identifier
        ]
    maybeCall obj
    where
    method = (<?> "method") $ do
        x       <- noneOf " \n\t()0123456789"
        xs      <- many (noneOf " \n\t();,")
        return (x:xs)
    maybeCall obj = option obj $ do
        symbol "."
        name    <- method
        args    <- option [] (parens $ commaSep expression)
        maybeCall (mkCall obj name args)

literal :: Parser Native
literal = choice 
    [ lit "nil"     mkNil
    , lit "true"    True
    , fmap toNative pointyBlock
    , fmap toNative stringLiteral
    , fmap toNative singleQuoteStringLiteral
    , fmap toNative (brackets $ commaSep literal)
    , fmap toNative (braces $ commaSep pair)
    , try (fmap toNative naturalOrFloat)
    , fmap toNative integer
    ]
    where
    lit :: IsNative a => String -> a -> Parser Native
    lit s n = do
        symbol s
        return (toNative n)
    pair = do
        key <- literal
        symbol "=>"
        val <- literal
        return (key, val)

singleQuoteStringLiteral :: Parser String
singleQuoteStringLiteral = between (char '\'') (lexeme $ char '\'') $ do
    many $ choice
        [ try $ do { char '\\'; oneOf "\\'" }
        , satisfy (/= '\'')
        ]

commaSep :: Parser a -> Parser [a]
commaSep = (`sepEndBy` (symbol ","))

semiColonSep :: Parser a -> Parser [a]
semiColonSep = (`sepEndBy` (many1 $ symbol ";"))

pointyBlock :: Parser NativeBlock
pointyBlock = do
    symbol "->"
    params <- commaSep identifier
    body   <- braces (semiColonSep expression)
    return (mkBlock params body)

nativeLangDef  :: LanguageDef st
nativeLangDef  = javaStyle
    { commentStart   = "=pod"
    , commentEnd     = "=cut"
    , commentLine    = "#"
    , nestedComments = False
    , identStart     = oneOf "$@%&:"
    , identLetter    = noneOf " \n\t.,;()[]{}<>#"
    }

nativeLangLexer :: P.TokenParser st
nativeLangLexer = P.makeTokenParser nativeLangDef

parens     :: CharParser st a -> CharParser st a
parens     = P.parens     nativeLangLexer
whiteSpace :: CharParser st ()
whiteSpace = P.whiteSpace nativeLangLexer
mandatoryWhiteSpace :: CharParser st ()
mandatoryWhiteSpace = skipMany1 (oneOf " \t\n")  -- XXX unicode and whatnot
symbol     :: String -> CharParser st String
symbol     = P.symbol     nativeLangLexer
lexeme     :: CharParser st a -> CharParser st a
lexeme     = P.lexeme     nativeLangLexer
identifier :: CharParser st String
identifier = P.identifier nativeLangLexer
braces     :: CharParser st a -> CharParser st a
braces     = P.braces     nativeLangLexer
brackets   :: CharParser st a -> CharParser st a
brackets   = P.brackets   nativeLangLexer
angles     :: CharParser st a -> CharParser st a
angles     = P.angles     nativeLangLexer
integer    :: CharParser st Integer
integer    = P.integer    nativeLangLexer
stringLiteral    :: CharParser st String
stringLiteral    = P.stringLiteral    nativeLangLexer
naturalOrFloat  :: CharParser st (Either Integer Double)
naturalOrFloat  = P.naturalOrFloat nativeLangLexer
