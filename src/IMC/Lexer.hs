{-# OPTIONS_GHC -fglasgow-exts #-}

module IMC.Lexer where
import Internals
import Rule hiding (letter)

-- Transcribed from parrot/imcc/imcc.l
letter      = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_@"
digits      = many1 digit
hex         = char '0' >> oneOf "xX" >> many1 hexDigit
bin         = char '0' >> oneOf "bB" >> many1 (oneOf "01")
dot         = char '.'
sign        = oneOf "-+"
maybeSign   = option '+' sign
bigint      = maybeSign >> digits >> char 'L'
floatnum    = do
    maybeSign
    do
        choice
            [ digits >> dot >> digits
            , dot >> digits
            ]
        optional $ oneOf "eE" >> maybeSign >> digits
    <|> do
        digits
        oneOf "eE"
        maybeSign
        digits
        return ()
letterDigit         = alphaNum <|> char '_'
labelLetterDigit    = (letter >>= return . (:[])) <|> string "::"
id                  = letter >> many labelLetterDigit
stringConstant      = do
    char '"'
    many $ string "\\\"" <|> many (noneOf "^\"\n")
    char '"'
encChar         = letter <|> digit <|> char '-'
encChars        = many encChar
enc             = letter >> encChars >> char ':'
unicode         = enc >> stringConstant
charConstant    = do
    char '\''
    many $ noneOf "^'\n"
    char '\''
rankSpec        = do
    char '['
    many (char ',')
    char ']'
eol             = optional (char '\r') >> char '\n'
ws              = oneOf "\t\f\r\x1A "
sp              = char ' '

