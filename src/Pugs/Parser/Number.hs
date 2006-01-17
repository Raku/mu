{-# OPTIONS_GHC -#include "../../UnicodeC.h" #-}

module Pugs.Parser.Number (
    parseNatOrRat,
    naturalOrRat,
) where
import Pugs.Internals
import Pugs.Rule

parseNatOrRat :: String -> Either ParseError (Either Integer (Ratio Integer))
parseNatOrRat s = runParser naturalOrRat () "" s

naturalOrRat :: GenParser Char st (Either Integer (Ratio Integer))
naturalOrRat  = (<?> "number") $ do
    sig <- sign
    num <- natRat
    return $ if sig
        then num
        else case num of
            Left i  -> Left $ -i
            Right d -> Right $ -d
    where
    natRat = do
            try (char '0' >> zeroNumRat)
        <|> decimalRat
        <|> fractRatOnly

    zeroNumRat = do
            n <- hexadecimal <|> decimal <|> octalBad <|> octal <|> binary
            return (Left n)
        <|> decimalRat
        <|> fractRat 0
        <|> return (Left 0)

    decimalRat = do
        n <- decimalLiteral
        option (Left n) (try $ fractRat n)

    fractRatOnly = do
        fract <- try $ fraction many1
        expo  <- option (1%1) expo
        return (Right $ fract * expo) -- Right is Rat

    fractRat n = do
            fract <- try $ fraction many
            expo  <- option (1%1) expo
            return (Right $ ((n % 1) + fract) * expo) -- Right is Rat
        <|> do
            expo <- expo
            if expo < 1
                then return (Right $ (n % 1) * expo)
                else return (Right $ (n % 1) * expo)

    fraction count = do
            char '.'
            notFollowedBy . satisfy $ \x -> case x of
                '_' -> True
                '.' -> True
                '=' -> True
                _   -> isAlpha x
            digits <- count (satisfy isWordDigit) <?> "fraction"
            return (digitsToRat $ filter (/= '_') digits)
        <?> "fraction"
        where
        digitsToRat d = digitsNum d % (10 ^ length d)
        digitsNum d = foldl (\x y -> x * 10 + (toInteger $ digitToInt y)) 0 d
        isWordDigit x = (isDigit x || x == '_')

    expo :: GenParser Char st Rational
    expo = do
            oneOf "eE"
            f <- sign
            e <- decimalLiteral <?> "exponent"
            return (power (if f then e else -e))
        <?> "exponent"
        where
        power e | e < 0      = 1 % (10^abs(e))
                | otherwise  = (10^e) % 1

    sign            =   (char '-' >> return False)
                    <|> (char '+' >> return True)
                    <|> return True

    decimalLiteral         = number 10 digit
    hexadecimal     = do{ char 'x'; number 16 hexDigit }
    decimal         = do{ char 'd'; number 10 digit }
    octal           = do{ char 'o'; number 8 octDigit }
    octalBad        = do{ many1 octDigit ; fail "0100 is not octal in perl6 any more, use 0o100 instead." }
    binary          = do{ char 'b'; number 2 (oneOf "01")  }

    number base baseDigit = do
        d   <- baseDigit
        ds  <- many (baseDigit <|> do { char '_'; lookAhead baseDigit; return '_' })
        let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
            digits = (d : filter (/= '_') ds)
        seq n (return n)
