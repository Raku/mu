module Pugs.Parser.Number (
    naturalOrRat,
) where
import Pugs.Internals
import Pugs.Rule

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
            char '0'
            zeroNumRat
        <|> decimalRat

    zeroNumRat = do
            n <- hexadecimal <|> decimal <|> octalBad <|> octal <|> binary
            return (Left n)
        <|> decimalRat
        <|> fractRat 0
        <|> return (Left 0)

    decimalRat = do
        n <- decimalLiteral
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
            notFollowedBy . satisfy $ \x ->
                (isAlpha x && ((x /=) `all` "eE"))
                || ((x ==) `any` ".=")
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

    number base baseDigit
        = do{ digits <- many1 baseDigit
            ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
            ; seq n (return n)
            }
