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
import qualified Text.ParserCombinators.Parsec.Token as P

perl6Def  = javaStyle
          { P.commentStart   = "\n=begin\n"
          , P.commentEnd     = "\n=cut\n"
          , P.commentLine    = "#"
          , P.nestedComments = False
          , P.identStart     = letter <|> char '_'
          , P.identLetter    = alphaNum <|> oneOf "_"
          , P.reservedNames  = words $
                "if then else do while skip"
          , P.reservedOpNames= words $
                " . .+ .? .* .+ .() .[] .{} .<<>> .= " ++
                " ++ -- **  ! + - ~ ? * ** +^ ~^ ?^ \\ " ++
                " * / % x xx +& +< +> ~& ~<< ~>> " ++
                " + - ~ +| +^ ~| ~^ " ++
                " & | ^ " ++
                " rand sleep abs " ++
                " => but does cmp <=> .. ^.. ..^ ^..^ " ++
                " != == < <= > >= ~~ !~ eq ne lt le gt ge =:= " ++
                " && || ^^ // ?? :: = := ::= += **= xx= " ++
                " , <== print push any all true not " ++
                " ==> and or xor err ;"
          , P.opLetter       = oneOf (concat (P.reservedOpNames perl6Def))
          , P.caseSensitive  = False
          }

perl6Lexer = P.makeTokenParser perl6Def
reservedOp = P.reservedOp perl6Lexer
integer    = P.integer perl6Lexer
whiteSpace = P.whiteSpace perl6Lexer
parens     = P.parens perl6Lexer
float      = P.float perl6Lexer
lexeme     = P.lexeme perl6Lexer
identifier = P.identifier perl6Lexer
braces     = P.braces perl6Lexer
brackets   = P.brackets perl6Lexer
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
                then return (Left  $ n * numerator expo)
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
        power e | e < 0      = 1 % (10^e)
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

