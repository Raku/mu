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
          , P.identStart     = letter <|> oneOf "_:$@%&"
          , P.identLetter    = alphaNum <|> oneOf "_:"
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
stringLiteral = choice
    [ P.stringLiteral  perl6Lexer
    , singleQuoted
    ]

naturalOrFloat  = lexeme (natFloat) <?> "number"
    where
    natFloat        = do{ char '0'
                        ; zeroNumFloat
                        }
                      <|> decimalFloat
                      
    zeroNumFloat    =  do{ n <- hexadecimal <|> octal <|> binary
                         ; return (Left n)
                         }
                    <|> decimalFloat
                    <|> fractFloat 0
                    <|> return (Left 0)                  
                      
    decimalFloat    = do{ n <- decimal
                        ; option (Left n) 
                                 (try $ fractFloat n)
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
                        ; digits <- many digit <?> "fraction"
                        ; return (foldr op 0.0 digits)
                        }
                      <?> "fraction"
                    where
                      op d f    = (f + fromIntegral (digitToInt d))/10.0
                        
    exponent'       = do{ oneOf "eE"
                        ; f <- sign
                        ; e <- decimal <?> "exponent"
                        ; return (power (f e))
                        }
                      <?> "exponent"
                    where
                       power e  | e < 0      = 1.0/power(-e)
                                | otherwise  = fromInteger (10^e)


    -- integers and naturals
    int             = do{ f <- lexeme sign
                        ; n <- nat
                        ; return (f n)
                        }
                        
    -- sign            :: CharParser st (Integer -> Integer)
    sign            =   (char '-' >> return negate) 
                    <|> (char '+' >> return id)     
                    <|> return id

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

singleStrChar = quotedQuote <|> noneOf "'"

quotedQuote = do
    char '\\'
    anyChar

