{-# OPTIONS -fglasgow-exts #-}

{-
    Lexical analyzer.

    No words were laid on stream or stone
    When Durin woke and walked alone.
    He named the nameless hills and dells;
    He drank from yet untasted wells...
-}

module Lexer where
import Data.Char
import Debug.Trace
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Pos
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

perl6Def  = javaStyle
          { P.commentStart   = "\n=begin\n"
          , P.commentEnd     = "\n=cut\n"
          , P.commentLine    = "#"
          , P.nestedComments = False
          , P.identStart     = letter <|> oneOf "_:$@%&"
          , P.identLetter    = alphaNum <|> oneOf "_:"
          , P.reservedNames  = [ "true", "false", "do", "else", "not",
                               "if", "then", "while", "skip"
                               -- , "begin", "proc", "is", "end", "val", "res", "malloc" 
                              ]
          , P.reservedOpNames= words $
                " . .+ .? .* .+ .() .[] .{} .<<>> .= " ++
                " ++ -- **  ! + - ~ ? * ** +^ ~^ ?^ \\ " ++
                " * / % x xx +& +<< +>> ~& ~<< ~>> " ++
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
                        ; digits <- many1 digit <?> "fraction"
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
    hexadecimal     = do{ oneOf "xX"; number 16 hexDigit }
    octal           = do{ oneOf "oO"; number 8 octDigit  }
    binary          = do{ oneOf "bB"; number 2 (oneOf "01")  }

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
    string "\\'"
    return '\''

--------------------------------------------------------------------------------

newtype Token = Token (SourcePos, Symbol)

instance Show Token where
   show (Token (pos, symbol)) = show symbol

data Symbol
   = Word String
   | SingleQuoted String 
   | DotString String 
   | QuestionString String 
   | Num Int
   | Equals
   | LeftRoundBracket
   | RightRoundBracket
   | LeftSquareBracket
   | RightSquareBracket
   | Comma
   | Colon
   | SemiColon
   | Newline
   | BackSlash
   | Exclamation
   | Dash
   | GreaterThan
   | Space 
   | Bad String
   deriving (Eq)

-- slighlty pretty printing of Symbols via Show. The 
-- prettiness is due to the fact that the Parsec parser
-- uses show to print Symbols as part of error messages
instance Show Symbol where
   show (Word s)           = "word: " ++ s
   show (SingleQuoted s)   = "single quoted string: " ++ s 
   show (DotString s)      = ".-string: " ++ s 
   show (QuestionString s) = "?-string: " ++ s 
   show (Num n)            = "number: " ++ show n
   show Equals             = "equals sign: '='"
   show LeftRoundBracket   = "bracket: '('"
   show RightRoundBracket  = "bracket: ')'"
   show LeftSquareBracket  = "bracket: '['"
   show RightSquareBracket = "bracket: ']'"
   show Comma              = "comma: ','"
   show Colon              = "colon: ':'"
   show SemiColon          = "semi-colon: ';'"
   show Newline            = "newline: '\\n'"
   show BackSlash          = "backslash: '\'"
   show Exclamation        = "exclamation sign: '!'"
   show Dash               = "dash '-'"
   show GreaterThan        = "greater than sign: '>'"
   show Space              = "space"
   show (Bad str)          = str 

-- turn a stream of characters into a stream of tokens
lexer :: String -> String -> [Token]
lexer filename input 
   = lexWork (newPos filename 1 1) input

lexWork :: SourcePos -> String -> [Token]
lexWork pos [] = []
lexWork pos (x:xs)
   | x == '='  = simpleToken Equals nextCol 
   | x == '('  = simpleToken LeftRoundBracket nextCol 
   | x == ')'  = simpleToken RightRoundBracket nextCol 
   | x == '['  = simpleToken LeftSquareBracket nextCol 
   | x == ']'  = simpleToken RightSquareBracket nextCol 
   | x == ','  = simpleToken Comma nextCol 
   | x == ':'  = simpleToken Colon nextCol 
   | x == ';'  = simpleToken SemiColon nextCol 
   | x == '\\' = simpleToken BackSlash nextCol 
   | x == '!'  = simpleToken Exclamation nextCol 
   | x == '-'  = simpleToken Dash nextCol 
   | x == '>'  = simpleToken GreaterThan nextCol 
   | x == '\n' = simpleToken Newline nextLine 
   -- source location does not need to be accurate within a comment
   | x == '#'  = lexWork pos (dropWhile (/= '\n') xs)
   | x == '.'  = Token (pos, DotString xs) : lexWork pos (dropWhile (/= '\n') xs)
   | x == '?'  = Token (pos, QuestionString xs) : lexWork pos (dropWhile (/= '\n') xs)
   | isWhiteSpace x = simpleToken Space nextCol
   | isDigit x = let (num, rest) = span isDigit (x:xs)
                     nextPos = incSourceColumn pos (length num)
                 in Token (pos, Num $ read num) : lexWork nextPos rest
   | isAlpha x
        = let (restWord, rest) = span isWordChar xs
              word = x:restWord
              nextPos = incSourceColumn pos (length word)
          in Token (pos, Word word) : lexWork nextPos rest
   -- quoted strings need special care - escaped quotes can appear
   -- within the string, and the string might not be terminated
   -- by a quote in the case of a lexical error
   | x == '\'' 
        = let (thisString, rest) = lexTailQuotedString xs
              nextPos = incSourceColumn pos (length thisString + 2)
          in if null rest
                then [Token (pos, Bad $ "ill-quoted input: " ++ x:thisString)]
                else Token (pos, SingleQuoted thisString) : lexWork nextPos (tail rest) 
   | otherwise = simpleToken (Bad $ "symbol: " ++ show x) nextCol
   where
   simpleToken :: Symbol -> (SourcePos -> SourcePos) -> [Token]
   simpleToken tok srcPosUpdate 
      = Token (pos, tok) : lexWork (srcPosUpdate pos) xs 
   isWhiteSpace :: Char -> Bool
   isWhiteSpace c = c `elem` " \t\r\f\v\xA0"
   isWordChar :: Char -> Bool
   isWordChar c = isAlpha c || isDigit c 
   nextLine :: SourcePos -> SourcePos
   nextLine pos = incSourceLine (setSourceColumn pos 1) 1
   nextCol :: SourcePos -> SourcePos
   nextCol pos = incSourceColumn pos 1

-- lex the rest of a string following the first quote mark
-- must skip escaped quotes, and escaped backslashes
-- include the final quote mark in the result
lexTailQuotedString :: String -> (String, String)
lexTailQuotedString [] = ([], [])
lexTailQuotedString str@('\'':xs) = ([], str)
-- a backslash then a quote is escaped
lexTailQuotedString ('\\':'\'':xs)
   = let (string, rest) = lexTailQuotedString xs in ('\'':string, rest)
-- a backslash then a backslash is an escaped backslash
lexTailQuotedString ('\\':'\\':xs)
   = let (string, rest) = lexTailQuotedString xs in ('\\':'\\':string, rest)
lexTailQuotedString (x:xs)
   = let (string, rest) = lexTailQuotedString xs in (x:string, rest)
