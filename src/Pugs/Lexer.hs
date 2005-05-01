{-# OPTIONS_GHC -fglasgow-exts -O #-}
{-# OPTIONS_GHC -#include "UnicodeC.h" #-}

{-
    Lexical analyzer.

    No words were laid on stream or stone
    When Durin woke and walked alone.
    He named the nameless hills and dells;
    He drank from yet untasted wells...
-}

module Pugs.Lexer where
import Pugs.Internals
import Pugs.AST
import Pugs.Rule
import Pugs.Rule.Language
import Pugs.Types
import qualified Pugs.Rule.Token as P

type RuleParser a = GenParser Char Env a
data ParensOption = ParensMandatory | ParensOptional
    deriving (Show, Eq)

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

literalIdentifier :: GenParser Char st String
literalIdentifier = do
    c <- wordAlpha
    cs <- many wordAny
    return (c:cs)
    
wordAlpha   :: GenParser Char st Char
wordAny     :: GenParser Char st Char
wordAlpha   = satisfy isWordAlpha <?> "alphabetic word character"
wordAny     = satisfy isWordAny <?> "word character"

isWordAny   :: Char -> Bool
isWordAlpha :: Char -> Bool
isWordAny x = (isAlphaNum x || x == '_')
isWordAlpha x = (isAlpha x || x == '_')

setVar :: String -> Val -> RuleParser ()
setVar = do
    -- env <- getState
    -- let lex = envLexical env
    -- setState env{ envLexical = lex' }
    error ""

getVar :: String -> RuleParser Val
getVar = do
    -- env <- getState
    error ""    

perl6Lexer = P.makeTokenParser perl6Def
parens     = P.parens perl6Lexer
whiteSpace = P.whiteSpace perl6Lexer
lexeme     = P.lexeme perl6Lexer
identifier = P.identifier perl6Lexer
braces     = P.braces perl6Lexer
brackets   = P.brackets perl6Lexer
angles     = P.angles perl6Lexer
balanced   = P.balanced perl6Lexer
balancedDelim = P.balancedDelim perl6Lexer
decimal    = P.decimal perl6Lexer
verbatimIdentifier = (<?> "identifier") $ do
    c  <- identStart perl6Def
    cs <- many (identLetter perl6Def)
    return (c:cs)

ruleWhiteSpaceLine = do
    many $ satisfy (\x -> isSpace x && x /= '\n')
    ruleEndOfLine
   
ruleEndOfLine = choice [ do { char '\n'; return () }, eof ]

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
    aheadSym '+' y   = not (y `elem` "&|^<>+")
    aheadSym '~' y   = not (y `elem` "&|^<>~")
    aheadSym x   y   = y `elem` ";!" || x /= y

interpolatingStringLiteral :: RuleParser x      -- Closing delimiter 
                              -> RuleParser Exp -- Interpolator
                              -> RuleParser Exp -- Entire string
                                                -- (without delims)

interpolatingStringLiteral endrule interpolator = do
    list <- stringList
    return . Cxt (CxtItem $ mkType "Str") $ homogenConcat list
    where
    homogenConcat :: [Exp] -> Exp
    homogenConcat [] = Val (VStr "")
    homogenConcat [v@(Val (VStr _))] = v
    homogenConcat (Val (VStr x):Val (VStr y):xs)
        = homogenConcat (Val (VStr (x ++ y)) : xs)
    homogenConcat (x:xs)
        = App "&infix:~" [x, homogenConcat xs] []
    
    stringList = do
        lookAhead endrule
        return []
        <|> do
        parse <- interpolator
        rest  <- stringList
        return (parse:rest)
        <|> do
        char <- anyChar
        rest <- stringList
        return (Val (VStr [char]):rest)

-- backslahed nonalphanumerics (except for ^) translate into themselves
escapeCode      = charEsc <|> charNum <|> charAscii <|> charControl <|> anyChar
                <?> "escape code"

-- charControl :: CharParser st Char
charControl     = do{ char '^'
                    ; code <- upper
                    ; return (toEnum (fromEnum code - fromEnum 'A'))
                    }

-- charNum :: CharParser st Char                    
charNum         = do{ code <- decimal 
                              <|> do{ char 'o'; number 8 octDigit }
                              <|> do{ char 'x'; number 16 hexDigit }
                              <|> do{ char 'd'; number 10 digit }
                    ; return (toEnum (fromInteger code))
                    }

number base baseDigit
    = do{ digits <- many1 baseDigit
        ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
        ; seq n (return n)
        }          

charEsc         = choice (map parseEsc escMap)
                where
                  parseEsc (c,code)     = do{ char c; return code }
                  
charAscii       = choice (map parseAscii asciiMap)
                where
                  parseAscii (asc,code) = try (do{ string asc; return code })


-- escape code tables
escMap          = zip ("abfnrtv\\\"\'") ("\a\b\f\n\r\t\v\\\"\'")
asciiMap        = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2) 

ascii2codes     = ["BS","HT","LF","VT","FF","CR","SO","SI","EM",
                   "FS","GS","RS","US","SP"]
ascii3codes     = ["NUL","SOH","STX","ETX","EOT","ENQ","ACK","BEL",
                   "DLE","DC1","DC2","DC3","DC4","NAK","SYN","ETB",
                   "CAN","SUB","ESC","DEL"]

ascii2          = ['\BS','\HT','\LF','\VT','\FF','\CR','\SO','\SI',
                   '\EM','\FS','\GS','\RS','\US','\SP']
ascii3          = ['\NUL','\SOH','\STX','\ETX','\EOT','\ENQ','\ACK',
                   '\BEL','\DLE','\DC1','\DC2','\DC3','\DC4','\NAK',
                   '\SYN','\ETB','\CAN','\SUB','\ESC','\DEL']

rule name action = (<?> name) $ lexeme $ action

verbatimRule name action = (<?> name) $ action

literalRule name action = (<?> name) $ postSpace $ action

tryRule name action = (<?> name) $ lexeme $ try action

tryVerbatimRule name action = (<?> name) $ try action

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

ruleTrait = do
    symbol "is"
    trait <- identifier
    return trait

ruleTraitName trait = do
    symbol "is"
    symbol trait
    identifier

ruleBareTrait trait = do
    choice [ ruleTraitName trait
           , do { symbol trait ; identifier }
           ]

ruleContext = literalRule "context" $ do
    lead    <- upper
    rest    <- many1 (wordAny <|> oneOf "&|")
    return (lead:rest)

ruleVarName = literalRule "variable name" $ do
    sigil   <- oneOf "$@%&"
    caret   <- option "" $ choice $ map string $ words " ^ * ? "
    name    <- many1 wordAny
    return $ (sigil:caret) ++ name

tryChoice = choice . map try

