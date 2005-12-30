{-# OPTIONS_GHC -fglasgow-exts #-}

module PIL.Native.Parser where
import Control.Arrow (first)
import PIL.Native.Types
import PIL.Native.Coerce
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.Parser.Rule (Grammar, grammar)

-- Beginning of a Rule-based parser for minilang.
miniLang :: Grammar
miniLang = grammar
    [ "literal" ~:~ "nil | true | false"
    , "twoLits" ~:~ "<literal> <literal>"
    ]

{- 

PIL.Native.Parser

This module implements a parser for a mini-language which is embedded 
inside the language runtime. It is used to "script" the interactions of
the core runtime types. It's primary purpose is to describe the object 
metamodel. 

Here are some examples of the syntax:

Numbers:

  1`add(1);  # 1 + 1 = 2

Strings:

  "Hello"`concat(", world"); # "Hello" ~ ", world" 

Lists:
  
  []`push(1, 2, 3); # create a new array and push 1, 2, 3 to it
  [1, 2, 3`add(1)]; # creates array with 1, 2 & 4 in it 
  
Hashes:

  {}`store("key" => 2); # create a hash with one key ("key") and one value (2)
  
Blocks:  
  
  -> $x { $x }; # create a closure which returns it's own argument
  (-> $x { $x`add(1) })`(3); # call a closure with `() 

Method Invocation:

  $x`foo(1, 2);   # primitive method call
  $x.foo(1, 2);   # desugars into $x.send('foo', 1, 2)
  $x!foo(1, 2);   # desugars into $x.send_private('foo', 1, 2)

More complex examples: 

  # Factorial of 10
  
  (-> $n { (-> &fact { &fact`(&fact, $n) })
      `(-> &f, $x {
          $x`eq(0)`cond(
              -> { 1 },
              -> { $x`multiply( &f`(&f, $x`subtract(1)) ) })
      });
  })`(10);

See Also:

  PIL.Native.Pretty
  PIL.Native.Eval

-}

parseNativeLang :: Monad m => String -> m [NativeLangExpression]
parseNativeLang src = case parse program "-" src of
    Left err    -> fail (show err)
    Right exp   -> return exp
    where
    program = between bof eof expressionList

parseWith :: Parser a -> String -> a
parseWith p src = case parse (between bof eof p) src src of
    Left err    -> error (show err)
    Right exp   -> exp

bof :: Parser ()
bof = whiteSpace

parseSub :: String -> Native
parseSub = toNative . parseWith pointySub

parseExp :: String -> NativeLangExpression
parseExp = parseWith expression

expressionList :: Parser [NativeLangExpression]
expressionList = do
    many $ symbol ";"
    exps <- maybeEof $ semiColonSep (fmap Left assignment <|> fmap Right expression)
    return $ unroll exps
    where
    assignment = try $ do
        lhs <- identifier
        symbol ":="
        rhs <- expression
        return (lhs, rhs)
    unroll [] = []
    unroll (Right exp:xs) = (exp:unroll xs)
    unroll (Left (lhs, rhs):xs) = [mkCall sub "" [rhs]]
        where
        sub = ELit . toNative $ mkSub [lhs] (unroll xs)
    maybeEof p = do
        exps <- p
        do { eof; return (exps ++ [Right ESaveContinuation]) } <|> return exps

expression :: Parser NativeLangExpression
expression = (<?> "expression") $ do
    obj <- choice
        [ parens expression
        , selfExpression
        , arrayExpression
        , hashExpression
        , fmap ELit literal
        , variableExpression
        ]
    maybeCall obj
    where
    method = (<?> "method") $ do
        x       <- noneOf " \n\t()0123456789.`!"
        xs      <- many (noneOf " \n\t();,.`!")
        return (x:xs)
    maybeCall obj = option obj (primCall obj <|> sugarCall obj)
    primCall obj = do
        symbol "`"
        (name, args) <- functionCall <|> methodCall
        maybeCall $ mkCall obj name args
    sugarCall obj = do
        dot <- lexeme (oneOf ".!")
        (exp, args) <- fmap (first (ELit . toNative)) (functionCall <|> methodCall) <|> dynCall
        maybeCall $ case dot of
            '.' -> mkCall obj "send" (exp:args)
            '!' -> mkCall obj "send_private" (exp:args)
            _   -> error "impossible"
    -- $obj.(1,2,3)
    functionCall = do
        args    <- parens $ commaSep expression
        return ("", args)
    -- $obj.method(1,2,3)
    methodCall = do
        name    <- method
        args    <- option [] (parens $ commaSep expression)
        return (name, args)
    -- $obj`$method(1,2,3)
    dynCall = do
        exp     <- variableExpression
        args    <- option [] (parens $ commaSep expression)
        return (exp, args)
    selfExpression = do
        symbol "self"
        return (EVar $ mkStr "$?SELF")

variableExpression :: Parser NativeLangExpression
variableExpression = fmap (EVar . mkStr) identifier

literal :: Parser Native
literal = choice 
    [ lit "nil"     mkNil
    , lit "true"    True
    , lit "false"   False
    , fmap toNative pointySub
    , fmap toNative stringLiteral
    , fmap toNative singleQuoteStringLiteral
    , try (fmap toNative naturalOrFloat)
    , fmap toNative integer
    ]
    where
    lit :: IsNative a => String -> a -> Parser Native
    lit s n = do
        symbol s
        return (toNative n)

arrayExpression :: Parser NativeLangExpression
arrayExpression = do
    -- parse and analyze to see whether all the commaSep
    -- arguments are without redexes; if so, make it a literal
    -- otherwise desugar it as [].push form
    exps <- brackets $ commaSep expression
    return $ maybe (mkCall emptyArray "push" exps)
                   (ELit . toNative)
                   (allLiteral exps)
    where
    emptyArray = ELit $ toNative (empty :: NativeSeq)
    allLiteral [] = Just []
    allLiteral (ELit l:xs) = fmap (l:) (allLiteral xs)
    allLiteral _ = Nothing

hashExpression :: Parser NativeLangExpression
hashExpression = do
    exps <- braces $ commaSep pairExpression
    return $ maybe (mkCall emptyHash "push" $ unroll exps)
                   (ELit . toNative)
                   (allLiteral exps)
    where
    emptyHash = ELit $ toNative (empty :: NativeMap)
    unroll [] = []
    unroll ((k, v):xs) = (k:v:unroll xs)
    allLiteral [] = Just []
    allLiteral ((ELit k, ELit l):xs) = fmap ((k, l):) (allLiteral xs)
    allLiteral _ = Nothing

pairExpression :: Parser (NativeLangExpression, NativeLangExpression)
pairExpression = do
    key <- expression
    symbol "=>"
    val <- expression
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

pointySub :: Parser NativeSub
pointySub = do
    try $ symbol "->"
    params <- commaSep identifier
    body   <- braces expressionList
    return (mkSub params body)

nativeLangDef  :: LanguageDef st
nativeLangDef  = javaStyle
    { commentStart   = "=pod"
    , commentEnd     = "=cut"
    , commentLine    = "#"
    , nestedComments = False
    , identStart     = oneOf "$@%&:"
    , identLetter    = noneOf " \n\t.`!,;()[]{}<>#"
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
