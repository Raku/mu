{-# OPTIONS -fglasgow-exts #-}

{-
    Higher-level parser for building ASTs.

    I sang of leaves, of leaves of gold, and leaves of gold there grew:
    Of wind I sang, a wind there came and in the branches blew.
    Beyond the Sun, beyond the Moon, the foam was on the Sea,
    And by the strand of Ilmarin there grew a golden Tree...
-}

module Parser where
import Internals
import AST
import Lexer

type StateParser a = GenParser Char () a

operators :: OperatorTable Char () Exp
operators =
    [ methOps  " . .+ .? .* .+ .() .[] .{} .<<>> .= "   -- Method postfix
    , postOps  " ++ -- "                                -- Auto-Increment
    , rightOps " ** "                                   -- Exponentiation
    , preOps   " ! + - ~ ? * ** +^ ~^ ?^ \\ "           -- Symbolic Unary
    , leftOps  " * / % x xx +& +< +> ~& ~< ~> "         -- Multiplicative
    , leftOps  " + - ~ +| +^ ~| ~^ "                    -- Additive
    , leftOps  " & ! "                                  -- Junctive And
    , leftOps  " ^ | "                                  -- Junctive Or
    , preOps   primitiveUnaryFunctions                  -- Name Unary
    , leftOps  " but does "                             -- Traits
      ++ noneOps " => but does cmp <=> .. ^.. ..^ ^..^ "-- Non-chaining Binary
      ++ postOps "..."                                  -- Infinite range
    , chainOps $
               " != == < <= > >= ~~ !~ " ++
               " eq ne lt le gt ge =:= "                -- Chained Binary
    , leftOps  " && "                                   -- Tight And
    , leftOps  " || ^^ // "                             -- Tight Or
    , ternOps  [("??", "::")]                           -- Ternary
    , rightSyn " = := ::= += **= xx= "                  -- Assignment
    , listOps  " , "                                    -- List Item Separator
    , preOps   primitiveListFunctions                   -- List Operator
    , leftOps  " ==> "                                  -- Pipe Forward
    , leftOps  " and "                                  -- Loose And
    , leftOps  " or xor err "                           -- Loose Or
    , leftSyn  " ; "                                    -- Terminator
    ]

primitiveListFunctions = " not <== any all one none"

parseExp = parseTerm

parseOp = buildExpressionParser operators parseTerm

ops f s = [f n | n <- words s]

preOps      = ops $ makeOp1 Prefix "&prefix:" App
postOps     = ops $ makeOp1 Postfix "&postfix:" App
leftOps     = ops $ makeOp2 AssocLeft "&infix:" App
rightOps    = ops $ makeOp2 AssocRight "&infix:" App
noneOps     = ops $ makeOp2 AssocNone "&infix:" App
listOps     = leftOps
chainOps    = leftOps
leftSyn     = ops $ makeOp2 AssocLeft "&infix:" Syn
rightSyn    = ops $ makeOp2 AssocRight "&infix:" Syn

-- chainOps    = ops $ makeOpChained

makeOp1 prec sigil con name = prec $ do
    reservedOp name
    return $ \x -> con (sigil ++ name) [x]

makeOp2 prec sigil con name = (`Infix` prec) $ do
    reservedOp name
    return $ \x y -> con (sigil ++ name) [x,y]

parseTerm = parseDecl
    <|> parseVar
    <|> parseLit
    <|> do
        cs <- parens parseOp
        return $ Parens cs
    <|> parseApply
{-
    <|> do
        cs <- parseOp
        return cs
-}
--  <|> nonTerm
    <?> "term"

buildSub body = VSub $ Sub
    { subType       = SubRoutine
    , subAssoc      = "pre"
    , subParams     = ["*List"]
    , subReturns    = "Any"
    , subFun        = body
    }

parseDecl = lexeme $ do
    lexeme (string "sub")
    pos     <- getPosition
    name    <- identifier
    body    <- braces parseOp
    return $ Syn "&infix:::=" [Var ('&':name) pos, Val (buildSub body)]

maybeParens p = choice [ parens p, p ]

parseApply = lexeme $ do
    name    <- identifier
    args    <- maybeParens $ parseTerm `sepBy` (lexeme $ char ',')
    return $ App ('&':name) args

parseVar = lexeme $ do
    pos     <- getPosition
    sigil   <- oneOf "$@%&"
    name    <- many1 (alphaNum <|> char '_')
    return $ Var (sigil:name) pos

nonTerm = do
    pos <- getPosition
    return $ NonTerm pos

parseLit = choice
    [ numLiteral
    , strLiteral
    , arrayLiteral
    , namedLiteral "undef"  VUndef
    , namedLiteral "NaN"    (VNum $ 0/0)
    , namedLiteral "Inf"    (VNum $ 1/0)
    ]

arrayLiteral = do
    items <- brackets $ parseOp `sepBy` (lexeme $ char ',')
    return $ App "&prefix:\\" [(Parens $ foldl app (Val $ VList []) items)]
    where
    app :: Exp -> Exp -> Exp
    app x y = App "&infix:," [x, y]

numLiteral = do
    n <- naturalOrRat  
    case n of
        Left  i -> return . Val $ VInt i
        Right d -> return . Val $ VRat d

strLiteral = return . Val . VStr =<< stringLiteral

namedLiteral n v = do
    lexeme (string n)
    return $ Val v

op_methodPostfix    = []
op_namedUnary       = []
methOps _ = []
primitiveUnaryFunctions = []
ternOps _ = []

-- runLex :: Show a => StateParser a -> String -> IO ()
runLex f p input
        = runParse f (do{ whiteSpace
                 ; x <- p
                 ; eof
                 ; return x
                 }) input

-- run :: Show a => StateParser a -> String -> IO ()
runParse f p input
        = case ( runParser p () "" input ) of
            Left err -> do{ putStr "parse error at "
                          ; print err
                          }
            Right x  -> f x

