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
    , leftOps  " * / % x xx +& +< +> ~& ~< ~> "     -- Multiplicative
    , leftOps  " + - ~ +| +^ ~| ~^ "                    -- Additive
    , leftOps  " & ! "                                  -- Junctive And
    , leftOps  " ^ | "                                  -- Junctive Or
    , preOps   primitiveUnaryFunctions                  -- Name Unary
    , leftOps  " => but does cmp <=> .. ^.. ..^ ^..^ "  -- Non-chaining Binary
      ++ postOps "..."
    , cmpOps $ " != == < <= > >= ~~ !~ " ++
               " eq ne lt le gt ge =:= "                -- Chained Binary
    , leftOps  " && "                                   -- Tight And
    , leftOps  " || ^^ // "                             -- Tight Or
    , ternOps  [("??", "::")]                           -- Ternary
    , leftOps  " = := ::= += **= xx= "                  -- Assignment
    -- XXX rewrite chained Ops using sepBy!
    , rightOps " , "                                    -- List Item Separator
    , preOps   primitiveListFunctions                   -- List Operator
    , leftOps  " ==> "                                  -- Pipe Forward
    , leftOps  " and "                                  -- Loose And
    , leftOps  " or xor err "                           -- Loose Or
    , leftOps  " ; "                                    -- Terminator
    ]

primitiveListFunctions = " not "

parseExp = parseOp

parseOp = buildExpressionParser operators parseTerm

ops f s = [f n | n <- words s]

makeOp op name = do
    reservedOp name
    return $ op name

leftOps     = ops left
rightOps    = ops right
cmpOps      = ops cmp
postOps     = ops postfix
preOps      = ops prefix

left name   = Infix (makeOp Op2 name) AssocLeft
right name  = Infix (makeOp Op2 name) AssocRight
cmp name    = Infix (makeOp OpCmp name) AssocLeft
prefix      = Prefix . makeOp Op1
postfix     = Postfix . makeOp Op1

parseTerm = parens parseOp
    <|> parseLit
--  <|> nonTerm
    <?> "term"

nonTerm = do
    pos <- getPosition
    return $ NonTerm pos

parseLit = choice
    [ numLiteral
    , strLiteral
    , namedLiteral "undef"  VUndef
    , namedLiteral "NaN"    (VNum $ 0/0)
    , namedLiteral "Inf"    (VNum $ 1/0)
    ]

numLiteral = do
    n <- naturalOrFloat  
    case n of
        Left  i -> return . Val $ VInt i
        Right d -> return . Val $ VNum d

strLiteral = return . Val . VStr =<< stringLiteral

namedLiteral n v = do
    lexeme (string n)
    return $ Val v

op_methodPostfix    = []
op_namedUnary       = []
methOps _ = []
primitiveUnaryFunctions = []
ternOps _ = []
listOps _ = []

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

