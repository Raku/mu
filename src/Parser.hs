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
import Help
import Lexer

-- Lexical units --------------------------------------------------

ruleProgram :: RuleParser Env
ruleProgram = rule "program" $ do
    whiteSpace
    many (symbol ";")
    statements <- option [] ruleStatementList
    many (symbol ";")
    eof
    env <- getState
    return $ env { envBody = Syn ";" statements }

ruleBlock :: RuleParser Exp
ruleBlock = rule "block" $ braces $ do
    whiteSpace
    many (symbol ";")
    statements <- option [] ruleStatementList
    many (symbol ";")
    retSyn ";" statements

ruleStatementList :: RuleParser [Exp]
ruleStatementList = rule "statements" $ choice
    [ nonSep  ruleDeclaration
    , nonSep  ruleConstruct
    , semiSep ruleExpression
    ]
    where
    nonSep = doSep many
    semiSep = doSep many1
    doSep count rule = do
        statement   <- rule
        rest        <- option [] $ try $ do { count (symbol ";"); ruleStatementList }
        return (statement:rest)

-- Declarations ------------------------------------------------

ruleDeclaration :: RuleParser Exp
ruleDeclaration = rule "declaration" $ choice
    [ ruleSubDeclaration
    , rulePackageDeclaration
    , ruleVarDeclaration
    , ruleUseDeclaration
    ]

ruleSubHead :: RuleParser (Bool, String)
ruleSubHead = rule "subroutine head" $ do
    multi   <- option False $ do { symbol "multi" ; return True }
    symbol "sub"
    name    <- ruleSubName
    return (multi, name)

ruleSubScopedWithContext = rule "scoped subroutine with context" $ do
    scope   <- ruleScope
    cxt     <- identifier
    (multi, name) <- ruleSubHead
    return (scope, cxt, multi, name)

ruleSubScoped = rule "scoped subroutine" $ do
    scope <- ruleScope
    (multi, name) <- ruleSubHead
    return (scope, "Any", multi, name)

ruleSubGlobal = rule "global subroutine" $ do
    (multi, name) <- ruleSubHead
    return (SGlobal, "Any", multi, name)

ruleSubDeclaration :: RuleParser Exp
ruleSubDeclaration = rule "subroutine declaration" $ do
    (scope, cxt1, multi, name) <- tryChoice
        [ ruleSubScopedWithContext
        , ruleSubScoped
        , ruleSubGlobal
        ]
    cxt2    <- option cxt1 $ ruleBareTrait "returns"
    formal  <- option Nothing $ return . Just =<< parens ruleSubParameters
    body    <- ruleBlock
    let (fun, names) = extract (body, [])
        params = (maybe [] id formal) ++ map nameToParam names
    -- Check for placeholder vs formal parameters
    unless (isNothing formal || null names || names == ["$_"] ) $
        fail "Cannot mix placeholder variables with formal parameters"
    let sub = Sub { isMulti       = multi
                  , subName       = name
                  , subPad        = []
                  , subType       = SubRoutine
                  , subAssoc      = "pre"
                  , subReturns    = cxt2
                  , subParams     = if null params then [defaultArrayParam] else params
                  , subFun        = fun
                  }
    -- XXX: user-defined infix operator
    return $ Syn "sym" [Sym $ Symbol scope name (Val $ VSub sub)]

ruleSubName = rule "subroutine name" $ do
    star    <- option "" $ string "*"
    fixity  <- option "prefix:" $ choice (map (try . string) $ words fixities)
    c       <- wordAlpha
    cs      <- many wordAny
    return $ "&" ++ star ++ fixity ++ (c:cs)
    where
    fixities = " prefix: postfix: infix: circumfix: "

ruleSubParameters = rule "subroutine parameters" $ do
    (invs:args:_) <- ruleParamList ruleFormalParam
    return $ map setInv invs ++ args
    where
    setInv e = e { isInvocant = True }

ruleParamList parse = rule "parameter list" $ do
    formal <- maybeParens ((parse `sepEndBy` symbol ",") `sepEndBy` symbol ":")
    case formal of
        []                  -> return [[], []]
        [args]              -> return [[], args]
        [invocants,args]    -> return formal
        _                   -> fail "Only one invocant list allowed"

ruleFormalParam = rule "formal parameter" $ do
    cxt     <- option "" $ ruleContext
    sigil   <- option "" $ choice . map symbol $ words " ? * + ++ "
    name    <- ruleVarName
    let required = (sigil /=) `all` ["?", "+"]
    exp     <- ruleParamDefault required
    return $ buildParam cxt sigil name exp

ruleParamDefault True  = return $ Val VUndef
ruleParamDefault False = rule "default value" $ option (Val VUndef) $ do
    symbol "="
    ruleExpression

ruleVarDeclaration :: RuleParser Exp
ruleVarDeclaration = rule "variable declaration" $ do
    scope   <- ruleScope
    name    <- parseVarName
    exp     <- option (Syn "mval" [Var name, Val VUndef]) $ do
        sym <- tryChoice $ map symbol $ words " = := ::= "
        exp <- ruleExpression
        return $ case sym of
            "=" -> (Syn "mval" [Var name, exp])
            _   -> exp
    return $ Syn "sym" [Sym $ Symbol scope name exp]

ruleUseDeclaration :: RuleParser Exp
ruleUseDeclaration = rule "use declaration" $ do
    symbol "use"
    option ' ' $ char 'v'
    version <- many1 (choice [ digit, char '.' ])
    when (version > versnum) $ do
        pos <- getPosition
        error $ "Perl v" ++ version ++ " required--this is only v" ++ versnum ++ ", stopped at " ++ (show pos)
    return $ Val VUndef

rulePackageDeclaration = rule "package declaration" $ fail ""

-- Constructs ------------------------------------------------

ruleConstruct = rule "construct" $ tryChoice
    [ ruleGatherConstruct
    , ruleLoopConstruct
    ]

ruleGatherConstruct = rule "gather construct" $ do
    symbol "gather"
    block <- ruleBlock
    retSyn "gather" [block]

ruleLoopConstruct = rule "loop construct" $ do
    symbol "loop"
    conds <- option [] $ maybeParens $ try $ do
        a <- ruleExpression
        symbol ";"
        b <- ruleExpression
        symbol ";"
        c <- ruleExpression
        return [a,b,c]
    block <- ruleBlock
    -- XXX while/until
    retSyn "loop" (conds ++ [block])

ruleCondConstruct = rule "conditional construct" $ fail ""
ruleWhileUntilConstruct = rule "while/until construct" $ fail ""
ruleForConstruct = rule "for construct" $ fail ""
ruleGivenConstruct = rule "given construct" $ fail ""

-- Expressions ------------------------------------------------

ruleExpression = (<?> "expression") $ parseOp

ruleBlockLiteral = rule "block construct" $ do
    (typ, formal) <- option (SubBlock, Nothing) $ choice
        [ ruleBlockFormalStandard
        , ruleBlockFormalPointy
        ]
    body <- ruleBlock
    let (fun, names) = extract (body, [])
        params = (maybe [] id formal) ++ map nameToParam names
    -- Check for placeholder vs formal parameters
    unless (isNothing formal || null names || names == ["$_"] ) $
        fail "Cannot mix placeholder variables with formal parameters"
    let sub = Sub { isMulti       = False
                  , subName       = "<anon>"
                  , subPad        = []
                  , subType       = typ
                  , subAssoc      = "pre"
                  , subReturns    = "Any"
                  , subParams     = if null params then [defaultArrayParam] else params
                  , subFun        = fun
                  }
    return (Val $ VSub sub)

ruleBlockFormalStandard = rule "standard block parameters" $ do
    symbol "sub"
    params <- option Nothing $ return . Just =<< parens ruleSubParameters
    return $ (SubRoutine, params)

ruleBlockFormalPointy = rule "pointy block parameters" $ do
    symbol "->"
    params <- ruleSubParameters
    return $ (SubBlock, Just params)



















-- Not yet transcribed ------------------------------------------------

tightOperators =
    [ methOps  " . .+ .? .* .+ .() .[] .{} .<<>> .= "   -- Method postfix
    , postOps  " ++ -- "                                -- Auto-Increment
    , rightOps " ** "                                   -- Exponentiation
--  , preOps   " ! + - ~ ? * ** +^ ~^ ?^ \\ "           -- Symbolic Unary
    , preOps   " ! + ~ ? * ** +^ ~^ ?^ \\ "             -- Symbolic Unary
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
    , leftOps  " && !! "                                -- Tight And
    , leftOps  " || ^^ // "                             -- Tight Or
    , ternOps  [("??", "::")]                           -- Ternary
    , rightSyn " = := ::= += **= xx= "                  -- Assignment
    ]

looseOperators =
    [ preOps   primitiveListFunctions                   -- List Operator
    , leftOps  " ==> "                                  -- Pipe Forward
    , leftOps  " and nor "                              -- Loose And
    , leftOps  " or xor err "                           -- Loose Or
    ]

operators = concat $
    [ tightOperators
    , [ listSyn  " , " ]                                -- Comma
    , looseOperators
--  , [ listSyn  " ; " ]                                -- Terminator
    ]

litOperators = tightOperators ++ looseOperators

primitiveListFunctions = " not <== any all one none perl eval "

parseOp = buildExpressionParser operators parseTerm
parseLitOp = buildExpressionParser litOperators parseLitTerm

ops f s = [f n | n <- words s]

doApp str args = App str args []

preOps      = ops $ makeOp1 Prefix "&prefix:" doApp
postOps     = ops $ makeOp1 Postfix "&postfix:" doApp
leftOps     = ops $ makeOp2 AssocLeft "&infix:" doApp
rightOps    = ops $ makeOp2 AssocRight "&infix:" doApp
noneOps     = ops $ makeOp2 AssocNone "&infix:" doApp
listOps     = leftOps
chainOps    = leftOps
leftSyn     = ops $ makeOp2 AssocLeft "" Syn
rightSyn    = ops $ makeOp2 AssocRight "" Syn
listSyn     = leftSyn
chainSyn    = leftSyn

-- chainOps    = ops $ makeOpChained

makeOp1 prec sigil con name = prec $ do
    symbol name
    return $ \x -> con (sigil ++ name) [x]

makeOp2 prec sigil con name = (`Infix` prec) $ do
    symbol name
    return $ \x y -> con (sigil ++ name) [x,y]

parseParens parse = do
    cs  <- parens parse
    return $ Parens cs

parseTerm = rule "term" $ do
    term <- choice
        [ parseVar
        , parseLit
        , parseApply
        , parseParens parseOp
        ]
    inv <- option id rulePostTerm
    return $ inv term

rulePostTerm = rule "term postfix" $ do
    inv <- tryChoice
        [ ruleInvocation
        , ruleArraySubscript
        , ruleHashSubscript
        , ruleCodeSubscript
        ]
    inv' <- option id rulePostTerm
    return $ inv' . inv

ruleInvocation = tryRule "invocation" $ do
    char '.'
    (App name invs args) <- parseApply
    return $ \x -> App name (x:invs) args

ruleArraySubscript = tryRule "array subscript" $ do
    option ' ' $ char '.'
    exp <- brackets ruleExpression
    return $ \x -> Syn "[]" [x, exp]

ruleHashSubscript = tryRule "hash subscript" $ do
    option ' ' $ char '.'
    exp <- braces ruleExpression
    return $ \x -> Syn "{}" [x, exp]

ruleCodeSubscript = tryRule "code subscript" $ do
    option ' ' $ char '.'
    (invs:args:_) <- parens $ parseParamList parseLitTerm
    return $ \x -> Syn "()" [x, Syn "invs" invs, Syn "args" args]

parseLitTerm = choice
    [ parseVar
    , parseLit
    , parseApply
    , parseParens parseLitOp
    ]
    <?> "argument"

subNameWithPrefix prefix = (<?> "subroutine name") $ lexeme $ try $ do
    star    <- option "" $ string "*"
    c       <- wordAlpha
    cs      <- many wordAny
    return $ "&" ++ star ++ prefix ++ (c:cs)

parseApply = lexeme $ do
    name            <- subNameWithPrefix "prefix:"
    (invs:args:_)   <- maybeParens $ parseParamList parseLitTerm
    return $ App name invs args

parseParamList parse = do
    formal <- maybeParens ((parse `sepEndBy` symbol ",") `sepEndBy` symbol ":")
    case formal of
        []                  -> return [[], []]
        [args]              -> return [[], args]
        [invocants,args]    -> return formal
        _                   -> fail "Only one invocant list allowed"

nameToParam :: String -> Param
nameToParam name = Param
    { isInvocant    = False
    , isSlurpy      = (name == "$_")
    , isOptional    = False
    , isNamed       = False
    , paramName     = name
    , paramContext  = (if name == "$_" then "List" else "Scalar")
    , paramDefault  = Val VUndef
    }

maybeParens p = choice [ parens p, p ]

parseVarName = lexeme $ do
    sigil   <- oneOf "$@%&"
    caret   <- option "" $ choice [ string "^", string "*" ]
    name    <- many1 (choice [ wordAny, char ':' ])
    return $ if sigil == '&' && not (':' `elem` name)
        then (sigil:caret) ++ "prefix:" ++ name
        else (sigil:caret) ++ name

parseVar = do
    name    <- parseVarName
    return $ Var name

nonTerm = do
    pos <- getPosition
    return $ NonTerm pos

parseLit = choice
    [ ruleBlockLiteral
    , numLiteral
    , strLiteral
    , arrayLiteral
--  , pairLiteral
    , namedLiteral "undef"  VUndef
    , namedLiteral "NaN"    (VNum $ 0/0)
    , namedLiteral "Inf"    (VNum $ 1/0)
    , dotdotdotLiteral
    , angleLiteral
    ]

angleLiteral = try $ do
    exp <- angles $ option Nothing $ return . Just =<< parseTerm
    return $ case exp of
        Nothing  -> App "&prefix:<>" [] []
        Just exp -> App "&prefix:<>" [] [exp]

numLiteral = do
    n <- naturalOrRat  
    case n of
        Left  i -> return . Val $ VInt i
        Right d -> return . Val $ VRat d

strLiteral = return . Val . VStr =<< stringLiteral

arrayLiteral = do
    items <- brackets $ parseOp `sepEndBy` symbol ","
    return $ App "&prefix:\\" [] [Syn "," items]

pairLiteral = do
    key <- identifier
    symbol "=>"
    val <- parseTerm
    return $ Syn "=>" [Val (VStr key), val]

namedLiteral n v = do { symbol n; return $ Val v }

dotdotdotLiteral = do
    pos <- getPosition
    symbol "..."
    return . Val $ VError "..." (NonTerm pos)

op_methodPostfix    = []
op_namedUnary       = []
methOps _ = []
primitiveUnaryFunctions = []
ternOps _ = []

parseProgram = do { whiteSpace ; x <- parseOp ; eof ; return x }

runRule :: Env -> (Env -> a) -> RuleParser Env -> String -> a
runRule env f p str = f $ case ( runParser ruleProgram env progName str ) of
    Left err    -> env { envBody = Val $ VError (showErr err) (NonTerm $ errorPos err) }
    Right env'  -> env'
    where
    progName
        | Just Symbol{ symExp = Val (VStr str) } <- find ((== "$*PROGNAME") . symName) $ envGlobal env
        = str
        | otherwise
        = "-"

showErr err = 
      showErrorMessages "or" "unknown parse error"
                        "expecting" "unexpected" "end of input"
                       (errorMessages err)

retSyn :: String -> [Exp] -> RuleParser Exp
retSyn sym args = do
    return $ Syn sym args

