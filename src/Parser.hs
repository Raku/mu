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
import Rule
import Rule.Error

-- Lexical units --------------------------------------------------

ruleProgram :: RuleParser Env
ruleProgram = rule "program" $ do
    whiteSpace
    many (symbol ";")
    statements <- option [] ruleStatementList
    many (symbol ";")
    eof
    env <- getState
    return $ env { envBody = Statements statements }

ruleBlock :: RuleParser Exp
ruleBlock = rule "block" $ braces $ do
    whiteSpace
    many (symbol ";")
    statements <- option [] ruleStatementList
    many (symbol ";")
    return $ Statements statements

ruleStatementList :: RuleParser [(Exp, SourcePos)]
ruleStatementList = rule "statements" $ choice
    [ nonSep  ruleDeclaration
    , nonSep  ruleConstruct
    , semiSep ruleExpression
    , ruleEndMarker
    ]
    where
    nonSep = doSep many
    semiSep = doSep many1
    doSep count rule = do
        pos         <- getPosition
        statement   <- rule
        rest        <- option [] $ try $ do { count (symbol ";"); ruleStatementList }
        return ((statement, pos):rest)

ruleEndMarker = rule "END marker" $ do
    string "\n=begin END\n"
    many anyChar
    return []
    
-- Declarations ------------------------------------------------

ruleDeclaration :: RuleParser Exp
ruleDeclaration = rule "declaration" $ choice
    [ ruleSubDeclaration
    , ruleModuleDeclaration
    , ruleVarDeclaration
    , ruleUseDeclaration
    , ruleRequireDeclaration
    , ruleClosureTrait -- ???
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
    formal  <- option Nothing $ return . Just =<< parens ruleSubParameters
    cxt2    <- option cxt1 $ try $ ruleBareTrait "returns"
    traits  <- many $ ruleTrait
    body    <- ruleBlock
    let (fun, names) = extract (body, [])
        params = map nameToParam names ++ (maybe [defaultArrayParam] id formal) 
    -- Check for placeholder vs formal parameters
    unless (isNothing formal || null names || names == ["$_"] ) $
        fail "Cannot mix placeholder variables with formal parameters"
    let sub = Sub { isMulti       = multi
                  , subName       = name
                  , subPad        = []
                  , subType       = SubRoutine
                  , subAssoc      = "pre"
                  , subReturns    = cxt2
                  , subParams     = params
                  , subFun        = fun
                  }
    -- XXX: user-defined infix operator
    return $ Syn "sym" [Sym $ Symbol scope name (Syn "sub" [Val $ VSub sub])]

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
    name    <- ruleVarName -- XXX support *[...]
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
    names   <- choice $
        [ return . (:[]) =<< parseVarName
        , parens $ parseVarName `sepEndBy` symbol ","
        ]
    let name = head names -- XXX Wrong
    exp     <- option (Syn "mval" [Var name, Val VUndef]) $ do
        sym <- tryChoice $ map symbol $ words " = := ::= "
        exp <- ruleExpression
        return $ case sym of
            "=" -> (Syn "mval" [Var name, exp])
            _   -> exp
    return $ Syn "sym" [Sym $ Symbol scope name exp | name <- names]

ruleUseDeclaration :: RuleParser Exp
ruleUseDeclaration = rule "use declaration" $ do
    symbol "use"
    tryChoice [ ruleUseVersion, ruleUsePackage ]

ruleUseVersion = rule "use version" $ do
    option ' ' $ char 'v'
    version <- many1 (choice [ digit, char '.' ])
    when (version > versnum) $ do
        pos <- getPosition
        error $ "Perl v" ++ version ++ " required--this is only v" ++ versnum ++ ", stopped at " ++ (show pos)
    return $ Val VUndef

ruleUsePackage = rule "use package" $ do
    package <- identifier -- XXX - ::
    return $ Val VUndef

ruleRequireDeclaration = tryRule "require declaration" $ do
    symbol "require"
    names <- identifier `sepBy1` string "::"
    return $ App "&prefix:require" [] [Val . VStr $ concat (intersperse "/" names) ++ ".pm"]

ruleModuleDeclaration = rule "module declaration" $ do
    symbol "module"
    name <- identifier
    version <- option "" $ do
        char '-'
        many1 (choice [ digit, char '.' ])
    return $ Val VUndef -- XXX

ruleClosureTrait = rule "closure trait" $ do
    name    <- tryChoice $ map symbol $ words " END "
    block   <- ruleBlock
    let (fun, names) = extract (block, [])
    -- Check for placeholder vs formal parameters
    unless (null names) $
        fail "Closure traits takes no formal parameters"
    let sub = Sub { isMulti       = False
                  , subName       = name
                  , subPad        = []
                  , subType       = SubBlock
                  , subAssoc      = "pre"
                  , subReturns    = "Any"
                  , subParams     = []
                  , subFun        = fun
                  }
    return $ App "&prefix:unshift" [] [Syn "," [Var "@*END", Syn "sub" [Val $ VSub sub]]]

rulePackageDeclaration = rule "package declaration" $ fail ""

-- Constructs ------------------------------------------------

ruleConstruct = rule "construct" $ tryChoice
    [ ruleGatherConstruct
    , ruleForeachConstruct
    , ruleLoopConstruct
    , ruleCondConstruct
    , ruleWhileUntilConstruct
    ]

ruleGatherConstruct = rule "gather construct" $ do
    symbol "gather"
    block <- ruleBlock
    retSyn "gather" [block]

ruleForeachConstruct = rule "foreach construct" $ do
    choice [ symbol "for", symbol "foreach" ]
    list <- maybeParens $ ruleExpression
    -- error $ show list
    block <- ruleBlockLiteral
    retSyn "for" [list, block]

ruleLoopConstruct = rule "loop construct" $ do
    symbol "loop"
    conds <- option [] $ maybeParens $ try $ do
        a <- option (Val VUndef) $ ruleExpression
        symbol ";"
        b <- option (Val VUndef) $ ruleExpression
        symbol ";"
        c <- option (Val VUndef) $ ruleExpression
        return [a,b,c]
    block <- ruleBlock
    -- XXX while/until
    retSyn "loop" (conds ++ [block])

ruleCondConstruct = rule "conditional construct" $ do
    csym <- choice [ symbol "if", symbol "unless" ]
    cond <- maybeParens $ ruleExpression
    body <- ruleBlock
    bodyElse <- option (Val VUndef) $ do
        symbol "else"
        ruleBlock
    retSyn csym [cond, body, bodyElse]

ruleWhileUntilConstruct = rule "while/until construct" $ do
    sym <- choice [ symbol "while", symbol "until" ]
    cond <- maybeParens $ ruleExpression
    body <- ruleBlock
    retSyn sym [ cond, body ]

ruleForConstruct = rule "for construct" $ fail ""
ruleGivenConstruct = rule "given construct" $ fail ""

-- Expressions ------------------------------------------------

ruleExpression = (<?> "expression") $ do
    exp <- parseOp
    f <- option id $ choice
        [ rulePostConditional
        , rulePostTernary
        ]
    return $ f exp

rulePostTernary = rule "ternary conditional" $ do
    symbol "??"
    body <- parseOp
    symbol "::"
    bodyElse <- parseOp
    return $ \x -> Syn "if" [x, body, bodyElse]

rulePostConditional = rule "postfix conditional" $ do
    cond <- tryChoice $ map symbol ["if", "unless", "while", "until"]
    exp <- parseOp
    return $ \x -> Syn cond [exp, x, Val VUndef]

ruleBlockLiteral = rule "block construct" $ do
    (typ, formal) <- option (SubBlock, Nothing) $ choice
        [ ruleBlockFormalPointy
        , ruleBlockFormalStandard
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
    return (Syn "sub" [Val $ VSub sub])

ruleBlockFormalStandard = rule "standard block parameters" $ do
    symbol "sub"
    params <- option Nothing $ return . Just =<< parens ruleSubParameters
    return $ (SubRoutine, params)

ruleBlockFormalPointy = rule "pointy block parameters" $ do
    symbol "->"
    params <- ruleSubParameters
    --- XXX -- need to disambiguate between parenful and nonparenful
    --      -- hacking maybeParens is the way to go.
    return $ (SubBlock, if null params then Nothing else Just params)



















-- Not yet transcribed ------------------------------------------------

tightOperators = do
  unary <- currentUnaryFunctions
  return $
    [ methOps  " . .+ .? .* .+ .() .[] .{} .<<>> .= "   -- Method postfix
    , postOps  " ++ -- " ++ preOps " ++ -- "            -- Auto-Increment
    , rightOps " ** "                                   -- Exponentiation
    , preOps   " ! + - ~ ? * ** +^ ~^ ?^ \\ "           -- Symbolic Unary
    , leftOps  " * / % x xx +& +< +> ~& ~< ~> "         -- Multiplicative
    , leftOps  " + - ~ +| +^ ~| ~^ "                    -- Additive
    , leftOps  " & ! "                                  -- Junctive And
    , leftOps  " ^ | "                                  -- Junctive Or
    , preOps   unary                                    -- Named Unary
    , noneOps  " but does "                             -- Traits
      ++ rightOps " => "                                -- Pair constructor
      ++ noneOps " cmp <=> .. ^.. ..^ ^..^ "            -- Non-chaining Binary
      ++ postOps "..."                                  -- Infinite range
    , chainOps $
               " != == < <= > >= ~~ !~ " ++
               " eq ne lt le gt ge =:= "                -- Chained Binary
    , leftOps  " && !! "                                -- Tight And
    , leftOps  " || ^^ // "                             -- Tight Or
    , ternOps  [("??", "::")]                           -- Ternary
    , rightSyn " = := ::= += **= xx= ||= &&= //= "      -- Assignment
    ]

looseOperators = do
    names <- currentListFunctions
    return $
        [ preOps   names                                -- List Operator
        , leftOps  " ==> "                              -- Pipe Forward
        , leftOps  " and nor "                          -- Loose And
        , leftOps  " or xor err "                       -- Loose Or
        ]

operators = do
    tight <- tightOperators
    loose <- looseOperators
    return $ concat $
        [ tight
        , [ listSyn  " , " ]                            -- Comma
        , loose
    --  , [ listSyn  " ; " ]                            -- Terminator
        ]

litOperators = do
    tight <- tightOperators
    loose <- looseOperators
    return $ tight ++ loose

currentFunctions = do
    env     <- getState
    let glob = unsafePerformIO $ readIORef $ envGlobal env
    return (glob ++ envLexical env)

currentUnaryFunctions = do
    funs <- currentFunctions
    return $ unwords [
        name | f@Symbol{ symExp = Val (VSub sub) } <- funs
        , subAssoc sub == "pre"
        , length (subParams sub) == 1
        , isNothing $ find isSlurpy $ subParams sub
        , let (_, (_:name)) = break (== ':') $ symName f
        , name /= "undef" -- XXX Wrong
        ]

currentListFunctions = do
    return []
    funs <- currentFunctions
    return $ unwords [
        name | f@Symbol{ symExp = Val (VSub sub) } <- funs
        , subAssoc sub == "pre"
        , isJust $ find isSlurpy $ subParams sub
        , let (_, (_:name)) = break (== ':') $ symName f
        ]
    -- " not <== any all one none perl eval "

parseOp = do
    ops <- operators
    buildExpressionParser ops parseTerm

ops f s = [f n | n <- sortBy revLength (words s)]
    where
    revLength x y = compare (length y) (length x)

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
    return cs

parseTerm = rule "term" $ do
    term <- choice
        [ parseVar
        , parseLit
        , parseApply
        , parseParens parseOp
        ]
    f <- option id rulePostTerm
    return $ f term

rulePostTerm = rule "term postfix" $ do
    f <- tryChoice
        [ ruleInvocation
        , ruleArraySubscript
        , ruleHashSubscript
        , ruleCodeSubscript
        ]
    f' <- option id rulePostTerm
    return $ f' . f

ruleInvocation = tryRule "invocation" $ do
    char '.'
    (App name invs args) <- parseInvoke
    return $ \x -> App name (x:invs) args
    where
    parseInvoke = lexeme $ do
        name            <- subNameWithPrefix "prefix:"
        (invs:args:_)   <- option [[],[]] $ maybeParens $ parseParamList ruleExpression
        return $ App name invs args
    

ruleArraySubscript = tryRule "array subscript" $ do
    option ' ' $ char '.'
    exp <- brackets ruleExpression
    return $ \x -> Syn "[]" [x, exp]

ruleHashSubscript = tryRule "hash subscript" $ do
    option ' ' $ char '.'
    exp <- subscripts
    return $ \x -> Syn "{}" [x, exp]
        where
            subscripts = do exp <- braces ruleExpression
                            return exp
                         <|> qwLiteral

ruleCodeSubscript = tryRule "code subscript" $ do
    option ' ' $ char '.'
    (invs:args:_) <- parens $ parseParamList ruleExpression
    return $ \x -> Syn "()" [x, Syn "invs" invs, Syn "args" args]

subNameWithPrefix prefix = (<?> "subroutine name") $ lexeme $ try $ do
    star    <- option "" $ string "*"
    c       <- wordAlpha
    cs      <- many wordAny
    return $ "&" ++ star ++ prefix ++ (c:cs)

parseApply = lexeme $ do
    name            <- subNameWithPrefix "prefix:"
    (invs:args:_)   <- maybeDotParens $ parseParamList ruleExpression
    return $ App name invs args

parseParamList parse = do
    formal <- maybeParens ((parse `sepEndBy` symbol ",") `sepEndBy` symbol ":")
    case formal of
        []                  -> return [[], []]
        [args]              -> return [[], unwind args]
        [invocants,args]    -> return [unwind invocants, unwind args]
        _                   -> fail "Only one invocant list allowed"
    where
    unwind :: [Exp] -> [Exp]
    unwind [] = []
    unwind ((Syn "," list):xs) = unwind list ++ unwind xs
    unwind x  = x

nameToParam :: String -> Param
nameToParam name = Param
    { isInvocant    = False
    , isSlurpy      = (name == "$_")
    , isOptional    = False
    , isNamed       = False
    , isLValue      = False
    , paramName     = name
    , paramContext  = cxtOfSigil $ head name
    , paramDefault  = Val VUndef
    }

maybeParens p = choice [ parens p, p ]
maybeDotParens p = choice [ dotParens p, p ]
    where
    dotParens rule = do
        option ' ' $ char '.'
        parens rule

parseVarName = rule "variable name" ruleVarNameString

ruleVarNameString = do
    sigil   <- oneOf "$@%&"
    caret   <- option "" $ choice $ map string $ words " ^ * ? "
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
    , listLiteral
    , arrayLiteral
--  , pairLiteral
    , undefLiteral
--    , namedLiteral "undef"  VUndef
    , namedLiteral "NaN"    (VNum $ 0/0)
    , namedLiteral "Inf"    (VNum $ 1/0)
    , dotdotdotLiteral
    , qqLiteral
    , qwLiteral
    ]

undefLiteral = try $ do
    symbol "undef"
    (invs:args:_)   <- maybeParens $ parseParamList ruleExpression
    return $ if null (invs ++ args)
        then Val VUndef
        else App "&prefix:undef" invs args    

numLiteral = do
    n <- naturalOrRat  
    case n of
        Left  i -> return . Val $ VInt i
        Right d -> return . Val $ VRat d

strLiteral = return . Val . VStr =<< stringLiteral

listLiteral = tryRule "list literal" $ do -- XXX Wrong
    parens whiteSpace
    -- items <- parens $ parseOp `sepEndBy` symbol ","
    return $ Syn "," []

arrayLiteral = do
    items   <- brackets $ parseOp `sepEndBy` symbol ","
    return $ App "&prefix:\\" [] [Syn "," items]

pairLiteral = do
    key <- identifier
    symbol "=>"
    val <- parseTerm
    return $ Syn "=>" [Val (VStr key), val]

qqInterpolator = do 
            var <- ruleVarNameString
            return (Var var)
          <|> do
            char '\\'
            nextchar <- escapeCode -- see Lexer.hs
            return (Val (VStr [nextchar]))
          <|> ruleBlock

qqLiteral = do
    ch <- getDelim
    expr <- interpolatingStringLiteral (balancedDelim ch) qqInterpolator
    ch <- char (balancedDelim ch)
    return expr
        where getDelim = try $ do string "qq"
                                  notFollowedBy alphaNum
                                  delim <- anyChar
                                  return delim
                               <|> char '"'

qwLiteral = try $ do
    str <- qwText
    return $ App "&prefix:\\" [] [Syn "," $ map (Val . VStr) (words str)]
        where qwText = do string "qw"
                          text <- balanced
                          return text
                       <|> angles (many $ satisfy (/= '>'))

namedLiteral n v = do { symbol n; return $ Val v }

dotdotdotLiteral = do
    pos <- getPosition
    symbol "..."
    return . Val $ VError "..." (NonTerm pos)

op_methodPostfix    = []
op_namedUnary       = []
methOps _ = []
ternOps _ = []

runRule :: Env -> (Env -> a) -> RuleParser Env -> FilePath -> String -> a
runRule env f p name str = f $ case ( runParser ruleProgram env name str ) of
    Left err    -> env { envBody = Val $ VError (showErr err) (NonTerm $ errorPos err) }
    Right env'  -> env'

showErr err = 
      showErrorMessages "or" "unknown parse error"
                        "expecting" "unexpected" "end of input"
                       (errorMessages err)

retSyn :: String -> [Exp] -> RuleParser Exp
retSyn sym args = do
    return $ Syn sym args
