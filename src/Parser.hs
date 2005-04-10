{-# OPTIONS_GHC -fglasgow-exts #-}
{-# OPTIONS_GHC -#include "UnicodeC.h" #-}

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
import Types
import Types.Code as Code
import Help
import Lexer
import Rule
import Rule.Expr
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
ruleBlock = lexeme ruleVerbatimBlock

ruleVerbatimBlock :: RuleParser Exp
ruleVerbatimBlock = verbatimRule "block" $ do
    body <- between (symbol "{") (char '}') ruleBlockBody
    retSyn "block" [body]

ruleBlockBody = do
    whiteSpace
    many (symbol ";")
    statements <- option [] ruleStatementList
    many (symbol ";")
    return $ Statements statements

ruleStandaloneBlock = tryRule "standalone block" $ do
    body <- bracesAlone ruleBlockBody
    retBlock SubBlock Nothing body
    where
    bracesAlone p  = between (symbol "{") closingBrace p
    closingBrace = do
        char '}'
        ruleWhiteSpaceLine

ruleStatement = do
    exp <- ruleExpression
    f <- option return $ choice
        [ rulePostConditional
        , rulePostLoop
        , rulePostIterate
        ]
    f exp

ruleStatementList :: RuleParser [(Exp, SourcePos)]
ruleStatementList = rule "statements" $ choice
    [ ruleDocBlock
    , nonSep  ruleBlockDeclaration
    , semiSep ruleDeclaration
    , nonSep  ruleConstruct
    , semiSep ruleStatement
    ]
    where
    nonSep = doSep many
    semiSep = doSep many1
    doSep count rule = do
        whiteSpace
        pos         <- getPosition
        statement   <- rule
        rest        <- option [] $ try $ do { count (symbol ";"); ruleStatementList }
        return ((statement, pos):rest)

ruleBeginOfLine = do
    pos <- getPosition
    unless (sourceColumn pos == 1) $ fail ""
    return ()

ruleDocIntroducer = (<?> "intro") $ do
    ruleBeginOfLine
    char '='

ruleDocCut = (<?> "cut") $ do
    ruleDocIntroducer
    string "cut"
    ruleWhiteSpaceLine
    return ()

ruleDocBlock = verbatimRule "Doc block" $ do
    isEnd <- try $ do
        ruleDocIntroducer
        section <- do
            c <- wordAlpha
            cs <- many $ satisfy (not . isSpace)
            return (c:cs)
        param <- option "" $ do
            satisfy isSpace
            -- XXX: drop trailing spaces?
            many $ satisfy (/= '\n')
        return (section == "begin" && param == "END")
    many1 newline
    if isEnd
        then do
            many anyChar
            return []
        else do
            ruleDocBody
            whiteSpace
            option [] ruleStatementList

ruleDocBody = (try ruleDocCut) <|> eof <|> do
    many $ satisfy  (/= '\n')
    many1 newline -- XXX - paragraph mode
    ruleDocBody
    return ()

ruleQualifiedIdentifier :: RuleParser [String]
ruleQualifiedIdentifier = rule "qualified identifer" $ do
    identifier `sepBy1` (try $ string "::")

-- Declarations ------------------------------------------------

ruleBlockDeclaration :: RuleParser Exp
ruleBlockDeclaration = rule "block declaration" $ choice
    [ ruleSubDeclaration
    , ruleClosureTrait -- ???
    ]

ruleDeclaration :: RuleParser Exp
ruleDeclaration = rule "declaration" $ choice
    [ ruleModuleDeclaration
    , ruleVarDeclaration
    , ruleUseDeclaration
    , ruleInlineDeclaration
    , ruleRequireDeclaration
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
    formal  <- option Nothing $ ruleSubParameters ParensMandatory
    cxt2    <- option cxt1 $ try $ ruleBareTrait "returns"
    _       <- many $ ruleTrait -- traits; not yet used
    body    <- ruleBlock
    let (fun, names) = extract (body, [])
        params = map nameToParam names ++ (maybe [defaultArrayParam] id formal) 
    -- Check for placeholder vs formal parameters
    unless (isNothing formal || null names || names == ["$_"] ) $
        fail "Cannot mix placeholder variables with formal parameters"
    env <- getState
    let subExp = Val . VCode $ Sub
            { isMulti       = multi
            , subName       = name
            , subPad        = envLexical env
            , subType       = SubRoutine
            , subAssoc      = "pre"
            , subReturns    = cxt2
            , subParams     = params
            , subBindings   = []
            , subFun        = fun
            }
    -- XXX: user-defined infix operator
    return $ Sym [SymExp scope name $ Syn "sub" [subExp]]

subNameWithPrefix prefix = (<?> "subroutine name") $ lexeme $ try $ do
    star    <- option "" $ string "*"
    c       <- wordAlpha
    cs      <- many wordAny
    return $ "&" ++ star ++ prefix ++ (c:cs)

ruleSubName = rule "subroutine name" $ do
    star    <- option "" $ string "*"
    fixity  <- option "" $ choice (map (try . string) $ words fixities)
    names   <- identifier `sepBy1` (try $ string "::")
    return $ "&" ++ star ++ fixity ++ concat (intersperse "::" names)
    where
    fixities = " prefix: postfix: infix: circumfix: "

ruleSubParameters :: ParensOption -> RuleParser (Maybe [Param])
ruleSubParameters wantParens = rule "subroutine parameters" $ do
    rv <- ruleParamList wantParens ruleFormalParam
    case rv of
        Just (invs:args:_)  -> return . Just $ map setInv invs ++ args
        _                   -> return Nothing
    where
    setInv e = e { isInvocant = True }

ruleParamList wantParens parse = rule "parameter list" $ do
    (formal, hasParens) <- f $
        ((parse `sepEndBy` symbol ",") `sepEndBy` symbol ":")
    case formal of
        [[]]   -> return $ if hasParens then Just [[], []] else Nothing
        [args] -> return $ Just [[], args]
        [_,_]  -> return $ Just formal
        _      -> fail "Only one invocant list allowed"
    where
    f = case wantParens of
        ParensOptional  -> maybeParensBool
        ParensMandatory -> \x -> do rv <- parens x; return (rv, True)

ruleFormalParam = rule "formal parameter" $ do
    cxt     <- option "" $ ruleContext
    sigil   <- option "" $ choice . map symbol $ words " ? * + ++ "
    name    <- ruleVarName -- XXX support *[...]
    trait   <- option id $ try $ do
        symbol "is"
        symbol "rw"
        return $ \x -> x { isLValue = True }
    let required = (sigil /=) `all` ["?", "+"]
    exp     <- ruleParamDefault required
    return $ trait $ buildParam cxt sigil name exp

ruleParamDefault True  = return $ Val VUndef
ruleParamDefault False = rule "default value" $ option (Val VUndef) $ do
    symbol "="
    parseLitOp

ruleVarDeclaration :: RuleParser Exp
ruleVarDeclaration = rule "variable declaration" $ do
    scope   <- ruleScope
    choice
        [ ruleVarDeclarationSingle scope
        , ruleVarDeclarationMultiple scope ]

ruleVarDeclarationSingle scope = do
    name <- parseVarName
    exp  <- option emptyExp $ do
        sym <- tryChoice $ map string $ words " = := ::= "
        when (sym == "=") $ do
            lookAhead (satisfy (/= '='))
            return ()
        whiteSpace
        ruleExpression
    return $ Sym [SymExp scope name exp]

ruleVarDeclarationMultiple scope = do
    names   <- parens $ parseVarName `sepEndBy` symbol ","
    (sym, expMaybe) <- option ("=", Nothing) $ do
        sym <- tryChoice $ map symbol $ words " = := ::= "
        when (sym == "=") $ do
           lookAhead (satisfy (/= '='))
           return ()
        whiteSpace
        exp <- ruleExpression
        return (sym, Just exp)
    -- now match exps up with names and modify them.
    let doAlign = case sym of { "=" -> alignAssign ; _ -> alignBind }
        syn = Sym $ doAlign scope names []
        lhs = Syn "," $ map Var names
    return $ case expMaybe of
        Just exp -> Syn ";" [syn, Syn sym [lhs, exp]]
        Nothing  -> syn
    where
    mvalSym scope n e = SymExp scope n e
    alignAssign scope names exps = doAssign scope names exps
    doAssign _ [] _ = []
    doAssign scope ns [] =
        map (\n -> mvalSym scope n emptyExp) ns
    doAssign scope (n@('$':_):ns) (e:es) =
        (mvalSym scope n e):(alignAssign scope ns es)
    doAssign scope (n:ns) exps =
        (mvalSym scope n (Syn "," exps)):(alignAssign scope ns [])
    alignBind scope names exps =
        [ SymExp scope name exp
        | name <- names
        | exp  <- exps ++ repeat emptyExp
        ]

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
    return $ Syn "noop" []

{- 
ruleUsePackage = rule "use package" $ do
    _ <- identifier -- package -- XXX - ::
    return $ Syn "noop" []
-}

ruleUsePackage = rule "use package" $ do
    names <- identifier `sepBy1` (try $ string "::")
    _ <- option "" $ do -- version - XXX
        char '-'
        many1 (choice [ digit, char '.' ])
    return $ App "&require" [] [Val . VStr $ concat (intersperse "/" names) ++ ".pm"]

ruleInlineDeclaration = tryRule "inline declaration" $ do
    symbol "inline"
    args <- ruleExpression
    case args of
        App "&infix:=>" [Val key, Val val] [] -> do
            return $ Syn "inline" $ map (Val . VStr . vCast) [key, val]
        _ -> fail "not yet parsed"

ruleRequireDeclaration = tryRule "require declaration" $ do
    symbol "require"
    names <- identifier `sepBy1` (try $ string "::")
    _ <- option "" $ do -- version - XXX
        char '-'
        many1 (choice [ digit, char '.' ])
    return $ App "&require" [] [Val . VStr $ concat (intersperse "/" names) ++ ".pm"]

ruleModuleDeclaration = rule "module declaration" $ do
    symbol "module"
    n <- identifier `sepBy1` (try $ string "::") -- name - XXX
    v <- option "" $ do -- version - XXX
        char '-'
        str <- many1 (choice [ digit, char '.' ])
        return ('-':str)
    a <- option "" $ do -- author - XXX
        char '-'
        str <- many1 (satisfy (/= ';'))
        return ('-':str)
    return $ Syn "module" [Val . VStr $ concat (intersperse "::" n) ++ v ++ a] -- XXX

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
                  , subBindings   = []
                  , subFun        = fun
                  }
    return $ App "&unshift" [Var "@*END"] [Syn "sub" [Val $ VCode sub]]

rulePackageDeclaration = rule "package declaration" $ fail ""

-- Constructs ------------------------------------------------

ruleConstruct = rule "construct" $ tryChoice
    [ ruleGatherConstruct
    , ruleForConstruct
    , ruleLoopConstruct
    , ruleCondConstruct
    , ruleWhileUntilConstruct
    , ruleTryConstruct
    , ruleStandaloneBlock
    , ruleGivenConstruct
    , ruleWhenConstruct
    , ruleDefaultConstruct
    ]

ruleKeywordConsturct keyword = rule (keyword ++ " construct") $ do
    symbol keyword
    block <- ruleBlock
    retSyn keyword [block]

ruleGatherConstruct = ruleKeywordConsturct "gather"

ruleTryConstruct = ruleKeywordConsturct "try"

ruleForConstruct = rule "for construct" $ do
    symbol "for"
    list  <- ruleExpression
    block <- ruleBlockLiteral
    retSyn "for" [list, block]

ruleLoopConstruct = rule "loop construct" $ do
    symbol "loop"
    conds <- option [] $ maybeParens $ try $ do
        a <- option (Syn "noop" []) $ ruleExpression
        symbol ";"
        b <- option (Syn "noop" []) $ ruleExpression
        symbol ";"
        c <- option (Syn "noop" []) $ ruleExpression
        return [a,b,c]
    block <- ruleBlock
    -- XXX while/until
    retSyn "loop" (conds ++ [block])

ruleCondConstruct = rule "conditional construct" $ do
    csym <- choice [ symbol "if", symbol "unless" ]
    ruleCondBody $ csym

ruleCondBody csym = rule "conditional expression" $ do
    cond <- maybeParens $ ruleExpression
    body <- ruleBlock
    bodyElse <- option (Syn "noop" []) $ ruleElseConstruct
    retSyn csym [cond, body, bodyElse]

ruleElseConstruct = rule "else or elsif construct" $
    do
        symbol "else"
        ruleBlock
    <|> do
        symbol "elsif"
        ruleCondBody "if"

ruleWhileUntilConstruct = rule "while/until construct" $ do
    sym <- choice [ symbol "while", symbol "until" ]
    cond <- maybeParens $ ruleExpression
    body <- ruleBlock
    retSyn sym [ cond, body ]

ruleGivenConstruct = rule "given construct" $ do
    sym <- symbol "given"
    topic <- maybeParens $ ruleExpression
    body <- ruleBlock
    retSyn sym [ topic, body ]

ruleWhenConstruct = rule "when construct" $ do
    sym <- symbol "when"
    match <- maybeParens $ ruleExpression
    body <- ruleBlock
    retSyn sym [ match, body ]

-- XXX: make this translate into when true, when smartmatch
-- against true works
ruleDefaultConstruct = rule "default construct" $ do
    sym <- symbol "default"
    body <- ruleBlock
    retSyn sym [ body ]

-- Expressions ------------------------------------------------

ruleExpression = (<?> "expression") $ parseOp

rulePostConditional = rule "postfix conditional" $ do
    cond <- tryChoice $ map symbol ["if", "unless"]
    exp <- ruleExpression
    return $ \body -> retSyn cond [exp, body, Syn "noop" []]

rulePostLoop = rule "postfix loop" $ do
    cond <- tryChoice $ map symbol ["while", "until"]
    exp <- ruleExpression
    return $ \body -> retSyn cond [exp, body]

rulePostIterate = rule "postfix iteration" $ do
    cond <- tryChoice $ map symbol ["for"]
    exp <- ruleExpression
    return $ \body -> do
        block <- retBlock SubBlock Nothing body
        retSyn cond [exp, block]

ruleBlockLiteral = rule "block construct" $ do
    (typ, formal) <- option (SubBlock, Nothing) $ choice
        [ ruleBlockFormalPointy
        , ruleBlockFormalStandard
        ]
    body <- ruleBlock
    retBlock typ formal body

retBlock typ formal body = do
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
                  , subBindings   = []
                  , subFun        = fun
                  }
    return (Syn "sub" [Val $ VCode sub])

ruleBlockFormalStandard = rule "standard block parameters" $ do
    symbol "sub"
    params <- option Nothing $ ruleSubParameters ParensMandatory
    return $ (SubRoutine, params)

ruleBlockFormalPointy = rule "pointy block parameters" $ do
    symbol "->"
    params <- ruleSubParameters ParensOptional
    return $ (SubBlock, params)



















-- Not yet transcribed ------------------------------------------------

tightOperators = do
  (optionary, unary) <- currentUnaryFunctions
  return $
    [ methOps  " . .+ .? .* .+ .() .[] .{} .<<>> .= "   -- Method postfix
    , postOps  " ++ -- " ++ preOps " ++ -- "            -- Auto-Increment
    , rightOps " ** "                                   -- Exponentiation
    , preSyn "* **"
      ++ preOps (concatMap (\x -> " -" ++ [x]) "rwxoRWXOezsfdlpSbctugkTBMAC")
      ++ preOps " = ! + - ~ ? +^ ~^ ?^ \\ "             -- Symbolic Unary
    , leftOps $
               " »*« »/« »x« »xx« " ++
               " >>*<< >>/<< >>x<< >>xx<< " ++
               " * / % x xx +& +< +> ~& ~< ~> "         -- Multiplicative
    , leftOps  " »+« >>+<< + - ~ +| +^ ~| ~^ ?| "       -- Additive
    , listOps  " & ! "                                  -- Junctive And
    , listOps  " ^ | "                                  -- Junctive Or
    , optOps optionary, preOps unary                    -- Named Unary
    , noneOps  " is but does "                          -- Traits
      ++ rightOps " => "                                -- Pair constructor
      ++ noneOps " cmp <=> .. ^.. ..^ ^..^ "            -- Non-chaining Binary
      ++ postOps "..."                                  -- Infinite range
    , chainOps $
               " != == < <= > >= ~~ !~ " ++
               " eq ne lt le gt ge =:= "                -- Chained Binary
    , leftOps  " && !! "                                -- Tight And
    , leftOps  " || ^^ // "                             -- Tight Or
    , [ternOp "??" "::" "if"]                           -- Ternary
    , rightSyn " = := ::= ~= += -= *= /= x= Y= ¥= **= xx= ||= &&= //= "-- Assignment
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
        , [ listSyn  " , ", listOps " Y ¥ " ]           -- Comma
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
    return . mapPair munge . partition fst . sort $
        [ (opt, encodeUTF8 name) | f@(SymVar _ _ (MkRef (ICode code))) <- funs
        , Code.assoc code == "pre"
        , length (Code.params code) == 1
        , let param = head $ Code.params code
        , let opt   = isOptional param
        , let name  = parseName $ symName f
        , name /= "say" && name /= "print" -- XXX: find other MMD duplicates
        , not $ isSlurpy param
        ]
    where
    munge = unwords . map snd
    mapPair f (x, y) = (f x, f y)

parseName str
    | (_, (_:name)) <- break (== ':') str
    = name
    | otherwise
    = dropWhile (not . isAlpha) str


currentListFunctions = do
    return []
{-
    funs <- currentFunctions
    return $ unwords [
        encodeUTF8 name | f@Symbol{ symExp = Val (VCode sub) } <- funs
        , subAssoc sub == "pre"
        , isJust $ find isSlurpy $ subParams sub
        , let name = parseName $ symName f
        ]
    -- " not <== any all one none perl eval "
-}

parseOp = do
    ops <- operators
    buildExpressionParser ops parseTerm (Syn "" [])

parseTightOp = do
    ops <- tightOperators
    buildExpressionParser ops parseTerm (Syn "" [])

parseLitOp = do
    ops <- litOperators
    buildExpressionParser ops parseTerm (Syn "" [])

ops f s = [f n | n <- sortBy revLength (words $ decodeUTF8 s)]
    where
    revLength x y = compare (length y) (length x)

doApp str args = App str args []

preSyn      = ops $ makeOp1 Prefix "" Syn
preOps      = ops $ makeOp1 Prefix "&prefix:" doApp
postOps     = ops $ makeOp1 Postfix "&postfix:" doApp
optOps      = ops $ makeOp1 OptionalPrefix "&prefix:" doApp
leftOps     = ops $ makeOp2 AssocLeft "&infix:" doApp
rightOps    = ops $ makeOp2 AssocRight "&infix:" doApp
noneOps     = ops $ makeOp2 AssocNone "&infix:" doApp
listOps     = leftOps
chainOps    = leftOps
leftSyn     = ops $ makeOp2 AssocLeft "" Syn
rightSyn    = ops $ makeOp2 AssocRight "" Syn
listSyn     = ops $ makeOp0 AssocList "" Syn
chainSyn    = leftSyn

-- chainOps    = ops $ makeOpChained

makeOp1 prec sigil con name = prec $ try $ do
    symbol name
    -- `int(3)+4` should not be parsed as `int((3)+4)`
    when (isWordAny (last name)) $ do
        lookAhead (satisfy (/= '('))
        return ()
    return $ \x -> con fullName $ case x of
        Syn "" []   -> []
        _           -> [x]
    where
    fullName
        | isAlpha (head name)
        , sigil == "&prefix:"
        = ('&':name)
        | otherwise
        = sigil ++ name

makeOp2 prec sigil con name = (`Infix` prec) $ do
    symbol name
    return $ \x y -> con (sigil ++ name) [x,y]

makeOp0 prec sigil con name = (`Infix` prec) $ do
    many1 $ do
        string name
        whiteSpace
    return make
    where
    make x (Syn syn xs) | syn == name = con (sigil ++ name) (x:xs)
    make x (Syn "" []) = con (sigil ++ name) [x]
    make x y = con (sigil ++ name) [x,y]

parseTerm = rule "term" $ do
    term <- choice
        [ ruleVar
        , ruleLit
        , parseApply
        , parens ruleExpression
        ]
    fs <- many rulePostTerm
    return $ foldr (.) id (reverse fs) $ term

rulePostTerm = tryRule "term postfix" $ do
    hasDot <- option False $ do whiteSpace; char '.'; return True
    choice $ (if hasDot then [ruleInvocation] else []) ++
        [ ruleArraySubscript
        , ruleHashSubscript
        , ruleCodeSubscript
        ]

ruleInvocation = tryVerbatimRule "invocation" $ do
    hasEqual <- option False $ do char '='; whiteSpace; return True
    name            <- ruleSubName
    (invs,args)     <- option ([],[]) $ parseParenParamList
    return $ \x -> if hasEqual
        then Syn "=" [x, App name (x:invs) args]
        else App name (x:invs) args

ruleInvocationParens = do
    hasEqual <- option False $ do char '='; whiteSpace; return True
    name            <- ruleSubName
    (invs,args)     <- parens $ parseNoParenParamList
    -- XXX we just append the adverbial block onto the end of the arg list
    -- it really goes into the *& slot if there is one. -lp
    return $ \x -> if hasEqual
        then Syn "=" [x, App name (x:invs) args]
        else App name (x:invs) args

ruleArraySubscript = tryVerbatimRule "array subscript" $ do
    brackets $ option id $ do
        exp <- ruleExpression
        return $ \x -> Syn "[]" [x, exp]

ruleHashSubscript = tryVerbatimRule "hash subscript" $ do
    choice [ ruleHashSubscriptBraces, ruleHashSubscriptQW ]

ruleHashSubscriptBraces = do
    braces $ option id $ do
        exp <- ruleExpression
        return $ \x -> Syn "{}" [x, exp]

ruleHashSubscriptQW = do
    exp <- qwLiteral
    return $ \x -> Syn "{}" [x, exp]

ruleCodeSubscript = tryRule "code subscript" $ do
    (invs,args) <- parens $ parseParamList
    return $ \x -> Syn "()" [x, Syn "invs" invs, Syn "args" args]

parseApply = lexeme $ do
    name            <- ruleSubName
    option ' ' $ char '.'
    (invs,args)   <- parseParamList
    return $ App name invs args

parseParamList = parseParenParamList <|> parseNoParenParamList

parseParenParamList = try $ do
    params <- option Nothing $ do
        return . Just =<< parens parseNoParenParamList
    block       <- option [] ruleAdverb
    when (isNothing params && null block) $ fail ""
    let (inv, norm) = maybe ([], []) id params
    -- XXX we just append the adverbial block onto the end of the arg list
    -- it really goes into the *& slot if there is one. -lp
    processFormals [inv, norm ++ block]

ruleAdverb = tryRule "adverb" $ do 
    char ':'
    rblock <- ruleBlockLiteral
    next <- option [] ruleAdverb
    return (rblock:next)

parseNoParenParamList = do
    formal <- (`sepEndBy` symbol ":") $ fix $ \rec -> do
        rv <- option Nothing $ do
            return . Just =<< choice [ ruleBlockLiteral, ruleExpression ]
        case rv of
            Nothing  -> return []
            Just exp -> do
                let f = case exp of
                        Syn "sub" _ -> optional
                        _ -> (>> return ())
                rest <- option [] $ do { f $ symbol ","; rec }
                return (exp:rest)
    processFormals formal 

processFormals :: Monad m => [[Exp]] -> m ([Exp], [Exp])
processFormals formal = do
    case formal of
        []                  -> return ([], [])
        [args]              -> return ([], unwind args)
        [invocants,args]    -> return (unwind invocants, unwind args)
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
    , isThunk       = False
    , paramName     = name
    , paramContext  = cxtOfSigil $ head name
    , paramDefault  = Val VUndef
    }

maybeParensBool p = choice
    [ do rv <- parens p; return (rv, True)
    , do rv <- p; return (rv, False)
    ]

maybeParens p = choice [ parens p, p ]
maybeDotParens p = choice [ dotParens p, p ]
    where
    dotParens rule = do
        option ' ' $ char '.'
        parens rule

parseVarName = rule "variable name" ruleVarNameString

ruleVarNameString =   try (string "$!")  -- error variable
                  <|> try (string "$/")  -- match object
                  <|> try ruleMatchVar
                  <|> do
    sigil   <- oneOf "$@%&"
    -- ^ placeholder, * global, ? magical, . member, : private member
    caret   <- option "" $ choice $ map string $ words " ^ * ? . : "
    names   <- many1 wordAny `sepBy1` (try $ string "::")
    return $ (sigil:caret) ++ concat (intersperse "::" names)

ruleMatchVar = do
    sigil   <- oneOf "$@%&"
    digits  <- many1 digit
    return $ (sigil:digits)

ruleVar = do
    name    <- ruleVarNameString
    return $ makeVar name

makeVar ('$':rest) | all (`elem` "1234567890") rest =
    Syn "[]" [Var "$/", Val $ VInt $ read rest]
makeVar var = Var var

nonTerm = do
    pos <- getPosition
    return $ NonTerm pos

ruleLit = choice
    [ ruleBlockLiteral
    , numLiteral
    , listLiteral
    , arrayLiteral
    , pairLiteral
    , undefLiteral
--    , namedLiteral "undef"  VUndef
    , namedLiteral "NaN"    (VNum $ 0/0)
    , namedLiteral "Inf"    (VNum $ 1/0)
    , dotdotdotLiteral
    , qLiteral
    , qqLiteral
    , qwLiteral
    , rxLiteral
    , substLiteral
    ]

undefLiteral = try $ do
    symbol "undef"
    (invs,args)   <- maybeParens $ parseParamList
    return $ if null (invs ++ args)
        then Val VUndef
        else App "&undef" invs args    

numLiteral = do
    n <- naturalOrRat  
    case n of
        Left  i -> return . Val $ VInt i
        Right d -> return . Val $ VRat d

listLiteral = tryRule "list literal" $ do -- XXX Wrong
    parens whiteSpace
    -- items <- parens $ parseOp `sepEndBy` symbol ","
    return $ Syn "," []

arrayLiteral = do
    items   <- brackets $ ruleExpression `sepEndBy` symbol ","
    -- return $ App "&prefix:\\" [] [Syn "cxt" [Val (VStr "List"), Syn "," items]]
    return $ Syn "\\[]" [Syn "," items]

pairLiteral = try $
    do
	key <- identifier
	symbol "=>"
	val <- parseTerm
	return $ App "&infix:=>" [Val (VStr key), val] []
    <|>
    do
	string ":"
	key <- many1 wordAny
	val <- option (Val $ VInt 1) valuePart
	return $ App "&infix:=>" [Val (VStr key), val] []
	where
	valuePart =
	    do{ skipMany1 (satisfy isSpace); option (Val $ VInt 1) $ do{ symbol "."; valueExp } }
	    <|>
	    valueExp
	valueExp =
	    parens ruleExpression
	    <|> arrayLiteral
	    <|> qwLiteral
	    -- <|> ruleStandaloneBlock -- :key{ k1 => val }

rxInterpolator end = choice
    [ qqInterpolatorVar end, rxInterpolatorChar, ruleVerbatimBlock ]

qqInterpolator end = choice
    [ qqInterpolatorVar end, qqInterpolatorChar, ruleVerbatimBlock ]

qqInterpolatorVar end = try $ do
    var <- ruleVarNameString
    if (last var == end) then fail "" else return ()
    fs <- if head var == '$'
        then many qqInterpolatorPostTerm
        else many1 qqInterpolatorPostTerm
    return $ foldr (.) id (reverse fs) $ makeVar var

qqInterpolatorPostTerm = try $ do
    option ' ' $ char '.'
    choice
        [ ruleInvocationParens
        , ruleArraySubscript
        , ruleHashSubscript
        , ruleCodeSubscript
        ]

rxInterpolatorChar = do
    char '\\'
    nextchar <- anyChar -- escapeCode -- see Lexer.hs
    return (Val $ VStr ['\\', nextchar])

qqInterpolatorChar = do
    char '\\'
    nextchar <- escapeCode -- see Lexer.hs
    return (Val $ VStr [nextchar])

qqLiteral = do
    ch   <- (getDelim "qq" '"')
    expr <- interpolatingStringLiteral (balancedDelim ch) qqInterpolator
    char (balancedDelim ch)
    return expr

qLiteral = do
    ch   <- (getDelim "q" '\'')
    str  <- (many (try $ quotedDelim (balancedDelim ch) <|> noneOf [ balancedDelim ch ]))
    char (balancedDelim ch)
    return . Val . VStr $ str

getDelim qstr sch = try $
    do
        string qstr
        notFollowedBy alphaNum
        delim <- anyChar
        return delim
    <|> char sch

quotedDelim ch = choice
    [ try $ do { string [ '\\', ch ]; return ch }
    , try $ do { string "\\\\"; return '\\' }
    ]

substLiteral = try $ do
    symbol "s"
    symbol ":perl5"
    isGlobal <- option False $ do { symbol ":g"; return True }
    ch <- anyChar
    let endch = balancedDelim ch
    expr <- interpolatingStringLiteral endch rxInterpolator
    char endch
    ch <- if ch == endch then return ch else do { whiteSpace ; anyChar }
    let endch = balancedDelim ch
    subst <- interpolatingStringLiteral endch qqInterpolator
    char endch
    return $ Syn "subst" [expr, Val $ VBool isGlobal, subst]

rxLiteral = try $ do
    symbol "rx"
    symbol ":perl5"
    isGlobal <- option False $ do { symbol ":g"; return True }
    ch <- anyChar
    expr <- interpolatingStringLiteral (balancedDelim ch) rxInterpolator
    char (balancedDelim ch)
    return $ Syn "rx" [expr, Val $ VBool isGlobal]

qwLiteral = try $ do
    str <- qwText
    return $ case words str of
        []  -> Val (VStr "")
        [x] -> Val (VStr x)
        xs  -> Syn "," $ map (Val . VStr) xs
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

ternOp pre post syn = (`Infix` AssocRight) $ do
    symbol pre
    y <- parseTightOp
    symbol post
    return $ \x z -> Syn syn [x, y, z]

runRule :: Env -> (Env -> a) -> RuleParser Env -> FilePath -> String -> a
runRule env f p name str = f $ case ( runParser p env name str ) of
    Left err    -> env { envBody = Val $ VError (showErr err) (NonTerm $ errorPos err) }
    Right env'  -> env'

showErr err = 
      showErrorMessages "or" "unknown parse error"
                        "expecting" "unexpected" "end of input"
                       (errorMessages err)

retSyn :: String -> [Exp] -> RuleParser Exp
retSyn sym args = do
    return $ Syn sym args

