{-# OPTIONS_GHC -cpp -fglasgow-exts -funbox-strict-fields #-}
{-# OPTIONS_GHC -#include "UnicodeC.h" #-}

{-|
    Higher-level parser for building ASTs.

>   I sang of leaves, of leaves of gold, and leaves of gold there grew:
>   Of wind I sang, a wind there came and in the branches blew.
>   Beyond the Sun, beyond the Moon, the foam was on the Sea,
>   And by the strand of Ilmarin there grew a golden Tree...
-}

module Pugs.Parser where
import Pugs.Internals
import Pugs.AST
import Pugs.Types
import Pugs.Help
import Pugs.Lexer
import Pugs.Rule
import Pugs.Rule.Expr
import Pugs.Rule.Error
import Pugs.Pretty
import qualified Data.Set as Set

-- Lexical units --------------------------------------------------

ruleProgram :: RuleParser Env
ruleProgram = rule "program" $ do
    statements <- ruleBlockBody
    -- error $ show statements
    eof
    env <- getState
    return $ env { envBody = mergeStmts emptyExp statements, envStash = "" }

ruleBlock :: RuleParser Exp
ruleBlock = lexeme ruleVerbatimBlock

ruleVerbatimBlock :: RuleParser Exp
ruleVerbatimBlock = verbatimRule "block" $ do
    body <- between (symbol "{") (char '}') ruleBlockBody
    retSyn "block" [body]

ruleEmptyExp :: RuleParser Exp
ruleEmptyExp = expRule $ do
    symbol ";"
    return emptyExp

expRule :: RuleParser Exp -> RuleParser Exp
expRule rule = do
    pos1 <- getPosition
    exp  <- rule
    pos2 <- getPosition
    return $ Pos (mkPos pos1 pos2) (unwrap exp)

mkPos :: SourcePos -> SourcePos -> Pos
mkPos pos1 pos2 = MkPos
    { posName         = sourceName pos1 
    , posBeginLine    = sourceLine pos1
    , posBeginColumn  = sourceColumn pos1
    , posEndLine      = sourceLine pos2
    , posEndColumn    = sourceColumn pos2
    }

ruleBlockBody :: RuleParser Exp
ruleBlockBody = do
    whiteSpace
    -- pos     <- getPosition
    env     <- getState
    pre     <- many ruleEmptyExp
    body    <- option emptyExp ruleStatementList
    post    <- many ruleEmptyExp
    let body' = foldl mergeStmts (foldl (flip mergeStmts) body pre) post
    env'    <- getState
    setState env'{ envLexical = envLexical env }
    return $ case unwrap body' of
        (Syn "sub" _)   -> mergeStmts emptyExp body'
        _               -> body'

ruleStandaloneBlock :: RuleParser Exp
ruleStandaloneBlock = tryRule "standalone block" $ do
    body <- bracesAlone ruleBlockBody
    retBlock SubBlock Nothing body
    where
    bracesAlone p  = between (symbol "{") closingBrace p
    closingBrace = do
        char '}'
        ruleWhiteSpaceLine

ruleStatement :: RuleParser Exp
ruleStatement = do
    exp <- ruleExpression
    f <- option return $ choice
        [ rulePostConditional
        , rulePostLoop
        , rulePostIterate
        ]
    f exp

ruleStatementList :: RuleParser Exp
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
        -- pos     <- getPosition
        exp     <- rule
        rest    <- option return $ try $ do
            count (symbol ";")
            stmts <- ruleStatementList
            return $ \exp -> return $ mergeStmts exp stmts
        rest exp


-- Stmt is essentially a cons cell
-- Stmt (Stmt ...) is illegal
mergeStmts :: Exp -> Exp -> Exp
mergeStmts (Stmts x1 x2) y = mergeStmts x1 (mergeStmts x2 y)
mergeStmts Noop y@(Stmts _ _) = y
mergeStmts (Sym scope name x) y = Sym scope name (mergeStmts x y)
mergeStmts (Pad scope lex x) y = Pad scope lex (mergeStmts x y)
mergeStmts x@(Pos pos (Syn "sub" [Val (VCode sub)])) y
    | subType sub >= SubBlock =
    -- bare Block in statement level; run it!
    mergeStmts (Pos pos $ App x [] []) y
mergeStmts x y@(Pos pos (Syn "sub" [Val (VCode sub)]))
    | subType sub >= SubBlock =
    -- bare Block in statement level; run it!
    mergeStmts x (Pos pos $ App y [] [])
mergeStmts x (Stmts y Noop) = mergeStmts x y
mergeStmts x (Stmts Noop y) = mergeStmts x y
mergeStmts x y = Stmts x y

ruleBeginOfLine :: RuleParser ()
ruleBeginOfLine = do
    pos <- getPosition
    unless (sourceColumn pos == 1) $ fail ""
    return ()

ruleDocIntroducer :: RuleParser Char
ruleDocIntroducer = (<?> "intro") $ do
    ruleBeginOfLine
    char '='

ruleDocCut :: RuleParser ()
ruleDocCut = (<?> "cut") $ do
    ruleDocIntroducer
    string "cut"
    ruleWhiteSpaceLine
    return ()

ruleDocBlock :: RuleParser Exp
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
    choice [ eof, do { many1 newline; return () } ]
    if isEnd
        then do
            many anyChar
            return emptyExp
        else do
            ruleDocBody
            whiteSpace
            option emptyExp ruleStatementList

ruleDocBody :: RuleParser ()
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
    , ruleClosureTrait False
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

-- | Scope, context, multi, name
ruleSubScopedWithContext :: RuleParser (Scope, String, Bool, String)
ruleSubScopedWithContext = rule "scoped subroutine with context" $ do
    scope   <- ruleScope
    cxt     <- identifier
    (multi, name) <- ruleSubHead
    return (scope, cxt, multi, name)

ruleSubScoped :: RuleParser (Scope, String, Bool, String)
ruleSubScoped = rule "scoped subroutine" $ do
    scope <- ruleScope
    (multi, name) <- ruleSubHead
    return (scope, "Any", multi, name)

ruleSubGlobal :: RuleParser (Scope, String, Bool, String)
ruleSubGlobal = rule "global subroutine" $ do
    (multi, name) <- ruleSubHead
    return (SGlobal, "Any", multi, name)


doExtract :: Maybe [Param] -> Exp -> (Exp, [String], [Param])
doExtract formal body = (fun, names', params)
    where
    (fun, names) = extract body []
    names' | Just params <- formal, any (== "$_") (map paramName params)
           = filter (/= "$_") names
           | otherwise
           = names
    params = map nameToParam (sort names')
        ++ (maybe (if null names' then [defaultArrayParam] else []) id formal)


ruleSubDeclaration :: RuleParser Exp
ruleSubDeclaration = rule "subroutine declaration" $ do
    -- namePos <- getPosition
    (scope, typ, multi, name) <- tryChoice
        [ ruleSubScopedWithContext
        , ruleSubScoped
        , ruleSubGlobal
        ]
    formal  <- option Nothing $ ruleSubParameters ParensMandatory
    typ'    <- option typ $ try $ ruleBareTrait "returns"
    _       <- many $ ruleTrait -- traits; not yet used
    -- bodyPos <- getPosition
    body    <- ruleBlock
    let (fun, names, params) = doExtract formal body
    -- Check for placeholder vs formal parameters
    unless (isNothing formal || null names) $ 
        fail "Cannot mix placeholder variables with formal parameters"
    env <- getState
    let subExp = Val . VCode $ MkCode
            { isMulti       = multi
            , subName       = name
            , subPad        = envLexical env
            , subType       = SubRoutine
            , subAssoc      = "pre"
            , subReturns    = mkType typ'
            , subParams     = params
            , subBindings   = []
            , subSlurpLimit = []
            , subBody       = fun
            }
        -- decl = Sym scope name -- , namePos)
        exp  = Syn ":=" [Var name, Syn "sub" [subExp]] -- , bodyPos)
    -- XXX: user-defined infix operator
    if scope == SGlobal
        then do { unsafeEvalExp (Sym scope name exp); return emptyExp }
        else do
            lexDiff <- unsafeEvalLexDiff (Sym scope name emptyExp)
            return $ Pad scope lexDiff exp

subNameWithPrefix :: String -> RuleParser String
subNameWithPrefix prefix = (<?> "subroutine name") $ lexeme $ try $ do
    star    <- option "" $ string "*"
    c       <- wordAlpha
    cs      <- many wordAny
    return $ "&" ++ star ++ prefix ++ (c:cs)

ruleSubName :: RuleParser String
ruleSubName = verbatimRule "subroutine name" $ do
    star    <- option "" $ string "*"
    fixity  <- option "" $ choice (map (try . string) $ words fixities)
    names   <- verbatimIdentifier `sepBy1` (try $ string "::")
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

ruleParamList :: ParensOption -> RuleParser a -> RuleParser (Maybe [[a]])
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

ruleFormalParam :: RuleParser Param
ruleFormalParam = rule "formal parameter" $ do
    cxt     <- option "" $ ruleContext
    sigil   <- option "" $ choice . map symbol $ words " ? * + ++ "
    name    <- ruleVarName -- XXX support *[...]
    traits  <- many ruleTrait
    let required = (sigil /=) `all` ["?", "+"]
    exp     <- ruleParamDefault required
    return $ foldr appTrait (buildParam cxt sigil name exp) traits
    where
    appTrait "rw"   x = x { isWritable = True }
    appTrait "copy" x = x { isLValue = False, isWritable = True }
    appTrait "lazy" x = x { isLazy = True }
    appTrait _      x = x -- error "unknown trait"

ruleParamDefault :: Bool -> RuleParser Exp
ruleParamDefault True  = return $ Val VUndef
ruleParamDefault False = rule "default value" $ option (Val VUndef) $ do
    symbol "="
    parseLitOp

ruleVarDeclaration :: RuleParser Exp
ruleVarDeclaration = rule "variable declaration" $ do
    scope       <- ruleScope
    (decl, lhs) <- choice
        [ do -- pos  <- getPosition
             name <- parseVarName
             return ((Sym scope name), Var name)
        , do names <- parens . (`sepEndBy` symbol ",") $
                parseVarName <|> do { undefLiteral; return "" }
             let mkVar v = if null v then Val undef else Var v
             return (combine (map (Sym scope) names), Syn "," (map mkVar names))
        ]
    -- pos <- getPosition
    (sym, expMaybe) <- option ("=", Nothing) $ do
        sym <- tryChoice $ map string $ words " = := ::= "
        when (sym == "=") $ do
           lookAhead (satisfy (/= '='))
           return ()
        whiteSpace
        exp <- ruleExpression
        return (sym, Just exp)
    lexDiff <- case sym of
        "::="   -> do
            env  <- getState
            env' <- unsafeEvalEnv $ decl (Syn sym [lhs, fromJust expMaybe])
            return $ envLexical env' `diffPads` envLexical env
        _       -> unsafeEvalLexDiff (decl emptyExp)
    let rhs | sym == "::=" = emptyExp
            | otherwise = maybe emptyExp (\exp -> Syn sym [lhs, exp]) expMaybe
    return $ Pad scope lexDiff rhs

ruleUseDeclaration :: RuleParser Exp
ruleUseDeclaration = rule "use declaration" $ do
    symbol "use"
    tryChoice [ ruleUseVersion, ruleUsePackage ]

ruleUseVersion :: RuleParser Exp
ruleUseVersion = rule "use version" $ do
    option ' ' $ char 'v'
    version <- many1 (choice [ digit, char '.' ])
    when (version > versnum) $ do
        pos <- getPosition
        error $ "Perl v" ++ version ++ " required--this is only v" ++ versnum ++ ", stopped at " ++ (show pos)
    return emptyExp

ruleUsePackage :: RuleParser Exp
ruleUsePackage = rule "use package" $ do
    names <- identifier `sepBy1` (try $ string "::")
    _ <- option "" $ do -- version - XXX
        char '-'
        many1 (choice [ digit, char '.' ])
    unsafeEvalExp $ App (Var "&require") [] [Val . VStr $ concat (intersperse "/" names) ++ ".pm"]
    return emptyExp

ruleInlineDeclaration :: RuleParser Exp
ruleInlineDeclaration = tryRule "inline declaration" $ do
    symbol "inline"
    args <- ruleExpression
    case args of
        App (Var "&infix:=>") [Val key, Val val] [] -> do
            return $ Syn "inline" $ map (Val . VStr . vCast) [key, val]
        _ -> fail "not yet parsed"

ruleRequireDeclaration :: RuleParser Exp
ruleRequireDeclaration = tryRule "require declaration" $ do
    symbol "require"
    names <- identifier `sepBy1` (try $ string "::")
    _ <- option "" $ do -- version - XXX
        char '-'
        many1 (choice [ digit, char '.' ])
    return $ App (Var "&require") [] [Val . VStr $ concat (intersperse "/" names) ++ ".pm"]

ruleModuleDeclaration :: RuleParser Exp
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

ruleClosureTrait :: Bool -> RuleParser Exp
ruleClosureTrait rhs = rule "closure trait" $ do
    let names | rhs       = " BEGIN "
              | otherwise = " BEGIN END "
    name    <- tryChoice $ map symbol $ words names
    block   <- ruleBlock
    let (fun, names) = extract block []
    -- Check for placeholder vs formal parameters
    unless (null names) $
        fail "Closure traits takes no formal parameters"
    let code = VCode mkSub { subName = name, subBody = fun } 
    case name of
        "END"   -> return $ App (Var "&unshift") [Var "@*END"] [Syn "sub" [Val code]]
        "BEGIN" -> do
            rv <- unsafeEvalExp fun
            return $ if rhs then rv else emptyExp 
        _       -> fail ""

unsafeEvalLexDiff :: Exp -> RuleParser Pad
unsafeEvalLexDiff exp = do
    env  <- getState
    setState env{ envLexical = mkPad [] }
    env' <- unsafeEvalEnv exp
    setState env'{ envLexical = envLexical env' `unionPads` envLexical env }
    return $ envLexical env'

-- XXX: Should these fail instead of error?
unsafeEvalEnv :: Exp -> RuleParser Env
unsafeEvalEnv exp = do
    -- pos <- getPosition
    env <- getState
    val <- unsafeEvalExp $ mergeStmts exp (Syn "env" [])
    case val of
        Val (VControl (ControlEnv env')) ->
            return env'{ envDebug = envDebug env }
        _  -> error $ pretty val

unsafeEvalExp :: Exp -> RuleParser Exp
unsafeEvalExp exp = do
    env <- getState
    setState env{ envStash = "" } -- cleans up function cache
    let val = unsafePerformIO $ do
        runEvalIO (env{ envDebug = Nothing }) $ do
            evl <- asks envEval
            evl exp
    case val of
        VError _ _  -> error $ pretty (val :: Val)
        _           -> return $ Val val

rulePackageDeclaration :: RuleParser a
rulePackageDeclaration = rule "package declaration" $ fail ""

-- Constructs ------------------------------------------------

ruleConstruct :: RuleParser Exp
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

ruleKeywordConsturct :: String -> RuleParser Exp
ruleKeywordConsturct keyword = rule (keyword ++ " construct") $ do
    symbol keyword
    block <- ruleBlock
    retSyn keyword [block]

ruleGatherConstruct :: RuleParser Exp
ruleGatherConstruct = ruleKeywordConsturct "gather"

ruleTryConstruct :: RuleParser Exp
ruleTryConstruct = ruleKeywordConsturct "try"

ruleForConstruct :: RuleParser Exp
ruleForConstruct = rule "for construct" $ do
    symbol "for"
    list  <- maybeParens ruleExpression
    optional (symbol ",")
    block <- ruleBlockLiteral <|> parseLitOp
    retSyn "for" [list, block]

ruleLoopConstruct :: RuleParser Exp
ruleLoopConstruct = rule "loop construct" $ do
    symbol "loop"
    conds <- option [] $ maybeParens $ try $ do
        a <- option emptyExp ruleExpression
        symbol ";"
        b <- option emptyExp ruleExpression
        symbol ";"
        c <- option emptyExp ruleExpression
        return [a,b,c]
    block <- ruleBlock
    -- XXX while/until
    retSyn "loop" (conds ++ [block])

ruleCondConstruct :: RuleParser Exp
ruleCondConstruct = rule "conditional construct" $ do
    csym <- choice [ symbol "if", symbol "unless" ]
    ruleCondBody $ csym

ruleCondBody :: String -> RuleParser Exp
ruleCondBody csym = rule "conditional expression" $ do
    cond <- maybeParens $ ruleExpression
    body <- ruleBlock
    bodyElse <- option emptyExp ruleElseConstruct
    retSyn csym [cond, body, bodyElse]

ruleElseConstruct :: RuleParser Exp
ruleElseConstruct = rule "else or elsif construct" $
    do
        symbol "else"
        ruleBlock
    <|> do
        symbol "elsif"
        ruleCondBody "if"

ruleWhileUntilConstruct :: RuleParser Exp
ruleWhileUntilConstruct = rule "while/until construct" $ do
    sym <- choice [ symbol "while", symbol "until" ]
    cond <- maybeParens $ ruleExpression
    body <- ruleBlock
    retSyn sym [ cond, body ]

ruleGivenConstruct :: RuleParser Exp
ruleGivenConstruct = rule "given construct" $ do
    sym <- symbol "given"
    topic <- maybeParens $ ruleExpression
    body <- ruleBlock
    retSyn sym [ topic, body ]

ruleWhenConstruct :: RuleParser Exp
ruleWhenConstruct = rule "when construct" $ do
    sym <- symbol "when"
    match <- maybeParens $ ruleExpression
    body <- ruleBlock
    retSyn sym [ match, body ]

-- XXX: make this translate into when true, when smartmatch
-- against true works
ruleDefaultConstruct :: RuleParser Exp
ruleDefaultConstruct = rule "default construct" $ do
    sym <- symbol "default"
    body <- ruleBlock
    retSyn sym [ body ]

-- Expressions ------------------------------------------------

ruleExpression :: RuleParser Exp
ruleExpression = (<?> "expression") $ parseOp

rulePostConditional :: RuleParser (Exp -> RuleParser Exp)
rulePostConditional = rule "postfix conditional" $ do
    cond <- tryChoice $ map symbol ["if", "unless"]
    exp <- ruleExpression
    return $ \body -> retSyn cond [exp, body, emptyExp]

rulePostLoop :: RuleParser (Exp -> RuleParser Exp)
rulePostLoop = rule "postfix loop" $ do
    cond <- tryChoice $ map symbol ["while", "until"]
    exp <- ruleExpression
    return $ \body -> retSyn cond [exp, body]

rulePostIterate :: RuleParser (Exp -> RuleParser Exp)
rulePostIterate = rule "postfix iteration" $ do
    cond <- tryChoice $ map symbol ["for"]
    exp <- ruleExpression
    return $ \body -> do
        block <- retBlock SubBlock Nothing body
        retSyn cond [exp, block]

ruleBlockLiteral :: RuleParser Exp
ruleBlockLiteral = rule "block construct" $ do
    (typ, formal) <- option (SubBlock, Nothing) $ choice
        [ ruleBlockFormalPointy
        , ruleBlockFormalStandard
        ]
    body <- ruleBlock
    retBlock typ formal body

extractHash :: Exp -> Maybe Exp
extractHash (Syn "block" [exp]) = extractHash (unwrap exp)
extractHash exp@(App (Var "&pair") _ _) = Just exp
extractHash exp@(App (Var "&infix:=>") _ _) = Just exp
extractHash exp@(Syn "," (App (Var "&pair") _ _:_)) = Just exp
extractHash exp@(Syn "," (App (Var "&infix:=>") _ _:_)) = Just exp
extractHash _ = Nothing

retBlock :: SubType -> Maybe [Param] -> Exp -> RuleParser Exp
retBlock SubBlock Nothing exp | Just hashExp <- extractHash (unwrap exp) = return $ Syn "\\{}" [hashExp]
retBlock typ formal body = retVerbatimBlock typ formal body

retVerbatimBlock :: SubType -> Maybe [Param] -> Exp -> RuleParser Exp
retVerbatimBlock typ formal body = expRule $ do
    let (fun, names, params) = doExtract formal body
    -- Check for placeholder vs formal parameters
    unless (isNothing formal || null names) $ 
        fail "Cannot mix placeholder variables with formal parameters"
    let sub = MkCode
            { isMulti       = False
            , subName       = "<anon>"
            , subPad        = mkPad []
            , subType       = typ
            , subAssoc      = "pre"
            , subReturns    = anyType
            , subParams     = if null params then [defaultArrayParam] else params
            , subBindings   = []
            , subSlurpLimit = []
            , subBody       = fun
            }
    return (Syn "sub" [Val $ VCode sub])

ruleBlockFormalStandard :: RuleParser (SubType, Maybe [Param])
ruleBlockFormalStandard = rule "standard block parameters" $ do
    symbol "sub"
    params <- option Nothing $ ruleSubParameters ParensMandatory
    return $ (SubRoutine, params)

ruleBlockFormalPointy :: RuleParser (SubType, Maybe [Param])
ruleBlockFormalPointy = rule "pointy block parameters" $ do
    symbol "->"
    params <- ruleSubParameters ParensOptional
    return $ (SubBlock, params)



















-- Not yet transcribed ------------------------------------------------


tightOperators :: RuleParser [[Operator Char Env Exp]]
tightOperators = do
  (optionary, unary) <- currentUnaryFunctions
  return $
    [ methOps  " . .+ .? .* .+ .() .[] .{} .<<>> .= "   -- Method postfix
    , postOps  " ++ -- " ++ preOps " ++ -- "            -- Auto-Increment
    , rightOps " ** "                                   -- Exponentiation
    , preSyn "* **"                                     -- Symbolic Unary
      ++ preOps (concatMap (\x -> " -" ++ [x]) "rwxoRWXOezsfdlpSbctugkTBMAC")
      ++ preOps " = ! + - ~ ? +^ ~^ ?^ \\ "
    , leftOps $
               " »*« »/« »x« »xx« »~« " ++
               " >>*<< >>/<< >>x<< >>xx<< >>~<< " ++
               " * / % x xx +& +< +> ~& ~< ~> "         -- Multiplicative
    , leftOps  " »+« >>+<< + - ~ +| +^ ~| ~^ ?| "       -- Additive
    , listOps  " & ! "                                  -- Junctive And
    , listOps  " ^ | "                                  -- Junctive Or
    , optOps optionary, preOps unary                    -- Named Unary
    , noneSyn  " is but does "                          -- Traits
      ++ rightOps " => "                                -- Pair constructor
      ++ noneOps " cmp <=> .. ^.. ..^ ^..^ "            -- Non-chaining Binary
      ++ postOps "..."                                  -- Infinite range
    , chainOps $
               " != == < <= > >= ~~ !~ " ++
               " eq ne lt le gt ge =:= "                -- Chained Binary
    , leftOps  " && !! "                                -- Tight And
    , leftOps  " || ^^ // "                             -- Tight Or
    , [ternOp "??" "::" "if"]                           -- Ternary
    -- Assignment
    , rightSyn $
               " = := ::= " ++
               " ~= += -= *= /= %= x= Y= ¥= **= xx= ||= &&= //= ^^= " ++
               " +&= +|= +^= ~&= ~|= ~^= ?|= ?^= |= ^= &= "
    ]

looseOperators :: RuleParser [[Operator Char Env Exp]]
looseOperators = do
    -- names <- currentListFunctions
    return $
        [ -- preOps names                               -- List Operator
          leftOps  " ==> "                              -- Pipe Forward
        , leftOps  " and nor "                          -- Loose And
        , leftOps  " or xor err "                       -- Loose Or
        ]

operators :: RuleParser (OperatorTable Char Env Exp)
operators = do
    tight <- tightOperators
    loose <- looseOperators
    return $ concat $
        [ tight
        , [ listSyn  " , ", listOps " Y ¥ " ]           -- Comma
        , loose
    --  , [ listSyn  " ; " ]                            -- Terminator
        ]

litOperators :: RuleParser (OperatorTable Char Env Exp)
litOperators = do
    tight <- tightOperators
    loose <- looseOperators
    return $ tight ++ loose

currentFunctions :: RuleParser [(Ident, VStr, Params)]
currentFunctions = do
    env     <- getState
    return . concat . unsafePerformSTM $ do
        glob <- readTVar $ envGlobal env
        let funs  = padToList glob ++ padToList (envLexical env)
        forM [ fun | fun@(('&':_), _) <- funs ] $ \(name, tvars) -> do
            let name' = dropWhile (not . isAlphaNum) $ name
            fmap catMaybes $ forM tvars $ \(_, tvar) -> do
                ref <- readTVar tvar
                -- read from ref
                return $ case ref of
                    MkRef (ICode cv) -> Just $
                        (name', code_assoc cv, code_params cv)
                    MkRef (IScalar sv)
                        | Just (VCode code) <- scalar_const sv -> Just $
                        (name', code_assoc code, code_params code)
                    _ -> Nothing

currentUnaryFunctions :: RuleParser (String, String)
currentUnaryFunctions = do
    env     <- getState
    case envStash env of
        "" -> do
            (x, y) <- currentUnaryFunctions'
            setState env{ envStash = unlines [x, y] }
            return (x, y)
        lns -> do
            let [x, y] = lines lns
            return (x, y)

currentUnaryFunctions' :: RuleParser (String, String)
currentUnaryFunctions' = do
    funs    <- currentFunctions
    let (unary, rest) = (`partition` funs) $ \x -> case x of
            (_, "pre", [param]) | not (isSlurpy param) -> True
            _  -> False
        rest' = (`filter` rest) $ \x -> case x of
            (_, _, (_:_:_)) -> True
            (_, _, [param])
                | ('@':_) <- paramName param
                , isSlurpy param -> True
            _ -> False
        restNames = Set.fromList $ map (\(name, _, _) -> name) rest'
    return . mapPair munge . partition fst . sort $
        [ (isOptional param, encodeUTF8 name) | (name, _, [param]) <- unary
        , not (name `Set.member` restNames)
        ]
    where
    munge = unwords . map snd
    mapPair f (x, y) = (f x, f y)

parseName :: String -> String
parseName str
    | (_, (_:name)) <- break (== ':') str
    = name
    | otherwise
    = dropWhile (not . isAlpha) str

currentListFunctions :: RuleParser [a]
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

parseOp :: RuleParser Exp
parseOp = expRule $ do
    ops <- operators
    buildExpressionParser ops parseTerm (Syn "" [])

parseTightOp :: RuleParser Exp
parseTightOp = expRule $ do
    ops <- tightOperators
    buildExpressionParser ops parseTerm (Syn "" [])

parseLitOp :: RuleParser Exp
parseLitOp = expRule $ do
    ops <- litOperators
    buildExpressionParser ops parseTerm (Syn "" [])

ops :: (String -> a) -> String -> [a]
ops f s = [f n | n <- sortBy revLength (words $ decodeUTF8 s)]
    where
    revLength x y = compare (length y) (length x)

doApp :: String -> [Exp] -> Exp
doApp str args = App (Var str) args []

preSyn      :: String -> [Operator Char Env Exp]
preSyn      = ops $ makeOp1 Prefix "" Syn
preOps      :: String -> [Operator Char Env Exp]
preOps      = ops $ makeOp1 Prefix "&prefix:" doApp
postOps     :: String -> [Operator Char Env Exp]
postOps     = ops $ makeOp1 Postfix "&postfix:" doApp
optOps      :: String -> [Operator Char Env Exp]
optOps      = ops $ makeOp1 OptionalPrefix "&prefix:" doApp
leftOps     :: String -> [Operator Char Env Exp]
leftOps     = ops $ makeOp2 AssocLeft "&infix:" doApp
rightOps    :: String -> [Operator Char Env Exp]
rightOps    = ops $ makeOp2 AssocRight "&infix:" doApp
noneOps     :: String -> [Operator Char Env Exp]
noneOps     = ops $ makeOp2 AssocNone "&infix:" doApp
listOps     :: String -> [Operator Char Env Exp]
listOps     = leftOps
chainOps    :: String -> [Operator Char Env Exp]
chainOps    = leftOps
leftSyn     :: String -> [Operator Char Env Exp]
leftSyn     = ops $ makeOp2 AssocLeft "" Syn
rightSyn    :: String -> [Operator Char Env Exp]
rightSyn    = ops $ makeOp2 AssocRight "" Syn
noneSyn     :: String -> [Operator Char Env Exp]
noneSyn     = ops $ makeOp2 AssocNone "" Syn
listSyn     :: String -> [Operator Char Env Exp]
listSyn     = ops $ makeOp0 AssocList "" Syn
chainSyn    :: String -> [Operator Char Env Exp]
chainSyn    = leftSyn

-- chainOps    = ops $ makeOpChained

makeOp1 :: (RuleParser (Exp -> a) -> b) -> 
        String -> 
        (String -> [Exp] -> a) -> 
        String -> 
        b
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

makeOp2 :: Assoc -> 
           String -> 
           (String -> [a] -> a) -> 
           String -> 
           Operator Char Env a
makeOp2 prec sigil con name = (`Infix` prec) $ do
    symbol name
    return $ \x y -> con (sigil ++ name) [x,y]

makeOp0 :: Assoc -> 
           String -> 
           (String -> [a] -> a) -> 
           String -> 
           Operator Char Env a
makeOp0 prec sigil con name = (`InfixList` prec) $ do
    many1 $ do
        string name
        whiteSpace
    return . con $ sigil ++ name

parseTerm :: RuleParser Exp
parseTerm = rule "term" $ do
    term <- choice
        [ ruleVar
        , ruleLit
        , ruleClosureTrait True
        , parseApply
        , parens ruleExpression
        ]
    fs <- many rulePostTerm
    return $ combine (reverse fs) term

rulePostTerm :: RuleParser (Exp -> Exp)
rulePostTerm = tryVerbatimRule "term postfix" $ do
    hasDot <- option False $ do whiteSpace; char '.'; return True
    choice $ (if hasDot then [ruleInvocation] else []) ++
        [ ruleArraySubscript
        , ruleHashSubscript
        , ruleCodeSubscript
        ]

ruleInvocation :: RuleParser (Exp -> Exp)
ruleInvocation = tryVerbatimRule "invocation" $ do
    hasEqual <- option False $ do char '='; whiteSpace; return True
    name            <- ruleSubName
    (invs,args)     <- option ([],[]) $ parseParenParamList
    return $ \x -> if hasEqual
        then Syn "=" [x, App (Var name) (x:invs) args]
        else App (Var name) (x:invs) args

ruleInvocationParens :: RuleParser (Exp -> Exp)
ruleInvocationParens = do
    hasEqual <- option False $ do char '='; whiteSpace; return True
    name            <- ruleSubName
    (invs,args)     <- parens $ parseNoParenParamList
    -- XXX we just append the adverbial block onto the end of the arg list
    -- it really goes into the *& slot if there is one. -lp
    return $ \x -> if hasEqual
        then Syn "=" [x, App (Var name) (x:invs) args]
        else App (Var name) (x:invs) args

ruleArraySubscript :: RuleParser (Exp -> Exp)
ruleArraySubscript = tryVerbatimRule "array subscript" $ do
    symbol "["
    p <- option id $ do exp <- ruleExpression; return $ \x -> Syn "[]" [x, exp]
    char ']'
    return p

ruleHashSubscript :: RuleParser (Exp -> Exp)
ruleHashSubscript = tryVerbatimRule "hash subscript" $ do
    choice [ ruleHashSubscriptBraces, ruleHashSubscriptQW ]

ruleHashSubscriptBraces :: RuleParser (Exp -> Exp)
ruleHashSubscriptBraces = do
    symbol "{"
    p <- option id $ do exp <- ruleExpression; return $ \x -> Syn "{}" [x, exp]
    char '}'
    return p

ruleHashSubscriptQW :: RuleParser (Exp -> Exp)
ruleHashSubscriptQW = do
    exp <- angleBracketLiteral
    return $ \x -> Syn "{}" [x, exp]

ruleCodeSubscript :: RuleParser (Exp -> Exp)
ruleCodeSubscript = tryRule "code subscript" $ do
    (invs,args) <- parens $ parseParamList
    return $ \x -> App x invs args

parseApply :: RuleParser Exp
parseApply = tryRule "apply" $ do
    name    <- ruleSubName <|> ruleFoldOp
    when ((name ==) `any` words " &if &unless &while &until &for ") $
        fail "reserved word"
    hasDot  <- option False $ try $ do { whiteSpace; char '.'; return True }
    (invs, args) <- if hasDot
        then parseNoParenParamList
        else parseParenParamList <|> do { whiteSpace; parseNoParenParamList }
    return $ App (Var name) invs args

ruleFoldOp :: RuleParser String
ruleFoldOp = verbatimRule "reduce metaoperator" $ do
    char '['
    name <- string "+" -- XXX - Query all infix here
    char ']'
    return $ "&prefix:[" ++ name ++ "]"

parseParamList :: RuleParser ([Exp], [Exp])
parseParamList = parseParenParamList <|> parseNoParenParamList

parseParenParamList :: RuleParser ([Exp], [Exp])
parseParenParamList = try $ do
    params <- option Nothing $ fmap Just (parens parseNoParenParamList)
    block       <- option [] ruleAdverbBlock
    when (isNothing params && null block) $ fail ""
    let (inv, norm) = maybe ([], []) id params
    -- XXX we just append the adverbial block onto the end of the arg list
    -- it really goes into the *& slot if there is one. -lp
    processFormals [inv, norm ++ block]

ruleAdverbBlock :: RuleParser [Exp]
ruleAdverbBlock = tryRule "adverbial block" $ do
    char ':'
    rblock <- ruleBlockLiteral
    next <- option [] ruleAdverbBlock
    return (rblock:next)

parseNoParenParamList :: RuleParser ([Exp], [Exp])
parseNoParenParamList = do
    formal <- (`sepEndBy` symbol ":") $ fix $ \rec -> do
        rv <- option Nothing $ do
            fmap Just $ tryChoice
                [ do x <- ruleBlockLiteral
                     lookAhead (satisfy (/= ','))
                     return (x, return "")
                , do x <- parseLitOp
                     return (x, symbol ",")
                ]
        case rv of
            Nothing           -> return []
            Just (exp, trail) -> do
                rest <- option [] $ do { trail; rec }
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
nameToParam name = MkParam
    { isInvocant    = False
    , isOptional    = False
    , isNamed       = False
    , isLValue      = True
    , isWritable    = (name == "$_")
    , isLazy        = False
    , paramName     = name
    , paramContext  = case name of
        -- "$_" -> CxtSlurpy $ typeOfSigil (head name)
        _    -> CxtItem   $ typeOfSigil (head name)
    , paramDefault  = Val VUndef
    }

maybeParensBool :: RuleParser a -> RuleParser (a, Bool)
maybeParensBool p = choice
    [ do rv <- parens p; return (rv, True)
    , do rv <- p; return (rv, False)
    ]

maybeParens :: CharParser Env a -> RuleParser a
maybeParens p = choice [ parens p, p ]
maybeDotParens :: CharParser Env a -> RuleParser a
maybeDotParens p = choice [ dotParens p, p ]
    where
    dotParens rule = do
        option ' ' $ char '.'
        parens rule

parseVarName :: RuleParser String
parseVarName = rule "variable name" ruleVarNameString

ruleVarNameString :: RuleParser String
ruleVarNameString =   try (string "$!")  -- error variable
                  <|> try (string "$/")  -- match object
                  <|> try ruleMatchPos
                  <|> try ruleMatchNamed
                  <|> do
    sigil   <- oneOf "$@%&"
    --  ^ placeholder, * global, ? magical, . member, : private member
    caret   <- option "" $ choice $ map string $ words " ^ * ? . : "
    names   <- many1 wordAny `sepBy1` (try $ string "::")
    return $ (sigil:caret) ++ concat (intersperse "::" names)

ruleMatchPos :: RuleParser String
ruleMatchPos = do
    sigil   <- char '$'
    digits  <- many1 digit
    return $ (sigil:digits)

ruleMatchNamed :: RuleParser String
ruleMatchNamed = do
    sigil   <- char '$'
    twigil  <- char '<'
    name    <- many (do { char '\\'; anyChar } <|> satisfy (/= '>'))
    char '>'
    return $ (sigil:twigil:name)

ruleVar :: RuleParser Exp
ruleVar = do
    name    <- ruleVarNameString
    return $ makeVar name

makeVar :: String -> Exp
makeVar ('$':rest) | all (`elem` "1234567890") rest =
    Syn "[]" [Var "$/", Val $ VInt (read rest - 1)]
makeVar ('$':'<':name) =
    Syn "{}" [Var "$/", doSplitStr name]
makeVar var = Var var

ruleLit :: RuleParser Exp
ruleLit = choice
    [ ruleBlockLiteral
    , numLiteral
    , emptyListLiteral
    , emptyArrayLiteral
    , arrayLiteral
    , pairLiteral
    , undefLiteral
--    , namedLiteral "undef"  VUndef
    , namedLiteral "NaN"    (VNum $ 0/0)
    , namedLiteral "Inf"    (VNum $ 1/0)
    , yadaLiteral
    , qLiteral
    , rxLiteral
    , rxLiteralBare
    , substLiteral
    ]

undefLiteral :: RuleParser Exp
undefLiteral = try $ do
    symbol "undef"
    (invs,args)   <- maybeParens $ parseParamList
    return $ if null (invs ++ args)
        then Val VUndef
        else App (Var "&undef") invs args

numLiteral :: RuleParser Exp
numLiteral = do
    n <- naturalOrRat
    case n of
        Left  i -> return . Val $ VInt i
        Right d -> return . Val $ VRat d

emptyListLiteral :: RuleParser Exp
emptyListLiteral = tryRule "empty list" $ do
    parens whiteSpace
    return $ Syn "," []

emptyArrayLiteral :: RuleParser Exp
emptyArrayLiteral = tryRule "empty array" $ do
    brackets whiteSpace
    return $ Syn "\\[]" [emptyExp]

arrayLiteral :: RuleParser Exp
arrayLiteral = try $ do
    item <- brackets ruleExpression
    return $ Syn "\\[]" [item]

pairLiteral :: RuleParser Exp
pairLiteral = tryChoice [ pairArrow, pairAdverb ]

pairArrow :: RuleParser Exp
pairArrow = do
    key <- identifier
    symbol "=>"
    val <- parseTerm
    return (Val (VStr key), val)
    return $ App (Var "&infix:=>") [Val (VStr key), val] []

pairAdverb :: RuleParser Exp
pairAdverb = do
    string ":"
    key <- many1 wordAny
    val <- option (Val $ VInt 1) (valueDot <|> valueExp)
    return $ App (Var "&infix:=>") [Val (VStr key), val] []
    where
    valueDot = do
        skipMany1 (satisfy isSpace)
        symbol "."
        option (Val $ VInt 1) $ valueExp
    valueExp = choice
        [ parens ruleExpression
        , arrayLiteral
        , angleBracketLiteral
        ]

-- Interpolating constructs
qInterpolatorChar :: RuleParser Exp
qInterpolatorChar = do
    char '\\'
    nextchar <- escapeCode -- see Lexer.hs
    return (Val $ VStr [nextchar])

qInterpolateDelimiter :: Char -> RuleParser Exp
qInterpolateDelimiter protectedChar = do
    char '\\'
    c <- oneOf (protectedChar:"\\")
    return (Val $ VStr [c])

qInterpolateQuoteConstruct :: RuleParser Exp
qInterpolateQuoteConstruct = try $ do
    string "\\q"
    flag <- many1 alphaNum
    char '['
    expr <- interpolatingStringLiteral (char ']') (qInterpolator $ getQFlags [flag] ']')
    char ']'
    return expr

qInterpolatorPostTerm :: RuleParser (Exp -> Exp)
qInterpolatorPostTerm = try $ do
    option ' ' $ char '.'
    choice
        [ ruleInvocationParens
        , ruleArraySubscript
        , ruleHashSubscript
        , ruleCodeSubscript
        ]

qInterpolator :: QFlags -> RuleParser Exp
qInterpolator flags = choice [
        closure,
        backslash,
        variable
    ]
    where
        closure = if qfInterpolateClosure flags
            then ruleVerbatimBlock
            else mzero
        backslash = case qfInterpolateBackslash flags of
            QB_All -> try qInterpolatorChar
               <|> (try qInterpolateQuoteConstruct)
               <|> (try $ qInterpolateDelimiter $ qfProtectedChar flags)
            QB_Single -> try qInterpolateQuoteConstruct
               <|> (try $ qInterpolateDelimiter $ qfProtectedChar flags)
            QB_No -> mzero
        variable = try $ do
            var <- ruleVarNameString
            fs <- case head var of
                '$' -> if qfInterpolateScalar flags &&
                          notProtected var flags
                    then many qInterpolatorPostTerm
                    else fail ""
                '@' -> if qfInterpolateArray flags
                    then many1 qInterpolatorPostTerm
                    else fail ""
                '%' -> if qfInterpolateHash flags
                    then many1 qInterpolatorPostTerm
                    else fail ""
                '&' -> if qfInterpolateFunction flags
                    then many1 qInterpolatorPostTerm
                    else fail ""
                _   -> fail ""
            return $ combine (reverse fs) (makeVar var)
        notProtected var flags =
            if second == qfProtectedChar flags
                then False --  $ followed by delimiter is protected
                else if qfP5RegularExpression flags &&
                        second `elem` ")]# \t"
                {- XXX this doesn't support Unicode whitespace. I'm not
                   sure this is a problem, because it's primarily meant
                   for legacy Perl 5 code -}
                    then False --  $ followed by )]# or whitespace
                    else True --  $ followed by anything else is interpolated
            where second = head $ tail var

qLiteral :: RuleParser Exp
qLiteral = do -- This should include q:anything// as well as '' "" <>
    (qEnd, flags) <- getQDelim
    qLiteral1 qEnd flags

qLiteral1 :: RuleParser x -- Closing delimiter
             -> QFlags
             -> RuleParser Exp
qLiteral1 qEnd flags = do
    expr <- interpolatingStringLiteral qEnd (qInterpolator flags)
    qEnd
    case qfSplitWords flags of
        -- expr ~~ rx:perl5:g/(\S+)/
        QS_Yes      -> return $ doSplit expr
        QS_Protect  -> return $ doSplit expr
        QS_No       -> return expr
    where
    -- words() regards \xa0 as (breaking) whitespace. But \xa0 is
    -- a nonbreaking ws char.
    doSplit (Cxt (CxtItem _) (Val (VStr str))) = doSplitStr str
    
    doSplit expr = Cxt cxtSlurpyAny $ App (Var "&infix:~~") [expr, rxSplit] []
    rxSplit = Syn "rx" $
        [ Val $ VStr "(\\S+)"
        , Val $ VList
            [ castV (VStr "P5", VInt 1)
            , castV (VStr "g", VInt 1)
            ]
        ]

doSplitStr str = case perl6Words str of
    []  -> Syn "," []
    [x] -> Val (VStr x)
    xs  -> Syn "," $ map (Val . VStr) xs
    where
    perl6Words :: String -> [String]
    perl6Words s
      | findSpace == [] = []
      | otherwise       = w : words s''
      where
      (w, s'')  = break isBreakingSpace findSpace
      findSpace = dropWhile isBreakingSpace s
      
    isBreakingSpace('\x09') = True
    isBreakingSpace('\x0a') = True
    isBreakingSpace('\x0d') = True
    isBreakingSpace('\x20') = True
    isBreakingSpace(_)      = False

angleBracketLiteral :: RuleParser Exp
angleBracketLiteral = try $
        do
        symbol "<<"
        qLiteral1 (symbol ">>") $ qqFlags
            { qfSplitWords = QS_Protect, qfProtectedChar = '>' }
    <|> do
        symbol "<"
        qLiteral1 (char '>') $ qFlags
            { qfSplitWords = QS_Yes, qfProtectedChar = '>' }
    <|> do
        symbol "\xab"
        qLiteral1 (char '\xbb') $ qFlags
            { qfSplitWords = QS_Yes, qfProtectedChar = '\xbb' }

-- Quoting delimitor and flags
-- qfProtectedChar is the character to be
--   protected by backslashes, if
--   qfInterpolateBackslash is Single or All.
data QS_Flag = QS_No | QS_Yes | QS_Protect deriving (Show, Eq, Ord, Typeable)
data QB_Flag = QB_No | QB_Single | QB_All deriving (Show, Eq, Ord, Typeable)

data QFlags = MkQFlags
    { qfSplitWords              :: !QS_Flag -- No, Yes, Protect
    , qfInterpolateScalar       :: !Bool
    , qfInterpolateArray        :: !Bool
    , qfInterpolateHash         :: !Bool
    , qfInterpolateFunction     :: !Bool
    , qfInterpolateClosure      :: !Bool
    , qfInterpolateBackslash    :: !QB_Flag -- No, Single, All
    , qfProtectedChar           :: !Char
    , qfP5RegularExpression     :: !Bool
    , qfFailed                  :: !Bool -- Failed parse
    }
    deriving (Show, Eq, Ord, Typeable)

getQFlags :: [String] -> Char -> QFlags
getQFlags flagnames protectedChar =
    (foldr useflag qFlags $ reverse flagnames) { qfProtectedChar = protectedChar }
    where
        -- Additive flags
          useflag "w" qf          = qf { qfSplitWords = QS_Yes }
          useflag "words" qf      = qf { qfSplitWords = QS_Yes }
          useflag "ww" qf         = qf { qfSplitWords = QS_Protect }
          useflag "quotewords" qf = qf { qfSplitWords = QS_Protect }
          useflag "s" qf          = qf { qfInterpolateScalar = True }
          useflag "scalar" qf     = qf { qfInterpolateScalar = True }
          useflag "a" qf          = qf { qfInterpolateArray = True }
          useflag "array" qf      = qf { qfInterpolateArray = True }
          useflag "h" qf          = qf { qfInterpolateHash = True }
          useflag "hash" qf       = qf { qfInterpolateHash = True }
          useflag "f" qf          = qf { qfInterpolateFunction = True }
          useflag "function" qf   = qf { qfInterpolateFunction = True }
          useflag "c" qf          = qf { qfInterpolateClosure = True }
          useflag "closure" qf    = qf { qfInterpolateClosure = True }
          useflag "b" qf          = qf { qfInterpolateBackslash = QB_All }
          useflag "backslash" qf  = qf { qfInterpolateBackslash = QB_All }

        -- Zeroing flags
          useflag "0" _           = rawFlags
          useflag "raw" _         = rawFlags
          useflag "1" _           = qFlags
          useflag "single" _      = qFlags
          useflag "2" _           = qqFlags
          useflag "double" _      = qqFlags
          useflag "q" _           = qqFlags -- support qq//

        -- in case of unknown flag, we simply abort the parse.
          useflag _ qf            = qf { qfFailed = True }


{- | XXX can be later defined to exclude alphanumerics, maybe also exclude
closing delims from being openers (disallow q]a]) -}
openingDelim :: CharParser Env Char
openingDelim = anyChar

getQDelim :: RuleParser (CharParser Env Char, QFlags)
getQDelim = try $
    do  string "q"
        flags <- do
            firstflag <- many alphaNum
            allflags  <- many oneflag
            case firstflag of
                "" -> return allflags
                _  -> return $ firstflag:allflags

        notFollowedBy alphaNum
        whiteSpace
        delim <- openingDelim
        let qflags = getQFlags flags $ balancedDelim delim
        when (qfFailed qflags) $ fail ""
        return (char $ balancedDelim delim, qflags)
    <|> try (do
        string "<<"
        return (
            string ">>" >> return 'x',
            qqFlags { qfSplitWords = QS_Yes, qfProtectedChar = '>' }))
    <|> do
        delim <- oneOf "\"'<\xab"
        case delim of
            '"'     -> return (char '"',    qqFlags)
            '\''    -> return (char '\'',   qFlags)
            '<'     -> return (char '>',    qFlags
                { qfSplitWords = QS_Yes, qfProtectedChar = '>' })
            '\xab'  -> return (char '\xbb', qqFlags
                { qfSplitWords = QS_Protect, qfProtectedChar = '\xbb' })
            _       -> fail ""

    where
          oneflag = do string ":"
                       many alphaNum

-- | Default flags
qFlags    :: QFlags
qFlags    = MkQFlags QS_No False False False False False QB_Single '\'' False False
-- | Default flags
qqFlags   :: QFlags
qqFlags   = MkQFlags QS_No True True True True True QB_All '"' False False
-- | Default flags
rawFlags  :: QFlags
rawFlags  = MkQFlags QS_No False False False False False QB_No 'x' False False
-- | Default flags
rxP5Flags :: QFlags
rxP5Flags = MkQFlags QS_No True True True True True QB_No '/' True False
-- | Default flags
rxP6Flags :: QFlags
rxP6Flags = MkQFlags QS_No False False False False False QB_No '/' False False

-- Regexps
rxLiteral5 :: Char -- ^ Closing delimiter
             -> RuleParser Exp
rxLiteral5 delim = qLiteral1 (char delim) $
        rxP5Flags { qfProtectedChar = delim }

rxLiteral6 :: Char -- ^ Closing delimiter
             -> RuleParser Exp
rxLiteral6 delim = qLiteral1 (char delim) $
        rxP6Flags { qfProtectedChar = delim }

ruleAdverbHash :: RuleParser Exp
ruleAdverbHash = do
    pairs <- many pairAdverb
    return $ Syn "\\{}" [Syn "," pairs]

substLiteral :: RuleParser Exp
substLiteral = try $ do
    symbol "s"
    adverbs <- ruleAdverbHash
    ch      <- openingDelim
    let endch = balancedDelim ch
    -- XXX - probe for adverbs to determine p5 vs p6
    expr    <- rxLiteral5 endch
    ch      <- if ch == endch then return ch else do { whiteSpace ; anyChar }
    let endch = balancedDelim ch
    subst   <- qLiteral1 (char endch) qqFlags { qfProtectedChar = endch }
    return $ Syn "subst" [expr, subst, adverbs]

rxLiteral :: RuleParser Exp
rxLiteral = try $ do
    symbol "rx"
    adverbs <- ruleAdverbHash
    ch      <- anyChar
    -- XXX - probe for adverbs to determine p5 vs p6
    expr    <- rxLiteral5 $ balancedDelim ch
    return $ Syn "rx" [expr, adverbs]

rxLiteralBare :: RuleParser Exp
rxLiteralBare = try $ do
    ch      <- char '/'
    expr    <- rxLiteral6 $ balancedDelim ch
    return $ Syn "rx" [expr, Val undef]

namedLiteral :: String -> Val -> RuleParser Exp
namedLiteral n v = do { symbol n; return $ Val v }

yadaLiteral :: RuleParser Exp
yadaLiteral = do
    pos1 <- getPosition
    sym  <- choice . map symbol $ words " ... ??? !!! "
    pos2 <- getPosition
    return . Val $ VError sym (NonTerm (mkPos pos1 pos2))
{-
    return . Val . VRef . thunkRef . MkThunk $
        local (\e -> e{ envPos = mkPos pos1 pos2}) $ do
            fail "This function is not yet implemented"
-}

op_methodPostfix    :: [a]
op_methodPostfix    = []
op_namedUnary       :: [a]
op_namedUnary       = []
methOps             :: a -> [b]
methOps _ = []

ternOp :: String -> String -> String -> Operator Char Env Exp
ternOp pre post syn = (`Infix` AssocRight) $ do
    symbol pre
    y <- parseTightOp
    symbol post
    return $ \x z -> Syn syn [x, y, z]

runRule :: Env -> (Env -> a) -> RuleParser Env -> FilePath -> String -> a
runRule env f p name str = f $ case ( runParser p env name str ) of
    Left err    -> env { envBody = Val $ VError msg (NonTerm (mkPos pos pos)) }
        where
        pos = errorPos err
        msg = showErr err
    Right env'  -> env'

showErr :: ParseError -> String
showErr err =
      showErrorMessages "or" "unknown parse error"
                        "expecting" "unexpected" "end of input"
                       (errorMessages err)

retSyn :: String -> [Exp] -> RuleParser Exp
retSyn sym args = do
    return $ Syn sym args
    
