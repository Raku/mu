{-# OPTIONS_GHC -cpp -fglasgow-exts -funbox-strict-fields #-}
{-# OPTIONS_GHC -#include "UnicodeC.h" #-}

{-|
    Higher-level parser for building ASTs.

>   I sang of leaves, of leaves of gold, and leaves of gold there grew:
>   Of wind I sang, a wind there came and in the branches blew.
>   Beyond the Sun, beyond the Moon, the foam was on the Sea,
>   And by the strand of Ilmarin there grew a golden Tree...
-}

module Pugs.Parser (
    runRule,
    ruleProgram,
) where
import Pugs.Internals
import Pugs.AST
import Pugs.Types
import Pugs.Context
import Pugs.Help
import Pugs.Lexer
import Pugs.Rule
import Pugs.Rule.Expr
import Pugs.Rule.Error
import Pugs.Pretty
import qualified Data.Set as Set

import Pugs.Parser.Number

-- Lexical units --------------------------------------------------

ruleProgram :: RuleParser Env
ruleProgram = rule "program" $ do
    env <- getState
    statements <- ruleBlockBody
    -- error $ show statements
    eof
    -- S04: CHECK {...}*      at compile time, ALAP
    --  $_() for @?CHECK
    rv <- unsafeEvalExp $ Syn "for"
	[ Var "@?CHECK"
	, Syn "sub"
	    [ Val . VCode $ mkSub
		{ subBody   = App (Var "$_") [] []
		, subParams = [defaultScalarParam]
		}
	    ]
	]
    -- If there was a exit() in a CHECK block, we've to exit.
    possiblyExit rv
    env' <- getState
    return $ env'
        { envBody       = mergeStmts emptyExp statements
        , envStash      = ""
        , envPackage    = envPackage env
        }

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

-- Declarations ------------------------------------------------

ruleBlockDeclaration :: RuleParser Exp
ruleBlockDeclaration = rule "block declaration" $ choice
    [ ruleSubDeclaration
    , ruleClosureTrait False
    , ruleRuleDeclaration
    , ruleClassDeclaration
    ]

ruleDeclaration :: RuleParser Exp
ruleDeclaration = rule "declaration" $ choice
    [ ruleModuleDeclaration
    , ruleVarDeclaration
    , ruleMemberDeclaration
    , ruleUseDeclaration
    , ruleInlineDeclaration
    , ruleRequireDeclaration
    ]

ruleSubHead :: RuleParser (Bool, SubType, String)
ruleSubHead = rule "subroutine head" $ do
    isMulti     <- option False $ do { symbol "multi" ; return True }
    (styp, name) <- choice
        [ do symbol "sub"
             str    <- ruleSubName
             return (SubRoutine, str)
        , do symbol "coro"
             colon  <- maybeColon
             str    <- ruleSubName
             return (SubCoroutine, colon str)
        , do (symbol "submethod" <|> symbol "method")
             colon  <- maybeColon
             str    <- ruleSubName
             return (SubMethod, colon str)
        ]
    return (isMulti, styp, name)

maybeColon :: RuleParser ([Char] -> [Char])
maybeColon = option id $ do
    char ':'
    return $ \(sigil:name) -> (sigil:':':name)

-- | Scope, context, isMulti, styp, name
ruleSubScopedWithContext :: RuleParser (Scope, String, Bool, SubType, String)
ruleSubScopedWithContext = rule "scoped subroutine with context" $ do
    scope   <- ruleScope
    cxt     <- identifier
    (isMulti, styp, name) <- ruleSubHead
    return (scope, cxt, isMulti, styp, name)

ruleSubScoped :: RuleParser (Scope, String, Bool, SubType, String)
ruleSubScoped = rule "scoped subroutine" $ do
    scope <- ruleScope
    (isMulti, styp, name) <- ruleSubHead
    return (scope, "Any", isMulti, styp, name)

ruleSubGlobal :: RuleParser (Scope, String, Bool, SubType, String)
ruleSubGlobal = rule "global subroutine" $ do
    (isMulti, styp, name) <- ruleSubHead
    return (SGlobal, "Any", isMulti, styp, name)


doExtract :: Maybe [Param] -> Exp -> (Exp, [String], [Param])
doExtract formal body = (fun, names', params)
    where
    (fun, names) = extract body []
    names' | isJust formal -- Just params <- formal, any (== "$_") (map paramName params)
           = filter (/= "$_") names
           | otherwise
           = names
    params = map nameToParam (sort names')
        ++ (maybe (if null names' then [defaultArrayParam] else []) id formal)

ruleRuleDeclaration :: RuleParser Exp
ruleRuleDeclaration = rule "rule declaration" $ try $ do
    symbol "rule"
    name    <- identifier
    adverbs <- ruleAdverbHash
    ch      <- char '{'
    expr    <- rxLiteralAny adverbs $ balancedDelim ch
    let exp = Syn ":=" [Var ('<':name), Syn "rx" [expr, adverbs]]
    unsafeEvalExp (Sym SGlobal ('<':name) exp)
    return emptyExp

ruleClassDeclaration :: RuleParser Exp
ruleClassDeclaration = rule "class declaration" $ try $ do
    _       <- choice $ map symbol (words "class role grammar")
    (name, _, _) <- rulePackageHead
    env     <- getState
    setState env{ envPackage = name, envClasses = envClasses env `addNode` mkType name }
    body    <- between (symbol "{") (char '}') ruleBlockBody
    env'    <- getState
    setState env'{ envPackage = envPackage env }
    return body

rulePackageHead :: RuleParser (String, String, String)
rulePackageHead = do
    name    <- ruleQualifiedIdentifier
    v       <- option "" $ ruleVersionPart
    a       <- option "" $ ruleAuthorPart
    whiteSpace
    traits  <- many $ ruleTrait
    unsafeEvalExp (newClass name traits)
    return (name, v, a)

newClass :: String -> [String] -> Exp
newClass name traits = Sym SGlobal (':':'*':name) $ Syn ":="
    [ Var (':':'*':name)
    , App (Var "&new")
        [ Val (VType $ mkType "Class") ]
        [ App (Var "&infix:=>")
            [ Val (VStr "traits")
            , Val (VList $ map VStr traits)
            ] []
        ]
    ]

ruleSubDeclaration :: RuleParser Exp
ruleSubDeclaration = rule "subroutine declaration" $ do
    -- namePos <- getPosition
    (scope, typ, isMulti, styp, name) <- tryChoice
        [ ruleSubScopedWithContext
        , ruleSubScoped
        , ruleSubGlobal
        ]
    typ'    <- option typ $ try $ ruleBareTrait "returns"
    formal  <- option Nothing $ ruleSubParameters ParensMandatory
    typ''   <- option typ' $ try $ ruleBareTrait "returns"
    _       <- many $ ruleTrait -- traits; not yet used
    -- bodyPos <- getPosition
    body    <- ruleBlock
    let (fun, names, params) = doExtract formal body
    -- Check for placeholder vs formal parameters
    unless (isNothing formal || null names) $ 
        fail "Cannot mix placeholder variables with formal parameters"
    env <- getState
    let subExp = Val . VCode $ MkCode
            { isMulti       = isMulti
            , subName       = name'
            , subEnv        = Just env
            , subType       = styp
            , subAssoc      = "pre"
            , subReturns    = mkType typ''
            , subLValue     = False -- XXX "is rw"
            , subParams     = self ++ params
            , subBindings   = []
            , subSlurpLimit = []
            , subBody       = fun
            , subCont       = Nothing
            }
        name' = if styp <= SubMethod then "&" ++ envPackage env ++ "::" ++ tail name else name
        self :: [Param]
        self | styp > SubMethod = []
             | (prm:_) <- params, isInvocant prm = []
             | otherwise = [selfParam $ envPackage env]
        -- decl = Sym scope name -- , namePos)
        exp  = Syn ":=" [Var name', Syn "sub" [subExp]]
    if scope == SGlobal
        then do { unsafeEvalExp (Sym scope name' exp); return emptyExp }
        else do
            lexDiff <- unsafeEvalLexDiff (Sym scope name' emptyExp)
            return $ Pad scope lexDiff exp

-- | A Param representing the default (unnamed) invocant of a method on the given type.
selfParam :: String -> Param
selfParam typ = MkParam
    { isInvocant    = True
    , isOptional    = False
    , isNamed       = False
    , isLValue      = True
    , isWritable    = True
    , isLazy        = False
    , paramName     = "$?SELF"
    , paramContext  = CxtItem (mkType typ)
    , paramDefault  = Noop
    }

ruleSubName :: RuleParser String
ruleSubName = verbatimRule "subroutine name" $ do
    twigil  <- ruleTwigil
    fixity  <- option "" $ choice (map (try . string) $ words fixities)
    name    <- ruleQualifiedIdentifier
                <|> try (between (string "<<") (string ">>")
                    (many1 (satisfy (/= '>') <|> lookAhead (satisfy (/= '>')))))
                <|> between (char '<') (char '>') (many1 $ satisfy (/= '>'))
                <|> between (char '\171') (char '\187') (many1 $ satisfy (/= '\187'))
    return $ "&" ++ twigil ++ fixity ++ name
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
    name    <- ruleParamName -- XXX support *[...]
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

ruleMemberDeclaration :: RuleParser Exp
ruleMemberDeclaration = do
    symbol "has"
    typ  <- option "" $ lexeme ruleQualifiedIdentifier
    attr <- ruleVarName
    case attr of
        (sigil:twigil:key) | twigil `elem` ".:" -> do
            traits  <- many $ ruleTrait
            optional $ do { symbol "handles"; ruleExpression }
            env     <- getState
            -- manufacture an accessor
            let sub = mkPrim
                    { isMulti       = False
                    , subName       = name
                    , subEnv        = Nothing
                    , subReturns    = if null typ then typeOfSigil sigil else mkType typ
                    , subBody       = fun
                    , subParams     = [selfParam $ envPackage env]
                    , subLValue     = "rw" `elem` traits
                    }
                exp = Syn ":=" [Var name, Syn "sub" [Val $ VCode sub]]
                name | twigil == '.' = '&':(envPackage env ++ "::" ++ key)
                     | otherwise     = '&':(envPackage env ++ "::" ++ (twigil:key))
                fun = Cxt (cxtOfSigil sigil) (Syn "{}" [Var "$?SELF", Val (VStr key)])
            unsafeEvalExp (Sym SGlobal name exp)
            return emptyExp
        _ -> return emptyExp

ruleVarDeclaration :: RuleParser Exp
ruleVarDeclaration = rule "variable declaration" $ do
    scope       <- ruleScope
    optional $ do { ruleQualifiedIdentifier ; whiteSpace } -- Type
    (decl, lhs) <- choice
        [ do -- pos  <- getPosition
             name <- ruleVarName
             return ((Sym scope name), Var name)
        , do names <- parens . (`sepEndBy` symbol ",") $
                ruleVarName <|> do { undefLiteral; return "" }
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
    -- state $x = 42 is really syntax sugar for state $x; FIRST { $x = 42 }
    case scope of
	SState -> do
	    implicit_first_block <- vcode2firstBlock $ VCode mkSub { subBody = rhs }
	    return $ Pad scope lexDiff implicit_first_block
	_      -> return $ Pad scope lexDiff rhs

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
    names   <- identifier `sepBy1` (try $ string "::")
    _       <- option "" $ ruleVersionPart
    _       <- option "" $ ruleAuthorPart
    val <- unsafeEvalExp $
        App (Var "&use") [Val . VStr $ concat (intersperse "/" names) ++ ".pm"] []
    case val of
        Val (VControl (ControlEnv env')) -> setState env'
        _  -> error $ pretty val
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
    return $ App (Var "&require") [Val . VStr $ concat (intersperse "/" names) ++ ".pm"] []

ruleModuleDeclaration :: RuleParser Exp
ruleModuleDeclaration = rule "module declaration" $ do
    _       <- choice $ map symbol (words "package module class grammar")
    (name, v, a)    <- rulePackageHead
    env     <- getState
    setState env{ envPackage = name, envClasses = envClasses env `addNode` mkType name }
    return $ Syn "module" [Val . VStr $ name ++ v ++ a] -- XXX

-- | The version part of a full class specification.
ruleVersionPart :: RuleParser String
ruleVersionPart = do -- version - XXX
    char '-'
    str <- many1 (choice [ digit, char '.' ])
    return ('-':str)

-- | The author part of a full class specification.
ruleAuthorPart :: RuleParser String
ruleAuthorPart = do -- author - XXX
    char '-'
    str <- many1 (satisfy (/= ';'))
    return ('-':str)

ruleDoBlock :: RuleParser Exp
ruleDoBlock = rule "do block" $ try $ do
    symbol "do"
    ruleBlock

ruleClosureTrait :: Bool -> RuleParser Exp
ruleClosureTrait rhs = rule "closure trait" $ do
    let names | rhs       = " BEGIN CHECK INIT FIRST "
              | otherwise = " BEGIN CHECK INIT FIRST END "
    name    <- tryChoice $ map symbol $ words names
    block   <- ruleBlock
    let (fun, names) = extract block []
    -- Check for placeholder vs formal parameters
    unless (null names) $
        fail "Closure traits take no formal parameters"
    let code = VCode mkSub { subName = name, subBody = fun } 
    case name of
        "END"   -> do
	    -- We unshift END blocks to @*END at compile-time.
	    -- They're then run at the end of runtime or at the end of the
	    -- whole program.
	    unsafeEvalExp $ App (Var "&unshift") [Var "@*END"] [Syn "sub" [Val code]]
        "BEGIN" -> do
	    -- We've to exit if the user has written code like BEGIN { exit }.
	    possiblyExit =<< unsafeEvalExp fun
	"CHECK" -> vcode2checkBlock code
	"INIT"  -> vcode2initBlock code
	"FIRST" -> vcode2firstBlock code
        _       -> fail ""

-- | If we've executed code like @BEGIN { exit }@, we've to run all @\@*END@
--   blocks and then exit. Returns the input expression if there's no need to
--   exit.
possiblyExit :: Exp -> RuleParser Exp
possiblyExit (Val (VControl (ControlExit exit))) = do
    -- Run all @*END blocks...
    unsafeEvalExp $ Syn "for"
	[ Var "@*END"
	, Syn "sub"
	    [ Val . VCode $ mkSub
		{ subBody   = App (Var "$_") [] []
		, subParams = [defaultScalarParam]
		}
	    ]
	]
    -- ...and then exit.
    return $ unsafePerformIO $ exitWith exit
possiblyExit x = return x

vcode2firstBlock :: Val -> RuleParser Exp
vcode2firstBlock code = do
    -- Ok. Now the tricky thing.
    -- This is the general idea:
    -- FIRST { 42 } is transformed into
    -- {
    --   state $?FIRST_RESULT;
    --   state $?FIRST_RUN;
    --   $?FIRST_RUN || { $?FIRST_RUN++; $?FIRST_RESULT = { 42 }() };
    --   $?FIRST_RESULT;
    -- }
    -- These are the two state variables we need.
    -- This will soon add our two state vars to our pad
    lexDiff <- unsafeEvalLexDiff $
        (Sym SState "$?FIRST_RESULT") . (Sym SState "$?FIRST_RUN") $ emptyExp
    -- And that's the transformation part.
    return $ Syn "block"        -- The outer block
        [ Pad SState lexDiff $  -- state ($?FIRST_RESULT, $?FIRST_RUN);
            Stmts (App (Var "&infix:||")    --  $?FIRST_RUN ||
                [ Var "$?FIRST_RUN"
                , Stmts (App (Var "&postfix:++") [Var "$?FIRST_RUN"] [])
                        (Syn "=" [Var "$?FIRST_RESULT", App (Val code) [] []])
                ] [])   --  { $?FIRST_RUN++; $?FIRST_RESULT = { 42 }() };
            (Var "$?FIRST_RESULT") --  $?FIRST_RESULT;
        ]

vcode2initBlock :: Val -> RuleParser Exp
vcode2initBlock code = vcode2initOrCheckBlock "@?INIT" code

vcode2checkBlock :: Val -> RuleParser Exp
vcode2checkBlock code = vcode2initOrCheckBlock "@?CHECK" code

vcode2initOrCheckBlock :: String -> Val -> RuleParser Exp
vcode2initOrCheckBlock magicalVar code = do
    -- Similar as with FIRST {...}, we transform our input:
    -- my $x = INIT { 42 }   is transformed into
    -- BEGIN { push @?INIT, { FIRST { 42 } } }; my $x = @?INIT[(index)]();
    -- Or, with CHECK:
    -- my $x = CHECK { 42 }  is transformed into
    -- BEGIN { push @?CHECK, { FIRST { 42 } } }; my $x = @?CHECK[(index)]();
    -- This is the inner FIRST {...} block we generate.
    body <- vcode2firstBlock code
    rv <- unsafeEvalExp $
	-- BEGIN { push @?INIT, { FIRST {...} } }
	App (Var "&push") [Var magicalVar] [Syn "sub" [Val $ VCode mkSub { subBody = body }]]
    -- rv is the return value of the push. Now we extract the actual num out of it:
    let (Val (VInt elems)) = rv
    -- Finally, we can return the transformed expression.
    -- elems is the new number of elems in @?INIT (as push returns the new
    -- number of elems), but we're interested in the index, so we -1 it.
    return $ App (Syn "[]" [Var magicalVar, Val . VInt $ elems - 1]) [] []

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

-- Constructs ------------------------------------------------

ruleConstruct :: RuleParser Exp
ruleConstruct = rule "construct" $ tryChoice
    [ ruleForConstruct
    , ruleLoopConstruct
    , ruleCondConstruct
    , ruleWhileUntilConstruct
    , ruleStandaloneBlock
    , ruleGivenConstruct
    , ruleWhenConstruct
    , ruleDefaultConstruct
    , yadaLiteral
    ]

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
    env <- getState
    let sub = MkCode
            { isMulti       = False
            , subName       = "<anon>"
            , subEnv        = Just env
            , subType       = typ
            , subAssoc      = "pre"
            , subReturns    = anyType
            , subLValue     = False -- XXX "is rw"
            , subParams     = if null params then [defaultArrayParam] else params
            , subBindings   = []
            , subSlurpLimit = []
            , subBody       = fun
            , subCont       = Nothing
            }
    return (Syn "sub" [Val $ VCode sub])

ruleBlockFormalStandard :: RuleParser (SubType, Maybe [Param])
ruleBlockFormalStandard = rule "standard block parameters" $ do
    styp <- choice
        [ do { symbol "sub"; return SubRoutine }
        , do { symbol "coro"; return SubCoroutine }
        ]
    params <- option Nothing $ ruleSubParameters ParensMandatory
    return $ (styp, params)

ruleBlockFormalPointy :: RuleParser (SubType, Maybe [Param])
ruleBlockFormalPointy = rule "pointy block parameters" $ do
    symbol "->"
    params <- ruleSubParameters ParensOptional
    return $ (SubBlock, params)



















-- Not yet transcribed ------------------------------------------------


tightOperators :: RuleParser [[Operator Char Env Exp]]
tightOperators = do
  [_, optionary, namedUnary, preUnary, postUnary, infixOps] <- currentTightFunctions
  return $
    [ methOps  " . .+ .? .* .+ .() .[] .{} .<<>> .= "   -- Method postfix
    , postOps  " ++ -- " ++ preOps " ++ -- "            -- Auto-Increment
    , rightOps " ** "                                   -- Exponentiation
    , preSyn "* **"                                     -- Symbolic Unary
      ++ preOps (concatMap (\x -> " -" ++ [x]) "rwxoRWXOezsfdlpSbctugkTBMAC")
      ++ preOps " = ! + - ~ ? +^ ~^ ?^ \\ "
      ++ preSymOps preUnary
      ++ postOps postUnary
    , leftOps  " * / % x xx +& +< +> ~& ~< ~> "         -- Multiplicative
    , leftOps  " + - ~ +| +^ ~| ~^ ?| " -- Additive
      ++ leftOps infixOps                               -- User defined ops
    , listOps  " & "                                    -- Junctive And
    , listOps  " ^ | "                                  -- Junctive Or
    , optOps optionary, preOps namedUnary               -- Named Unary
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

currentFunctions :: RuleParser [(Var, VStr, Params)]
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
                    MkRef (ICode cv)
                        | code_assoc cv == "pre" || code_type cv /= SubPrim
                        -> Just (name', code_assoc cv, code_params cv)
                    MkRef (IScalar sv)
                        | Just (VCode cv) <- scalar_const sv
                        , code_assoc cv == "pre" || code_type cv /= SubPrim
                        -> Just (name', code_assoc cv, code_params cv)
                    _ -> Nothing

currentTightFunctions :: RuleParser [String]
currentTightFunctions = do
    env     <- getState
    case envStash env of
        "" -> do
            funs <- currentTightFunctions'
            setState env{ envStash = unlines funs }
            return funs
        lns -> do
            return $ lines lns

currentTightFunctions' :: RuleParser [String]
currentTightFunctions' = do
    funs    <- currentFunctions
    let (unary, rest) = (`partition` funs) $ \x -> case x of
            (_, "pre", [param]) | not (isSlurpy param) -> True
            _ -> False
        (maybeNullary, notNullary) = (`partition` funs) $ \x -> case x of
            (_, "pre", []) -> True
            _ -> False
        rest' = (`filter` rest) $ \x -> case x of
            (_, _, (_:_:_)) -> True
            (_, _, [param])
                | ('@':_) <- paramName param
                , isSlurpy param -> True
            _ -> False
        namesFrom = map (\(name, _, _) -> name)
        restNames = Set.fromList $ namesFrom rest'
        notNullaryNames = Set.fromList $ namesFrom notNullary
        nullary = filter (not . (`Set.member` notNullaryNames)) $ namesFrom maybeNullary
        (optionary, unary') = mapPair (map snd) . partition fst . sort $
            [ (isOptional param, name) | (name, _, [param]) <- unary
            , not (name `Set.member` restNames)
            ]
        (namedUnary, preUnary, postUnary) = foldr splitUnary ([],[],[]) unary'
        splitUnary ('p':'r':'e':'f':'i':'x':':':op) (n, pre, post) = (n, (op:pre), post)
        splitUnary ('p':'o':'s':'t':'f':'i':'x':':':op) (n, pre, post) = (n, pre, (op:post))
        splitUnary op (n, pre, post) = ((op:n), pre, post)
        -- Then we grep for the &infix:... ones.
        (infixs, _) = (`partition` rest) $ \x -> case x of
                ('i':'n':'f':'i':'x':':':_, _, _) -> True
                _  -> False
        infixOps = map (\(name, _, _) -> drop 6 name) infixs
        mapPair f (x, y) = (f x, f y)
    -- Finally, we return the names of the ops.
    -- But we've to s/^infix://, as we've to return (say) "+" instead of "infix:+".
    return $ map (encodeUTF8 . unwords . nub)
        [nullary, optionary, namedUnary, preUnary, postUnary, infixOps]

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
ops f s = [f n | n <- sortBy revLength (nub . words $ decodeUTF8 s)]
    where
    revLength x y = compare (length y) (length x)

doApp :: String -> [Exp] -> Exp
doApp str args = App (Var str) args []

doAppSym :: String -> [Exp] -> Exp
doAppSym name@(_:'p':'r':'e':'f':'i':'x':':':_) args = App (Var name) args []
doAppSym (sigil:name) args = App (Var (sigil:("prefix:"++name))) args []
doAppSym _ _ = error "doAppSym: bad name"

preSyn      :: String -> [Operator Char Env Exp]
preSyn      = ops $ makeOp1 Prefix "" Syn
preOps      :: String -> [Operator Char Env Exp]
preOps      = (ops $ makeOp1 Prefix "&prefix:" doApp) . addHyperPrefix
preSymOps   :: String -> [Operator Char Env Exp]
preSymOps   = (ops $ makeOp1 Prefix "&prefix:" doAppSym) . addHyperPrefix
postOps     :: String -> [Operator Char Env Exp]
postOps     = (ops $ makeOp1 Postfix "&postfix:" doApp) . addHyperPostfix
optOps      :: String -> [Operator Char Env Exp]
optOps      = (ops $ makeOp1 OptionalPrefix "&prefix:" doApp) . addHyperPrefix
leftOps     :: String -> [Operator Char Env Exp]
leftOps     = (ops $ makeOp2 AssocLeft "&infix:" doApp) . addHyperInfix
rightOps    :: String -> [Operator Char Env Exp]
rightOps    = (ops $ makeOp2 AssocRight "&infix:" doApp) . addHyperInfix
noneOps     :: String -> [Operator Char Env Exp]
noneOps     = ops $ makeOp2 AssocNone "&infix:" doApp
listOps     :: String -> [Operator Char Env Exp]
listOps     = ops $ makeOp2 AssocLeft "&infix:" doApp
chainOps    :: String -> [Operator Char Env Exp]
chainOps    = (ops $ makeOp2 AssocLeft "&infix:" doApp) . addHyperInfix
rightSyn    :: String -> [Operator Char Env Exp]
rightSyn    = ops $ makeOp2 AssocRight "" Syn
noneSyn     :: String -> [Operator Char Env Exp]
noneSyn     = ops $ makeOp2 AssocNone "" Syn
listSyn     :: String -> [Operator Char Env Exp]
listSyn     = ops $ makeOp0 AssocList "" Syn

addHyperInfix :: String -> String
addHyperInfix = unwords . concatMap hyperForm . words
    where
    hyperForm op = [op, ">>" ++ op ++ "<<", "»" ++ op ++ "«"]

addHyperPrefix :: String -> String
addHyperPrefix = unwords . concatMap hyperForm . words
    where
    hyperForm op = [op, op ++ "<<", op ++ "«"]

addHyperPostfix :: String -> String
addHyperPostfix = unwords . concatMap hyperForm . words
    where
    hyperForm op = [op, ">>" ++ op, "»" ++ op]

-- chainOps    = ops $ makeOpChained

makeOp1 :: (RuleParser (Exp -> a) -> b) -> 
        String -> 
        (String -> [Exp] -> a) -> 
        String -> 
        b
makeOp1 prec sigil con name = prec $ try $ do
    symbol name
    -- `int(3)+4` should not be parsed as `int((3)+4)`
    when (isWordAny (last name)) $ try $ choice
        [ do { char '('; unexpected "(" } 
        , do { string "=>"; unexpected "=>" } 
        , return ()
        ]
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
        [ ruleDereference
        , ruleVar
        , ruleApply True    -- Folded metaoperators
        , ruleLit
        , ruleClosureTrait True
        , ruleTypeVar
        , ruleTypeLiteral
        , ruleApply False   -- Normal application
        , parens ruleExpression
        ]
    fs <- many rulePostTerm
    return $ combine (reverse fs) term

ruleTypeVar :: RuleParser Exp
ruleTypeVar = rule "type" $ try $ do
    string "::"
    name <- ruleQualifiedIdentifier
    return . Val . VType $ mkType name

ruleTypeLiteral :: RuleParser Exp
ruleTypeLiteral = rule "type" $ do
    env     <- getState
    name    <- tryChoice [
        do { symbol n; notFollowedBy (alphaNum <|> char ':'); return n }
        | (MkType n) <- flatten (envClasses env) ]
    return . Val . VType $ mkType name

rulePostTerm :: RuleParser (Exp -> Exp)
rulePostTerm = tryVerbatimRule "term postfix" $ do
    hasDot <- option False $ try $ do
        whiteSpace; char '.'; notFollowedBy (char '.'); return True
    choice $ (if hasDot then [ruleInvocation] else []) ++
        [ ruleArraySubscript
        , ruleHashSubscript
        , ruleCodeSubscript
        ]

ruleInvocation :: RuleParser (Exp -> Exp)
ruleInvocation = tryVerbatimRule "invocation" $ do
    colon       <- maybeColon
    hasEqual    <- option False $ do char '='; whiteSpace; return True
    name        <- do { str <- ruleSubName; return $ colon str }
    (invs,args) <- option ([],[]) $ parseParenParamList False
    return $ \x -> if hasEqual
        then Syn "=" [x, App (Var name) (x:invs) args]
        else App (Var name) (x:invs) args

ruleInvocationParens :: RuleParser (Exp -> Exp)
ruleInvocationParens = do
    colon       <- maybeColon
    hasEqual    <- option False $ do { char '='; whiteSpace; return True }
    name        <- do { str <- ruleSubName; return $ colon str }
    (invs,args) <- verbatimParens $ parseNoParenParamList False
    -- XXX we just append the adverbial block onto the end of the arg list
    -- it really goes into the *& slot if there is one. -lp
    return $ \x -> if hasEqual
        then Syn "=" [x, App (Var name) (x:invs) args]
        else App (Var name) (x:invs) args

ruleArraySubscript :: RuleParser (Exp -> Exp)
ruleArraySubscript = tryVerbatimRule "array subscript" $ do
    between (symbol "[") (char ']') $ option id $ do
        exp <- ruleExpression; return $ \x -> Syn "[]" [x, exp]

ruleHashSubscript :: RuleParser (Exp -> Exp)
ruleHashSubscript = tryVerbatimRule "hash subscript" $ do
    choice [ ruleHashSubscriptBraces, ruleHashSubscriptQW ]

ruleHashSubscriptBraces :: RuleParser (Exp -> Exp)
ruleHashSubscriptBraces = do
    between (symbol "{") (char '}') $ option id $ do
        exp <- ruleExpression; return $ \x -> Syn "{}" [x, exp]

ruleHashSubscriptQW :: RuleParser (Exp -> Exp)
ruleHashSubscriptQW = do
    exp <- angleBracketLiteral
    return $ \x -> Syn "{}" [x, exp]

ruleCodeSubscript :: RuleParser (Exp -> Exp)
ruleCodeSubscript = tryVerbatimRule "code subscript" $ do
    (invs,args) <- parens $ parseParamList
    return $ \x -> App x invs args

ruleApply :: Bool -> RuleParser Exp
ruleApply isFolded = tryVerbatimRule "apply" $ do
    (colon, implicitInv) <- option (id, []) $ do
        when isFolded $ fail ""
        char '.'
        option (id, [Var "$_"]) $ choice
            [ do { char '/'; return (id, [Var "$?SELF"]) }
            , do char ':'
                 return ( \(sigil:name) -> (sigil:':':name)
                        , [Var "$?SELF"]
                        )
            ]
    name    <- if isFolded then ruleFoldOp else fmap colon ruleSubName
    when ((name ==) `any` words " &if &unless &while &until &for ") $
        fail "reserved word"
    hasDot  <- option False $ try $ do { whiteSpace; char '.'; return True }
    (invs, args) <- if hasDot
        then parseNoParenParamList (null implicitInv)
        else parseParenParamList (null implicitInv) <|> do { whiteSpace; parseNoParenParamList (null implicitInv) }
    return $ App (Var name) (implicitInv ++ invs) args

ruleFoldOp :: RuleParser String
ruleFoldOp = verbatimRule "reduce metaoperator" $ do
    char '['
    [_, _, _, _, _, infixOps] <- currentTightFunctions
    name <- tryChoice $ ops string (addHyperInfix $ infixOps ++ defaultInfixOps)
    char ']'
    -- XXX: I don't know why the "«" doesn't work. [+]<< parses fine, but [+]«
    -- does not.
    possiblyHyper <- many $ string "«" <|> string "<<"
    return $ "&prefix:[" ++ name ++ "]" ++ concat possiblyHyper
    where
    defaultInfixOps = concat
        [ " ** * / % x xx +& +< +> ~& ~< ~> "
        , " + - ~ +| +^ ~| ~^ ?| , "
        , " & ^ | "
        , " => "
        , " != == < <= > >= ~~ !~ "
        , " eq ne lt le gt ge =:= "
        , " && !! "
        , " || ^^ // "
	, " and nor or xor err "
	, " .[] .{} "
        ]

parseParamList :: RuleParser ([Exp], [Exp])
parseParamList = parseParenParamList True <|> parseNoParenParamList True

parseParenParamList :: Bool -> RuleParser ([Exp], [Exp])
parseParenParamList defaultToInvs = do
    params      <- option Nothing . fmap Just $
        verbatimParens $ parseHasParenParamList defaultToInvs
    block       <- option [] ruleAdverbBlock
    when (isNothing params && null block) $ fail ""
    let (inv, norm) = maybe ([], []) id params
    -- XXX we just append the adverbial block onto the end of the arg list
    -- it really goes into the *& slot if there is one. -lp
    processFormals False [inv, norm ++ block]

ruleAdverbBlock :: RuleParser [Exp]
ruleAdverbBlock = tryRule "adverbial block" $ do
    char ':'
    rblock <- ruleBlockLiteral
    next <- option [] ruleAdverbBlock
    return (rblock:next)

parseHasParenParamList :: Bool -> RuleParser ([Exp], [Exp])
parseHasParenParamList defaultToInvs = do
    formal <- (`sepEndBy` symbol ":") $ fix $ \rec -> do
        rv <- option Nothing $ fmap Just $ do
            x <- parseLitOp
            return (x, symbol ",")
        case rv of
            Nothing           -> return []
            Just (exp, trail) -> do
                rest <- option [] $ do { trail; rec }
                return (exp:rest)
    processFormals defaultToInvs formal

parseNoParenParamList :: Bool -> RuleParser ([Exp], [Exp])
parseNoParenParamList defaultToInvs = do
    formal <- (`sepEndBy` symbol ":") $ fix $ \rec -> do
        rv <- option Nothing $ do
            fmap Just $ tryChoice
                [ do x <- ruleBlockLiteral
                     lookAhead (satisfy (/= ','))
                     return (x, return "")
                , do x <- parseTightOp
                     return (x, symbol ",")
                ]
        case rv of
            Nothing           -> return []
            Just (exp, trail) -> do
                rest <- option [] $ do { trail; rec }
                return (exp:rest)
    processFormals defaultToInvs formal

processFormals :: Monad m => Bool -> [[Exp]] -> m ([Exp], [Exp])
processFormals defaultToInvs formal = do
    case formal of
        []                  -> return ([], [])
        [invocants]         -> return $ if defaultToInvs
	    then (unwind invocants, [])
	    else ([], unwind invocants)
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

ruleParamName :: RuleParser String
ruleParamName = literalRule "parameter name" $ do
    sigil   <- oneOf "$@%&:"
    twigil  <- ruleTwigil
    name    <- many1 wordAny
    return $ (sigil:twigil) ++ name

ruleVarName :: RuleParser String
ruleVarName = rule "variable name" ruleVarNameString

ruleVarNameString :: RuleParser String
ruleVarNameString =   try (string "$!")  -- error variable
                  <|> try (string "$/")  -- match object
                  <|> try ruleMatchPos
                  <|> try ruleMatchNamed
                  <|> do
    sigil   <- oneOf "$@%&"
    if sigil == '&' then ruleSubName else do
    --  ^ placeholder, * global, ? magical, . member, : private member
    twigil  <- ruleTwigil
    names   <- many1 wordAny `sepBy1` (try $ string "::")
    return $ (sigil:twigil) ++ concat (intersperse "::" names)

ruleTwigil :: RuleParser String
ruleTwigil = option "" . choice . map string $ words " ^ * ? . : "

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
    return $ (sigil:twigil:name) ++ ">"

ruleDereference :: RuleParser Exp
ruleDereference = try $ do
    sigil   <- oneOf "$@%&"
    exp     <- ruleDereference <|> ruleVar <|> braces ruleExpression
    return $ Syn (sigil:"{}") [exp]

ruleVar :: RuleParser Exp
ruleVar = try ruleNormalVar <|> ruleSymbolicDeref
    where
    ruleNormalVar = ruleVarNameString >>= return . makeVar

ruleSymbolicDeref :: RuleParser Exp
ruleSymbolicDeref = do
    sigil    <- oneOf "$@%&"
    nameExps <- many1 $ do
	string "::"
        -- nameExp is the expression which will yield the varname.
	(parens ruleExpression) <|> (liftM (Val . VStr) $ many1 wordAny)
    return $ Syn (sigil:"::()") nameExps

makeVar :: String -> Exp
makeVar "$<>" = Var "$/"
makeVar ('$':rest) | all (`elem` "1234567890") rest =
    Syn "[]" [Var "$/", Val $ VInt (read rest)]
makeVar ('$':'<':name) =
    Syn "{}" [Var "$/", doSplitStr (init name)]
makeVar (sigil:'.':name) =
    Cxt (cxtOfSigil sigil) (Syn "{}" [Var "$?SELF", Val (VStr name)])
makeVar (sigil:':':name) =
    Cxt (cxtOfSigil sigil) (Syn "{}" [Var "$?SELF", Val (VStr name)])
makeVar var = Var var

ruleLit :: RuleParser Exp
ruleLit = choice
    [ ruleDoBlock
    , ruleBlockLiteral
    , numLiteral
    , emptyListLiteral
    , emptyArrayLiteral
    , arrayLiteral
    , pairLiteral
    , undefLiteral
    , namedLiteral "NaN"    (VNum $ 0/0)
    , namedLiteral "Inf"    (VNum $ 1/0)
    , yadaLiteral
    , qLiteral
    , rxLiteral
    , rxLiteralBare
    , substLiteral
    , nullaryLiteral
    ]

nullaryLiteral :: RuleParser Exp
nullaryLiteral = try $ do
    (nullary:_) <- currentTightFunctions
    name <- choice $ map symbol $ words nullary
    notFollowedBy (char '(')
    return $ App (Var ('&':name)) [] []

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
    val <- parseTightOp
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
    
    doSplit expr = App (Var "&infix:~~") [expr, rxSplit] []
    rxSplit = Syn "rx" $
        [ Val $ VStr "(\\S+)"
        , Val $ VList
            [ castV (VStr "P5", VInt 1)
            , castV (VStr "g", VInt 1)
            , castV (VStr "stringify", VInt 1)
            ]
        ]

-- | splits the string into expressions on whitespace.
-- Implements the <> operator at parse-time.
doSplitStr :: String -> Exp
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
rxP5Flags = MkQFlags QS_No True True True True False QB_No '/' True False
-- | Default flags
rxP6Flags :: QFlags
rxP6Flags = MkQFlags QS_No False False False False False QB_No '/' False False

-- Regexps

-- | A parser returning a regex, given a hashref of adverbs and a closing delimiter.
rxLiteralAny :: Exp -> Char -> RuleParser Exp
rxLiteralAny adverbs
    | Syn "\\{}" [Syn "," pairs] <- adverbs
    , not (null [
        True
        | (App (Var "&infix:=>") [Val (VStr name), _] []) <- pairs
        , (name ==) `any` words "P5 Perl5 perl5"
        ])
    = rxLiteral5
    | otherwise
    = rxLiteral6

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
    expr    <- rxLiteralAny adverbs endch
    ch      <- if ch == endch then return ch else do { whiteSpace ; anyChar }
    let endch = balancedDelim ch
    subst   <- qLiteral1 (char endch) qqFlags { qfProtectedChar = endch }
    return $ Syn "subst" [expr, subst, adverbs]

rxLiteral :: RuleParser Exp
rxLiteral = try $ do
    sym     <- symbol "rx" <|> do { symbol "m"; return "match" } <|> do
        symbol "rule"
        lookAhead $ do { ruleAdverbHash; char '{' }
        return "rx"
    adverbs <- ruleAdverbHash
    ch      <- anyChar
    expr    <- rxLiteralAny adverbs $ balancedDelim ch
    return $ Syn sym [expr, adverbs]

rxLiteralBare :: RuleParser Exp
rxLiteralBare = try $ do
    ch      <- char '/'
    expr    <- rxLiteral6 $ balancedDelim ch
    return $ Syn "//" [expr, Val undef]

namedLiteral :: String -> Val -> RuleParser Exp
namedLiteral n v = do { symbol n; return $ Val v }

yadaLiteral :: RuleParser Exp
yadaLiteral = expRule $ do
    sym  <- choice . map symbol $ words " ... ??? !!! "
    return $ App (Var $ doYada sym) [Val $ VStr (sym ++ " - not yet implemented")] []
    where
    doYada "..." = "&fail_" -- XXX rename to fail() eventually
    doYada "???" = "&warn"
    doYada "!!!" = "&die"
    doYada _ = error "Bad yada symbol"

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
    Left err    -> env { envBody = Val $ VError msg [mkPos pos pos] }
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
    
