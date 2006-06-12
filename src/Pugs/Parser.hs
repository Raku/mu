{-# OPTIONS_GHC -cpp -fglasgow-exts -funbox-strict-fields -fno-full-laziness -fno-cse #-}

{-|
    Higher-level parser for building ASTs.

>   I sang of leaves, of leaves of gold, and leaves of gold there grew:
>   Of wind I sang, a wind there came and in the branches blew.
>   Beyond the Sun, beyond the Moon, the foam was on the Sea,
>   And by the strand of Ilmarin there grew a golden Tree...
-}

module Pugs.Parser (
    ruleBlockBody,
    possiblyExit,
    module Pugs.Lexer,
    module Pugs.Parser.Types,
    module Pugs.Parser.Unsafe,
    module Pugs.Parser.Operator,
) where
import Pugs.Internals
import Pugs.AST
import Pugs.Types
import Pugs.Version (versnum)
import Pugs.Lexer
import Pugs.Rule
import Pugs.Rule.Expr

import Pugs.Parser.Types
import Pugs.Parser.Number
import Pugs.Parser.Unsafe
import Pugs.Parser.Export
import Pugs.Parser.Operator
import Pugs.Parser.Doc
import Pugs.Parser.Literal
import Pugs.Parser.Util

import qualified Data.Map as Map

-- Lexical units --------------------------------------------------

ruleBlock :: RuleParser Exp
ruleBlock = do
    lvl <- gets ruleBracketLevel
    case lvl of
        StatementBracket    -> ruleBlock'
        _                   -> lexeme ruleVerbatimBlock
    where
    ruleBlock' = do
        rv <- ruleVerbatimBlock
        -- Implementation of 'line-ending } terminates statement'
        -- See L<S04/Statement-ending blocks>.
        -- We are now at end of closing '}'. Mark the position...
        prevPos <- getPosition
        -- Skip whitespaces.  If we go into another line,
        -- then it's statement-level break
        whiteSpace
        currPos <- getPosition
        when (sourceLine prevPos /= sourceLine currPos) $ do
            -- Manually insert a ';' symbol here!
            insertIntoPosition ';' 
        return rv

ruleVerbatimBlock :: RuleParser Exp
ruleVerbatimBlock = verbatimRule "block" $ do
    body <- verbatimBraces ruleBlockBody
    return $ Syn "block" [body]

ruleEmptyExp :: RuleParser Exp
ruleEmptyExp = expRule $ do
    symbol ";"
    return emptyExp

ruleBlockBody :: RuleParser Exp
ruleBlockBody =
  localEnv $ do
    whiteSpace
    pre     <- many ruleEmptyExp
    body    <- option emptyExp ruleStatementList
    post    <- many ruleEmptyExp
    whiteSpace
    return $ foldl1 mergeStmts (pre ++ [body] ++ post)

{-|
Match a single statement (not including any terminating semicolon).  A
statement consists of a single 'ruleExpression', followed by an optional
statement-modifier (e.g. @if $foo@ or @for \@baz@).

One of the sub-rules used by 'ruleStatementList'.
-}
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
ruleStatementList = rule "statements" .
    enterBracketLevel StatementBracket .
        sepLoop $ choice
            [ noSep     ruleDocBlock
            , nonSep    ruleBlockDeclaration
            , semiSep   ruleDeclaration
            , nonSep    ruleConstruct
            , semiSep   ruleStatement
            ]
    where
    nonSep  = doSep many  -- must be followed by 0+ semicolons
    semiSep = doSep many1 -- must be followed by 1+ semicolons
    noSep r = fmap (\x -> (x, False)) r
    doSep sepCount r = do
        exp <- r
        terminate <- option True $ do
            sepCount $ symbol ";"
            return False
        return (exp, terminate)
    sepLoop rule = do
        whiteSpace
        (eof >> return Noop) <|> do
            (exp, terminate) <- rule
            if terminate then return exp else do
            rest <- option Noop (sepLoop rule)
            return $ exp `mergeStmts` rest

-- Declarations ------------------------------------------------

ruleBlockDeclaration :: RuleParser Exp
ruleBlockDeclaration = rule "block declaration" $ choice
    [ ruleSubDeclaration
    , ruleClosureTrait False
    , ruleRuleDeclaration
    , rulePackageBlockDeclaration
    ]

ruleDeclaration :: RuleParser Exp
ruleDeclaration = rule "declaration" $ choice
    [ rulePackageDeclaration
    , ruleMemberDeclaration
    , ruleTraitDeclaration
    , ruleUseDeclaration
    , ruleNoDeclaration
    , ruleInlineDeclaration
    , ruleRequireDeclaration
    , ruleTrustsDeclaration
    ]

ruleSubHead :: RuleParser (Bool, SubType, String)
ruleSubHead = rule "subroutine head" $ do
    isMulti <- option False $ do { symbol "multi" ; return True }
    -- You're allowed to omit the "sub":
    --   multi sub foo (...) {...}      # legal
    --         sub foo (...) {...}      # legal, too
    let implicitSub | isMulti == True = return SubRoutine
                    | otherwise       = pzero
    styp    <- choice
        [ do symbol "sub"
             return SubRoutine
        , do symbol "coro"
             return SubCoroutine
        , do (symbol "submethod" <|> symbol "method")
             return SubMethod
        , do symbol "macro"
             return SubMacro
        ] <|> implicitSub
    name    <- ruleSubName
    return (isMulti, styp, name)

-- | Scope, context, isMulti, styp, name
type SubDescription = (Scope, String, Bool, SubType, String)

ruleSubScopedWithContext :: RuleParser SubDescription
ruleSubScopedWithContext = tryRule "scoped subroutine with context" $ do
    scope   <- ruleScope
    cxt     <- identifier
    (isMulti, styp, name) <- ruleSubHead
    return (scope, cxt, isMulti, styp, name)

ruleSubScoped :: RuleParser SubDescription
ruleSubScoped = tryRule "scoped subroutine" $ do
    scope <- ruleScope
    (isMulti, styp, name) <- ruleSubHead
    return (scope, "Any", isMulti, styp, name)

ruleSubGlobal :: RuleParser SubDescription
ruleSubGlobal = tryRule "global subroutine" $ do
    (isMulti, styp, name) <- ruleSubHead
    return (SGlobal, "Any", isMulti, styp, name)

ruleRuleDeclaration :: RuleParser Exp
ruleRuleDeclaration = rule "rule declaration" $ do
    -- XXX - fill in default adverbs
    name    <- try (ruleRegexDeclarator >> identifier)
    adverbs <- ruleAdverbHash
    ch      <- char '{'
    expr    <- rxLiteralAny adverbs ch (balancedDelim ch)
    let exp = Syn ":=" [Var ('<':'*':name), Syn "rx" [expr, adverbs]]
    unsafeEvalExp (Sym SGlobal ('<':'*':name) exp)
    return emptyExp

rulePackageBlockDeclaration :: RuleParser Exp
rulePackageBlockDeclaration = rule "package block declaration" $ do
    -- scope <- option Nothing $ fmap Just ruleScope
    rv <- try $ do
        optional ruleScope -- XXX - not handled yet
        rv <- rulePackageHead
        lookAhead (char '{')
        return rv
    case rv of 
        Right (_, kind, pkgVal, env) -> do
            body <- verbatimBraces ruleBlockBody
            env' <- ask
            putRuleEnv env'{ envPackage = envPackage env }
            return $ Syn "namespace" [kind, pkgVal, body]
        Left err -> fail err

rulePackageDeclaration :: RuleParser Exp
rulePackageDeclaration = rule "package declaration" $ do
    -- scope <- option Nothing $ fmap Just ruleScope
    rv <- try $ do
        optional ruleScope -- XXX - not handled yet
        rulePackageHead
    case rv of
        Right (_, kind, pkgVal, _) -> return $ Syn "package" [kind, pkgVal]
        Left err -> fail err

rulePackageHead :: RuleParser (Either String (String, Exp, Exp, Env))
rulePackageHead = do
    scope <- option Nothing $ fmap Just ruleScope
    sym <- choice $ map symbol (words "package module class role grammar")
    name    <- ruleQualifiedIdentifier
    optional ruleVersionPart -- v
    optional ruleAuthorPart  -- a
    whiteSpace
    env <- ask
    newName <- case scope of
        Just SOur -> return $ envPackage env ++ "::" ++ name
        Nothing   -> return name
        _         -> fail "I only know about package- and global-scoped classes. Sorry."
    traits  <- many $ ruleTrait
    let pkgClass = case sym of
                       "package" -> "Package"
                       "module"  -> "Module"
                       "class"   -> "Class"
                       "role"    -> "Class" -- XXX - Wrong - need metamodel
                       "grammar" -> "Grammar"
                       _ -> fail "bug"
        parentClasses = nub ("Object":traits)
    if (elem name parentClasses)
        then return (Left $ "Circular inheritance detected for " ++ sym ++ " '" ++ name ++ "'")
        else do
            unsafeEvalExp (newPackage pkgClass newName parentClasses)
            modify $ \state -> state
                { ruleEnv = (ruleEnv state)
                    { envPackage = newName
                    , envClasses = envClasses env `addNode` mkType newName
                    }
                , ruleDynParsers = MkDynParsersEmpty
                }
            let pkgVal = Val . VStr $ newName
                kind   = Val . VStr $ sym
            return $ Right (newName, kind, pkgVal, env)

ruleSubDeclaration :: RuleParser Exp
ruleSubDeclaration = rule "subroutine declaration" $ do
    namePos <- getPosition
    (scope, typ, isMulti, styp, name) <- choice
        [ ruleSubScopedWithContext
        , ruleSubScoped
        , ruleSubGlobal
        ]
    optional $ do { symbol "handles"; ruleExpression }
    typ'    <- option typ $ try $ ruleBareTrait "returns"
    formal  <- option Nothing $ ruleSubParameters ParensMandatory
    typ''   <- option typ' $ try $ ruleBareTrait "returns"
    traits  <- many $ ruleTrait

    -- XXX - We have the prototype now; install it immediately?

    -- bodyPos <- getPosition
    body    <- ruleBlock
    let (fun, names, params) = doExtract styp formal body
    -- Check for placeholder vs formal parameters
    when (isJust formal && (not.null) names) $
        fail "Cannot mix placeholder variables with formal parameters"
    env <- ask
    let sub = VCode $ MkCode
            { isMulti       = isMulti
            , subName       = nameQualified
            , subEnv        = Just env
            , subType       = if "primitive" `elem` traits
                then SubPrim else styp
            , subAssoc      = "pre"
            , subReturns    = mkType typ''
            , subLValue     = "rw" `elem` traits
            , subParams     = self ++ paramsFor styp formal params
            , subBindings   = []
            , subSlurpLimit = []
            , subBody       = fun
            , subCont       = Nothing
            }
        pkg = envPackage env
        nameQualified | ':' `elem` name     = name
                      | scope <= SMy        = name
                      | isGlobal            = name
                      | isBuiltin           = (head name:'*':tail name)
                      | otherwise           = (head name:pkg) ++ "::" ++ tail name
        self :: [Param]
        self | styp > SubMethod = []
             | (prm:_) <- params, isInvocant prm = []
             | otherwise = [selfParam $ envPackage env]
        mkExp n = Syn ":=" [Var n, Syn "sub" [Val sub]]
        mkSym n = Sym scope (mkMulti n) (mkExp n)
        -- Horrible hack! Sym "&&" is the multi form.
        mkMulti | isMulti   = ('&':)
                | otherwise = id
        isGlobal = '*' `elem` name
        isBuiltin = ("builtin" `elem` traits)
        isExported = ("export" `elem` traits)
        
    -- XXX this belongs in semantic analysis, not in the parser
    -- also, maybe we should only warn when you try to export an
    -- operator that is "standard"
    -- XXX I can't figure out how to do this without trace
    when (isExported && isOperatorName name) $
        trace 
            ("You probably don't want to export an operator name; instead\n\
  define a new variant on the new operator (eg. multi sub *infix:<+>): "
                ++ show name ++ " at " ++ show namePos)
            (return ())
            
    -- Don't add the sub if it's unsafe and we're in safemode.
    if "unsafe" `elem` traits && safeMode then return emptyExp else do
    rv <- case scope of
        SGlobal | isExported -> do
            -- we mustn't perform the export immediately upon parse, because
            -- then only the first consumer of a module will see it. Instead,
            -- make a note of this symbol being exportable, and defer the
            -- actual symbol table manipulation to opEval.
            unsafeEvalExp $ mkSym nameQualified
            -- push %*INC<This::Package><exports><&this_sub>, expression-binding-&this_sub
            --    ==>
            -- push %This::Package::EXPORTS<&this_sub>, expression-binding-&this_sub
            -- (a singleton list for subs, a full list of subs for multis)
            return $
                App (Var "&push")
                    (Just (Syn "{}" [Var ("%" ++ pkg ++ "::EXPORTS"), Val $ VStr name]))
                    [Val sub]
        SGlobal -> do
            unsafeEvalExp $ mkSym nameQualified
            return emptyExp
        _ -> do
            lexDiff <- unsafeEvalLexDiff $ mkSym nameQualified
            addBlockPad scope lexDiff
            return $ mkExp name
    clearDynParsers
    return rv


ruleSubNamePossiblyWithTwigil :: RuleParser String
ruleSubNamePossiblyWithTwigil = verbatimRule "subroutine name" $ try $ do
    twigil  <- ruleTwigil
    name    <- ruleOperatorName <|> ruleQualifiedIdentifier
    return $ "&" ++ twigil ++ name

ruleSubName :: RuleParser String
ruleSubName = verbatimRule "subroutine name" $ do
    twigil  <- option "" (string "*")
    name <- ruleOperatorName <|> ruleQualifiedIdentifier
    return $ "&" ++ twigil ++ name

ruleOperatorName :: RuleParser String
ruleOperatorName = verbatimRule "operator name" $ do
    fixity  <- choice (map (try . string) fixities) `tryLookAhead` (oneOf "\xAB<{")
    name    <- do
        -- char ':'
        sub <- ruleHashSubscript
        -- Not exactly un-evil
        let (Syn "{}" [_, expr]) = sub (Val VUndef)
        Val (VStr name) <- unsafeEvalExp $
            App (Var "&*join") 
            Nothing 
            (Val (VStr " ") : [expr])
        return name
    return $ fixity ++ name


ruleSubParameters :: ParensOption -> RuleParser (Maybe [Param])
ruleSubParameters wantParens = rule "subroutine parameters" $ do
    rv <- ruleParamList wantParens (ruleFormalParam FormalsComplex)
    case rv of
        Just (invs:args:_)  -> return . Just $ map setInv invs ++ args
        _                   -> return Nothing
    where
    setInv e = e { isInvocant = True }

ruleFormalParam :: FormalsOption -> RuleParser Param
ruleFormalParam opt = rule "formal parameter" $ do
    typ     <- option "" $ ruleType
    optional $ char '\\'  -- XXX hack to parse arglist (\$foo)
    sigil1  <- option "" $ choice . map symbol $ words " : * "
    name    <- ruleParamName -- XXX support *[...]
    sigil2  <- option "" $ choice . map symbol $ words " ? ! "
    traits  <- many ruleTrait
    -- sigil' is the canonical form of sigil1 and sigil2, e.g.
    --   $foo is required -->  !$foo
    --  :$foo             --> ?:$foo
    --  :$foo!            --> !:$foo
    --  :$foo is required --> !:$foo
    isDefaultSpecified <- case opt of
        FormalsSimple  -> return False
        FormalsComplex -> option False $ do
            lookAhead $ symbol "="
            return True
    let isOptional = isDefaultSpecified
                  || sigil2 == "?"
                  || sigil1 == ":" && sigil2 /= "!" && "required" `notElem` traits
                  || "optional" `elem` traits
    let sigil'   = (if isOptional then '?' else '!'):sigil1
    exp <- case opt of
        FormalsSimple -> return Noop
        FormalsComplex -> do
            rv <- ruleParamDefault (not isOptional)
            optional $ do
                symbol "-->"
                ruleParamList ParensOptional $ choice
                    [ ruleType
                    , ruleFormalParam FormalsComplex >> return ""
                    ]
            return rv
    return $ foldr appTrait (buildParam typ sigil' name exp) traits
    where
    appTrait "rw"   x = x { isWritable = True }
    appTrait "copy" x = x { isLValue = False, isWritable = True }
    appTrait "lazy" x = x { isLazy = True }
    appTrait _      x = x -- error "unknown trait"

ruleParamDefault :: Bool -> RuleParser Exp
ruleParamDefault True  = return emptyExp
ruleParamDefault False = rule "default value" $ option emptyExp $ do
    symbol "="
    parseExpWithItemOps

ruleTrustsDeclaration :: RuleParser Exp
ruleTrustsDeclaration = do
    symbol "trusts"
    lexeme ruleQualifiedIdentifier
    return emptyExp

ruleTraitDeclaration :: RuleParser Exp
ruleTraitDeclaration = try $ do
    -- XXX horrible hack! "is eval(...), ..." should *not* be parsed as a trait
    -- declaration. So we check whether we're really statement-level, i.e.
    --   is eval(...) [eof]   # trait
    --   is eval(...);        # trait
    --   is eval(...) }       # trait
    --   is eval(...), ...    # sub call
    trait   <- ruleTrait
    lookAhead (eof <|> (oneOf ";}" >> return ()))
    env     <- ask
    let pkg = Var (':':'*':envPackage env)
    return $ Syn "=" [Syn "{}" [pkg, Val (VStr "traits")], Syn "," [Syn "@{}" [Syn "{}" [pkg, Val (VStr "traits")]], Val (VStr trait)]]

ruleMemberDeclaration :: RuleParser Exp
ruleMemberDeclaration = do
    symbol "has"
    typ  <- option "" $ lexeme ruleQualifiedIdentifier
    attr <- ruleVarName
    (sigil:twigil:key) <- case attr of
        (_:'.':_)   -> return attr
        (_:'!':_)   -> return attr
        (x:xs@(twigil:_))
            | (isAlpha twigil) || twigil == '_'
                    -> return (x:'!':xs)
        _           -> fail $ "Invalid member variable name '" ++ attr ++ "'"
    traits  <- many ruleTrait
    optional $ do { symbol "handles"; ruleExpression }
    env     <- ask
    -- manufacture an accessor
    let sub = mkPrim
            { isMulti       = False
            , subName       = name
            , subEnv        = Nothing
            , subReturns    = if null typ then typeOfSigil sigil else mkType typ
            , subBody       = fun
            , subParams     = [selfParam $ envPackage env]
            , subLValue     = "rw" `elem` traits
            , subType       = SubMethod
            }
        exp = Syn ":=" [Var name, Syn "sub" [Val $ VCode sub]]
        name | twigil == '.' = '&':(envPackage env ++ "::" ++ key)
                | otherwise     = '&':(envPackage env ++ "::" ++ (twigil:key))
        fun = Ann (Cxt (cxtOfSigil sigil)) (Syn "{}" [Var "$?SELF", Val (VStr key)])
    unsafeEvalExp (Sym SGlobal name exp)
    return emptyExp

-- was: parseTightOp
parseExpWithTightOps :: RuleParser Exp
parseExpWithTightOps = parseExpWithCachedParser dynParseTightOp

-- was: parseOpWith
parseExpWithCachedParser :: (DynParsers -> RuleParser Exp) -> RuleParser Exp
parseExpWithCachedParser f = do
    state <- getState
    case ruleDynParsers state of
        MkDynParsersEmpty   -> refillCache state f
        p                   -> f p
    where
    refillCache state f = do
        let ?parseExpWithTightOps = parseExpWithTightOps
        ops         <- operators
        opsTight    <- tightOperators
        opsLit      <- litOperators
        nullary:_   <- currentTightFunctions
        let [parse, parseTight, parseLit] = map
                (expRule . (\o -> buildExpressionParser o parseTerm (Syn "" [])))
                [ops, opsTight, opsLit]
            opParsers = MkDynParsers parse parseTight parseLit parseNullary
            parseNullary = try $ do
                name <- choice . map symbol $ nullary
                notFollowedBy (char '(' <|> (char ':' >> char ':'))
                possiblyApplyMacro $ App (Var ('&':name)) Nothing []
        setState state{ ruleDynParsers = opParsers }
        f opParsers

{-
ruleVarDeclaration :: RuleParser Exp
ruleVarDeclaration = rule "variable declaration" $ do
    scope       <- ruleScope
    typename    <- choice
        [ lexeme (optional (string "::") >> ruleQualifiedIdentifier)
        , return ""
        ]  -- Type
    (decl, lhs) <- choice
        [ do -- pos  <- getPosition
             name <- ruleVarName
             return ((Sym scope name), Var name)
        , do names <- parens . (`sepEndBy` ruleComma) $
                ruleVarName <|> do { undefLiteral; return "" }
             let mkVar v = if null v then Val undef else Var v
             return (combine (map (Sym scope) names), Syn "," (map mkVar names))
        ]
    _traits <- many ruleTrait
    -- pos <- getPosition
    (sym, expMaybe) <- option ("=", Nothing) $ do
        sym <- choice $ map (try . string) $ words " = .= := ::= "
        when (sym == "=") $ do
           lookAhead (satisfy (/= '='))
           return ()
        whiteSpace
        -- XXX handle 'my $a = my $b = foo'.
        -- matching ruleVarDeclaration here results in nested pads,
        -- i.e. Pad SMy _ (Syn "=",[Var _,Stmts Noop (Pad SMy _ _)]),
        -- which is NOT what we want. we re-arrange this below in unnestPad.
        exp <- choice
            [ ruleVarDeclaration
            , ruleExpression
            ]
        -- Slightly hacky. Here "my Foo $foo .= new(...)" is rewritten into
        -- "my Foo $foo = Foo.new(...)".
        -- And note that IIRC not the type object should be the invocant, but
        -- an undef which knows to dispatch .new to the real class.
        let exp' | Ann (Pos _) (App sub Nothing args) <- exp, sym == ".=" && typename /= "" -- XXX: App _ maybe?
                 = return $ App sub (Just . Var $ ':':typename) args
                 | sym == ".=" && typename /= ""
                 = fail "The .= operator expects a method application as RHS."
                 | sym == ".="
                 = fail "The .= operator needs a type specification."
                 | otherwise
                 = return exp
        let sym' | sym == ".=" = "="
                 | otherwise   = sym
        exp'' <- exp'
        return (sym', Just exp'')
    lexDiff <- case sym of
        "::="   -> do
            env  <- ask
            env' <- unsafeEvalEnv $ decl (Syn sym [lhs, fromJust expMaybe])
            return $ envLexical env' `diffPads` envLexical env
        _       -> unsafeEvalLexDiff (decl emptyExp)
    let rhs | sym == "::=" = emptyExp
            | otherwise = maybe emptyExp (\exp -> Syn sym [lhs, exp]) expMaybe
    -- state $x = 42 is really syntax sugar for state $x; FIRST { $x = 42 }
    -- XXX always wrap the Pad in a Stmts so that expRule has something to unwrap
    case scope of
        SState -> do
            implicit_first_block <- vcode2firstBlock $ VCode mkSub { subBody = rhs }
            return $ Stmts Noop $ unnestPad $ Pad scope lexDiff implicit_first_block
        _      -> return $ Stmts Noop $ unnestPad $ Pad scope lexDiff rhs
    where
    -- XXX here's the other half of the squinting fix.
    -- see above. together, we turn 'my $a = my $b = foo' into
    -- 'my $a; my $b; $a = $b = foo'. should we handle more scopes than 'SMy'?
    -- more syntax than '(Syn "=" _)'?
    --unnestPad (Pad SMy lex (Syn "=" [v,Pad SMy lex' (Syn "=" [v',x])])) = Pad SMy lex (Stmts Noop (Pad SMy lex' (Syn "=" [v,(Syn "=" [v',unnestPad x])])))
    unnestPad (Pad scope1 lex (Syn "=" [v,Stmts Noop (Pad scope2 lex' (Syn "=" [v',x]))])) = Pad scope1 lex (Stmts Noop (Pad scope2 lex' (Syn "=" [v,(Syn "=" [v',unnestPad x])])))
    unnestPad x@_ = x
-}

{-|
Match a @no@ declaration, i.e. the opposite of @use@ (see
'ruleUseDeclaration').

Works by matching \'@no@\', then trying 'ruleNoVersion' and
@'ruleUsePackage' False@.
-}
ruleNoDeclaration :: RuleParser Exp
ruleNoDeclaration = rule "no declaration" $ do
    symbol "no"
    choice [ ruleNoVersion >> return emptyExp
              , ruleUsePackage False
              ]
    return emptyExp

{-|
Match a @use@ declaration.

Works by matching \'@use@\', then trying 'ruleUseVersion' and
@'ruleUsePackage' True@.
-}
ruleUseDeclaration :: RuleParser Exp
ruleUseDeclaration = rule "use declaration" $ do
    symbol "use"
    choice [ try ruleUseVersion >> return emptyExp
           , ruleUsePackage True
           ]

rulePerlVersion :: RuleParser String
rulePerlVersion = rule "perl version" $ do
    optional (string "v" <|> string "Perl-")
    version <- many1 (choice [ digit, char '.' ])
    optional ruleAuthorPart
    return version

{-|
Match a Perl version number (as part of a @use@ declaration), and abort if
the version needed is higher than our version.
-}
ruleUseVersion :: RuleParser ()
ruleUseVersion = rule "use version" $ do
    version <- rulePerlVersion
    when (version > versnum) $ do
        pos <- getPosition
        error $ "Perl v" ++ version ++ " required--this is only v" ++ versnum ++ ", stopped at " ++ (show pos)

{-|
Match a Perl version number (as part of a @no@ declaration), and abort if
the version needed is lower than our version.
-}
ruleNoVersion :: RuleParser ()
ruleNoVersion = rule "no version" $ do
    version <- rulePerlVersion
    when (version <= versnum) $ do
        pos <- getPosition
        error $ "Perls since v" ++ version ++ " too modern--this is v" 
                  ++ versnum ++ ", stopped at " ++ show pos

{-|
Match the contents of a @use@ or @no@ declaration.

Works by reading the (optional) \'lang\' prefix, then dispatching to either
'ruleUseJSANModule' or 'ruleUsePerlPackage' as appropriate.

It is assumed that the @use@ or @no@ has already been consumed by
'ruleUseDeclaration' or 'ruleNoDeclaration'.
-}
ruleUsePackage :: Bool -- ^ @True@ for @use@; @False@ for @no@
               -> RuleParser Exp
ruleUsePackage use = rule "use package" $ do
    lang <- ruleUsePackageLang
    case lang of
        "jsan" -> if use
                      then ruleUseJSANModule
                      else fail "can't 'no' a JSAN module"
        "jsperl5" -> if use
                      then ruleUseJSPerl5Module
                      else fail "can't 'no' a Perl5 module"
        _      -> ruleUsePerlPackage use lang
    where
    ruleUsePackageLang = option "pugs" $ try $ do
        lang <- identifier
        char ':'
        notFollowedBy (char ':')
        return lang
        
{-|
Match the package-name and import-list part of a (non-JSAN) @use@ or @no@
declaration, then perform the actual import.

The parser itself does not yield a useful value; instead it modifies the
'Pugs.AST.Internals.Env' part of the parser's state to reflect the results
of the declaration.  (If the package is a Perl 5 package, the modification
will be different.)

It is assumed that 'ruleUsePackage' has already consumed the \'lang\' prefix,
which is passed in as the second argument.
-}
ruleUsePerlPackage :: Bool   -- ^ @True@ for @use@; @False@ for @no@
                   -> String -- ^ \'lang\' prefix (e.g. \"@perl5@\" or
                             --     \"@pugs@\")
                   -> RuleParser Exp
ruleUsePerlPackage use lang = rule "use perl package" $ do
    -- author and version get thrown away for now
    (names, _, _) <- rulePackageFullName
    let pkg = concat (intersperse "::" names)
    when use $ do   -- for &no, don't load code
        env  <- ask
        env' <- unsafeEvalEnv $ if lang == "perl5"
            then Stmts (Sym SGlobal (':':'*':pkg)
                            (Syn ":=" [ Var (':':'*':pkg)
                               , App (Var "&require_perl5") Nothing [Val $ VStr pkg] ]))
                       (newType pkg)
            else Stmts (App (Var "&use") Nothing [Val $ VStr pkg])
                       (App (Var "&unshift")
                            (Just (Var ("@" ++ envPackage env ++ "::END")))
                            [Var ("@" ++ pkg ++ "::END")])
        modify $ \state -> state
            { ruleEnv = env
                { envClasses = envClasses env' `addNode` mkType pkg
                , envGlobal  = envGlobal env'
                }
            , ruleDynParsers = MkDynParsersEmpty
            }
    try (do { verbatimParens whiteSpace ; return emptyExp}) <|> do
        imp <- option emptyExp ruleExpression
        let sub = Var $ ('&':pkg) ++ if use then "::import" else "::unimport"
        unsafeEvalExp $ Syn "if"
            [ App (Var "&name") (Just sub) [] -- XXX Hack
            , App sub (Just $ Val $ VStr $ pkg) [imp]
            , emptyExp
            ]

        Val (VList exportList) <- unsafeEvalExp $ case lang of
            -- map { ~$_, [::Pkg.can($_)] }, @importlist
            "perl5" -> App (Var "&map") Nothing [Syn "sub"
                [ Val . VCode $ mkSub
                    { subBody   = Syn ","
                        [ App (Var "&prefix:<~>") (Just $ Var "$_") []
                        , Syn "\\[]" [ App (Var "&can") (Just $ Var (':':'*':pkg)) [Var "$_"] ]
                        ]
                    , subParams = [defaultScalarParam]
                    }
                ], imp ]
            -- %Pkg::EXPORTS.kv
            _ -> App (Var "&kv") (Just $ Var ('%':pkg ++ "::EXPORTS")) []

        let hardcodedScopeFixme = SMy
            doExportList [] = []
            doExportList [x] = error $ "doExportList [x]: " ++ show x
            doExportList (VStr name:ex:xs) = 
                (exportSym hardcodedScopeFixme name ex : doExportList xs)
            doExportList x = error $ "doExportList x: " ++ show x
        sequence_ $ doExportList exportList
        clearDynParsers
        return emptyExp

{-|
Match a JSAN module name, returning an appropriate
sub call 'Pugs.AST.Exp' that will load the module using subs defined in
@PIL2JS::Internals@.

Used by 'ruleUsePackage' after matching \'@use jsan:@\'.

More info about JSAN can be found at <http://www.openjsan.org/>.
-}
ruleUseJSANModule :: RuleParser Exp
ruleUseJSANModule = do
    (names, _, _) <- choice
        [ rulePackageFullName
        -- leave this in as a hack, until we decide
        -- whether to allow it or not
        , do
            name <- ruleDelimitedIdentifier "."
            return (name, Nothing, Nothing)
        ]
    
    let name = Val . VStr . concat $ intersperse "." names
    choice
        [ try $ do
            verbatimParens whiteSpace
            return $ App (Var "&PIL2JS::Internals::use_jsan_module_noimp") Nothing [name]
        , do
            exp <- option emptyExp ruleExpression
            let exp' | exp == emptyExp = []
                     | otherwise       = [exp]
            return $ App (Var "&PIL2JS::Internals::use_jsan_module_imp") Nothing $ name:exp'
        ] 

{-|
Match a perl5 module for js backend, returning an appropriate
sub call 'Pugs.AST.Exp' that will load the module using subs defined in
@PIL2JS::Internals@.

-}
ruleUseJSPerl5Module :: RuleParser Exp
ruleUseJSPerl5Module = do
    (names, _, _) <- choice
        [ rulePackageFullName
        -- leave this in as a hack, until we decide
        -- whether to allow it or not
        , do
            name <- ruleDelimitedIdentifier "::"
            return (name, Nothing, Nothing)
        ]
    
    let name = Val . VStr . concat $ intersperse "::" names
    choice
        [ try $ do
            verbatimParens whiteSpace
            return $ App (Var "&PIL2JS::Internals::use_perl5_module_noimp") Nothing [name]
        , do
            exp <- option emptyExp ruleExpression
            let exp' | exp == emptyExp = []
                     | otherwise       = [exp]
            return $ App (Var "&PIL2JS::Internals::use_perl5_module_imp") Nothing $ name:exp'
        ] 
        
{-|
Match a full package name, consisting of:

* A short name, optionally delimited by double colons (@::@)

* An optional version specification

* An optional author specification (e.g. @cpan:JRANDOM@)
-}
rulePackageFullName :: RuleParser ( [String]
                                  , (Maybe String)
                                  , (Maybe String)
                                  )
rulePackageFullName = do
    name    <- ruleDelimitedIdentifier "::"
    version <- option Nothing $ fmap Just ruleVersionPart
    author  <- option Nothing $ fmap Just ruleAuthorPart
    whiteSpace
    return (name, version, author)

-- | The version part of a fully-qualified package name.
ruleVersionPart :: RuleParser String
ruleVersionPart = do -- version - XXX
    char '-'
    str <- many (choice [ digit, char '.', char '(', char ')' ])
    return ('-':str)

-- | The author part of a fully-qualified package name.
ruleAuthorPart :: RuleParser String
ruleAuthorPart = do -- author - XXX
    char '-'
    -- this will break if you specify an author AND an imports list
    str <- many1 (satisfy (/= ';'))
    return ('-':str)
{- end of ruleUseDeclaration -}

ruleInlineDeclaration :: RuleParser Exp
ruleInlineDeclaration = rule "inline declaration" $ do
    symbol "inline"
    args <- ruleExpression
    case args of
        App (Var "&infix:=>") Nothing exp -> do
            return $ Syn "inline" exp
        _ -> fail "not yet parsed"

{-|
Match a @require@ declaration, returning a sub call 'Pugs.AST.Exp' that will
load the package at runtime.

(This should probably be merged with 'ruleUseDeclaration' & friends, if
anybody has some tuits.)
-}
ruleRequireDeclaration :: RuleParser Exp
ruleRequireDeclaration = rule "require declaration" $ do
    symbol "require"
    (names, _, _) <- rulePackageFullName
    return $ App (Var "&require") Nothing [Val . VStr $ concat (intersperse "::" names)]

ruleDoBlock :: RuleParser Exp
ruleDoBlock = rule "do block" $ do
    symbol "do"
    enterBracketLevel StatementBracket $ choice
        [ ruleVerbatimBlock
        , ruleBlockDeclaration
        , ruleDeclaration
        , ruleConstruct
        , ruleStatement
        ]

ruleClosureTrait :: Bool -> RuleParser Exp
ruleClosureTrait rhs = rule "closure trait" $ do
    let names | rhs       = " BEGIN CHECK INIT FIRST "
              | otherwise = " BEGIN CHECK INIT FIRST END "
    name    <- choice $ map symbol $ words names
    block   <- ruleBlock
    let (fun, names) = extractPlaceholderVars block []
    -- Check for placeholder vs formal parameters
    when (not $ null names) $
        fail "Closure traits take no formal parameters"
    env <- ask
    let code = VCode mkSub { subName = name, subBody = fun, subEnv = Just env } 
    case name of
        "END"   -> do
            -- We unshift END blocks to @*END at compile-time.
            -- They're then run at the end of runtime or at the end of the
            -- whole program.
            let pkg = envPackage env
                end | pkg == "main" = "@*END"
                    | otherwise     = '@':pkg++"::END"
            unsafeEvalExp $ 
                App (Var "&unshift")
                    (Just (Var end))
                    [Val code]
            return emptyExp
        "BEGIN" -> do
            -- We've to exit if the user has written code like BEGIN { exit }.
            val <- possiblyExit =<< unsafeEvalExp (checkForIOLeak fun)
            -- And install any pragmas they've requested.
            env <- ask
            let idat = unsafePerformSTM . readTVar $ envInitDat env
            install $ initPragmas idat
            clearDynParsers
            return val
        "CHECK" -> vcode2checkBlock code
        "INIT"  -> vcode2initBlock code
        "FIRST" -> vcode2firstBlock code
        _       -> fail ""
    where
        install [] = return $ ()
        install prag = do
            env' <- ask
            let env'' = envCaller env'  -- not sure about this.
            case env'' of
                Just target -> do
                    putRuleEnv target { envPragmas = prag ++ envPragmas target }
                _ -> fail "no caller env to install pragma in"

{-| Match a @q:code { ... }@ quotation -}
ruleCodeQuotation :: RuleParser Exp
ruleCodeQuotation = rule "code quotation" $ do
    -- XXX - This is entirely kluge
    symbol "q:code" >> optional (symbol "(:COMPILING)")
    body <- verbatimBraces ruleBlockBody
    return $ Syn "q:code" [ body ]
    
-- | If we've executed code like @BEGIN { exit }@, we've to run all @\@*END@
--   blocks and then exit. Returns the input expression if there's no need to
--   exit.
{-# NOINLINE possiblyExit #-}
possiblyExit :: Exp -> RuleParser Exp
possiblyExit (Val (VControl (ControlExit exit))) = do
    -- Run all @*END blocks...
    unsafeEvalExp $ Syn "for"
        [ Var "@main::END"
        , Syn "sub"
            [ Val . VCode $ mkSub
                { subBody   = App (Var "$_") Nothing []
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
    --   $?FIRST_RUN++ ?? $?FIRST_RESULT !! $?FIRST_RESULT = { 42 }();
    -- }
    -- These are the two state variables we need.
    -- This will soon add our two state vars to our pad
    lexDiff <- unsafeEvalLexDiff $
        (Sym SState "$?FIRST_RESULT") . (Sym SState "$?FIRST_RUN") $ emptyExp
    -- And that's the transformation part.
    return $ Syn "block"        -- The outer block
        [ Pad SState lexDiff $  -- state ($?FIRST_RESULT, $?FIRST_RUN);
            Syn "if"
                [ App (Var "&postfix:++") Nothing [Var "$?FIRST_RUN"]
                , Var "$?FIRST_RESULT"
                , Syn "=" [Var "$?FIRST_RESULT", App (Val code) Nothing []]
                ]   --  { $?FIRST_RUN++; $?FIRST_RESULT = { 42 }() };
        ]

vcode2initBlock :: Val -> RuleParser Exp
vcode2initBlock code = vcode2initOrCheckBlock "@*INIT" True code

vcode2checkBlock :: Val -> RuleParser Exp
vcode2checkBlock code = vcode2initOrCheckBlock "@*CHECK" False code

vcode2initOrCheckBlock :: String -> Bool -> Val -> RuleParser Exp
vcode2initOrCheckBlock magicalVar allowIOLeak code = do
    -- Similar as with FIRST {...}, we transform our input:
    -- my $x = INIT { 42 }   is transformed into
    -- BEGIN { push @*INIT, { FIRST { 42 } } }; my $x = @*INIT[(index)]();
    -- Or, with CHECK:
    -- my $x = CHECK { 42 }  is transformed into
    -- BEGIN { push @*CHECK, { FIRST { 42 } } }; my $x = @*CHECK[(index)]();
    -- This is the inner FIRST {...} block we generate.
    body <- vcode2firstBlock code
    let possiblyCheck | allowIOLeak = id
                      | otherwise   = checkForIOLeak
    rv <- unsafeEvalExp $
        -- BEGIN { push @*INIT, { FIRST {...} } }
        App (Var "&push") (Just $ Var magicalVar)
            [ Syn "sub" [ Val $ VCode mkSub { subBody = possiblyCheck body }]]
    -- rv is the return value of the push. Now we extract the actual num out of it:
    let (Val (VInt elems)) = rv
    -- Finally, we can return the transformed expression.
    -- elems is the new number of elems in @*INIT (as push returns the new
    -- number of elems), but we're interested in the index, so we -1 it.
    return $ App (Syn "[]" [Var magicalVar, Val . VInt $ elems - 1]) Nothing []

-- Constructs ------------------------------------------------

ruleConstruct :: RuleParser Exp
ruleConstruct = rule "construct" $ choice
    [ ruleForConstruct
    , ruleLoopConstruct
    , ruleCondConstruct
    , ruleWhileUntilConstruct
--  , ruleStandaloneBlock
    , ruleGivenConstruct
    , ruleWhenConstruct
    , ruleDefaultConstruct
    , yadaLiteral
    ]

ruleForConstruct :: RuleParser Exp
ruleForConstruct = rule "for construct" $ do
    symbol "for"
    list  <- maybeParens ruleExpression
    optional ruleComma
    block <- ruleBlockLiteral <|> parseExpWithItemOps
    return $ Syn "for" [list, block]

ruleLoopConstruct :: RuleParser Exp
ruleLoopConstruct = rule "loop construct" $ do
    symbol "loop"
    choice [ ruleSemiLoopConstruct, rulePostLoopConstruct ]

ruleSemiLoopConstruct :: RuleParser Exp
ruleSemiLoopConstruct = rule "for-like loop construct" $ do
    conds <- parens $ do
        a <- option emptyExp ruleExpression
        symbol ";"
        b <- option emptyExp ruleExpression
        symbol ";"
        c <- option emptyExp ruleExpression
        return [a,b,c]
    block <- ruleBlock
    return $ Syn "loop" (conds ++ [block])

rulePostLoopConstruct :: RuleParser Exp
rulePostLoopConstruct = rule "postfix loop construct" $ do
    block <- ruleBlock
    option (Syn "loop" [block]) $ do
        name <- choice [ symbol "while", symbol "until" ]
        cond <- ruleExpression
        return $ Syn ("post" ++ name) [cond, block]

ruleCondConstruct :: RuleParser Exp
ruleCondConstruct = rule "conditional construct" $ do
    csym <- choice [ symbol "if", symbol "unless" ]
    ruleCondBody $ csym

ruleCondBody :: String -> RuleParser Exp
ruleCondBody csym = rule "conditional expression" $ do
    cond     <- ruleCondPart
    enterBracketLevel ParensBracket $ do
        body     <- ruleBlock
        bodyElse <- option emptyExp ruleElseConstruct
        return $ Syn csym [cond, body, bodyElse]

ruleCondPart :: RuleParser Exp
ruleCondPart = enterBracketLevel ConditionalBracket ruleExpression

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
    cond <- ruleCondPart
    body <- ruleBlock
    return $ Syn sym [ cond, body ]

ruleGivenConstruct :: RuleParser Exp
ruleGivenConstruct = rule "given construct" $ do
    sym <- symbol "given"
    topic <- ruleCondPart
    body <- ruleBlock
    return $ Syn sym [ topic, body ]

ruleWhenConstruct :: RuleParser Exp
ruleWhenConstruct = rule "when construct" $ do
    sym <- symbol "when"
    match <- ruleCondPart
    body <- ruleBlock
    return $ Syn sym [ match, body ]

-- XXX: make this translate into when true, when smartmatch
-- against true works
ruleDefaultConstruct :: RuleParser Exp
ruleDefaultConstruct = rule "default construct" $ do
    sym <- symbol "default"
    body <- ruleBlock
    return $ Syn sym [ body ]

-- Expressions ------------------------------------------------

ruleExpression :: RuleParser Exp
ruleExpression = (<?> "expression") $ parseExpWithOps

{-|
Match a statement's /conditional/ statement-modifier,
e.g. '@say \"hello\" if \$cheerful@' or '@die unless +\@arguments@'.

Returns a function that will take the statement proper, and enclose it in an
appropriate 'Pugs.AST.Internals.Syn' (either @\"if\"@ or @\"unless\"@).
-}
rulePostConditional :: RuleParser (Exp -> RuleParser Exp)
rulePostConditional = rule "postfix conditional" $ do
    cond <- choice $ map symbol ["if", "unless"]
    exp <- ruleExpression
    return $ \body -> return $ Syn cond [exp, body, emptyExp]

{-|
Match a statement's /looping/ statement-modifier,
e.g. '@procrastinate while $bored@' or '@eat until $full@'.

Returns a function that will take the statement proper, and enclose it in an
appropriate 'Pugs.AST.Internals.Syn' (either @\"while\"@ or @\"until\"@).
-}
rulePostLoop :: RuleParser (Exp -> RuleParser Exp)
rulePostLoop = rule "postfix loop" $ do
    cond <- choice $ map symbol ["while", "until"]
    exp <- ruleExpression
    return $ \body -> return $ Syn cond [exp, body]

{-|
Match a statement's /iterating/ statement-modifier,
e.g. '@say for 1..10@'.

Returns a function that will take the statement proper, and enclose it in
@'Pugs.AST.Internals.Syn' \"for\"@.
-}
rulePostIterate :: RuleParser (Exp -> RuleParser Exp)
rulePostIterate = rule "postfix iteration" $ do
    cond <- choice $ map symbol ["for"]
    exp <- ruleExpression
    return $ \body -> do
        block <- retBlock SubBlock Nothing False body
        return $ Syn cond [exp, block]

ruleBareOrPointyBlockLiteral :: RuleParser Exp
ruleBareOrPointyBlockLiteral = rule "bare or pointy block construct" $
    ruleBlockVariants [ ruleBlockFormalPointy ]

ruleBlockLiteral :: RuleParser Exp
ruleBlockLiteral = rule "block construct" $
    ruleBlockVariants [ ruleBlockFormalPointy, ruleBlockFormalStandard ]

ruleBlockVariants :: [RuleParser (SubType, Maybe [Param], Bool)] -> RuleParser Exp
ruleBlockVariants variants = do
    (typ, formal, lvalue) <- option (SubBlock, Nothing, False)
        $ choice variants
    body <- ruleBlock
    retBlock typ formal lvalue body

retBlock :: SubType -> Maybe [Param] -> Bool -> Exp -> RuleParser Exp
retBlock SubBlock Nothing _ exp | Just hashExp <- extractHash (unwrap exp) = return $ Syn "\\{}" [hashExp]
retBlock typ formal lvalue body = retVerbatimBlock typ formal lvalue body

retVerbatimBlock :: SubType -> Maybe [Param] -> Bool -> Exp -> RuleParser Exp
retVerbatimBlock styp formal lvalue body = expRule $ do
    let (fun, names, params) = doExtract styp formal body
    -- Check for placeholder vs formal parameters
    when (isJust formal && (not.null) names) $
        fail "Cannot mix placeholder variables with formal parameters"
    env <- ask
    let sub = MkCode
            { isMulti       = False
            , subName       = "<anon>"
            , subEnv        = Just env
            , subType       = styp
            , subAssoc      = "pre"
            , subReturns    = anyType
            , subLValue     = lvalue
            , subParams     = paramsFor styp formal params
            , subBindings   = []
            , subSlurpLimit = []
            , subBody       = fun
            , subCont       = Nothing
            }
    return (Syn "sub" [Val $ VCode sub])

ruleBlockFormalStandard :: RuleParser (SubType, Maybe [Param], Bool)
ruleBlockFormalStandard = rule "standard block parameters" $ do
    styp <- choice
        [ do { symbol "sub";   return SubRoutine }
        , do { symbol "coro";  return SubCoroutine }
        , do { symbol "macro"; return SubMacro }
        ]
    params <- option Nothing $ ruleSubParameters ParensMandatory
    traits <- many $ ruleTrait
    return $ (styp, params, "rw" `elem` traits)

ruleBlockFormalPointy :: RuleParser (SubType, Maybe [Param], Bool)
ruleBlockFormalPointy = rule "pointy block parameters" $ do
    symbol "->"
    params <- ruleSubParameters ParensOptional
    traits <- many $ ruleTrait
    return $ (SubPointy, params, "rw" `elem` traits)




















-- was: parseOp
parseExpWithOps :: RuleParser Exp
parseExpWithOps = parseExpWithCachedParser dynParseOp

{-
was: parseLitOp
[09:23] <autrijus> lit = tight+loose
[09:24] <autrijus> i.e. all operators minus the list-associative , Y
[09:24] <scook0> any reason it's called 'lit'?
[09:26] <scook0> I didn't realise parseLitOp/parseTightOp were parsing terms, btw
[09:29] <autrijus> please fix them
[09:29] <autrijus> ruleLitTerms
[09:29] <autrijus> "lit" as in "literal"
[09:29] <autrijus> as in non-list
[09:29] <autrijus> but this is too vague
[09:29] <autrijus> maybe "Item".
-}
parseExpWithItemOps :: RuleParser Exp
parseExpWithItemOps = parseExpWithCachedParser dynParseLitOp

ruleVarDecl :: RuleParser Exp
ruleVarDecl = rule "variable declaration" $ do
    scope           <- ruleScope
    (cxtNames, exp) <- oneDecl <|> manyDecl
    let makeBinding (name, cxt)
            | ('$':_) <- name, typ /= anyType   = mkSym . bindSym
            | otherwise                         = mkSym
            where
            mkSym   = Sym scope name
            bindSym = Stmts (Syn "=" [Var name, Val (VType typ)])
            typ     = typeOfCxt cxt
    lexDiff <- unsafeEvalLexDiff $ combine (map makeBinding cxtNames) emptyExp
    -- Now hoist the lexDiff to the current block
    addBlockPad scope lexDiff
    return exp
    where
    deSigil (sig:'!':rest@(_:_)) = (sig:rest)
    deSigil (sig:'.':rest) = (sig:rest)
    deSigil x              = x
    oneDecl = do
        param <- ruleFormalParam FormalsSimple
        let name = deSigil (paramName param)
        return ([(name, paramContext param)], Var name)
    manyDecl = do
        params <- verbatimParens . enterBracketLevel ParensBracket $
            ruleFormalParam FormalsComplex `sepBy1` ruleComma
        let names = map (deSigil . paramName) params
            types = map paramContext params
        return (names `zip` types, Syn "," $ map Var names)

parseTerm :: RuleParser Exp
parseTerm = rule "term" $ do
    term <- choice
        [ ruleDereference
        , ruleVarDecl
        , ruleVar
        , ruleApply True    -- Folded metaoperators
        , ruleLit
--      , ruleBarewordMethod
        , ruleClosureTrait True
        , ruleCodeQuotation
        , ruleTypeVar
--      , ruleTypeLiteral
        , ruleApply False   -- Normal application
        , verbatimParens ruleBracketedExpression
        ]
    cls  <- getPrevCharClass
    case cls of
        SpaceClass -> return term
        _ -> do
            -- rulePostTerm returns an (Exp -> Exp) that we apply to the original term
            fs <- many rulePostTerm
            return (combine (reverse fs) term)

{-
ruleBarewordMethod :: RuleParser Exp
ruleBarewordMethod = try $ do
    name <- identifier
    lookAhead (char '.' >> ruleSubName)
    return $ Var (':':name)
-}

ruleTypeVar :: RuleParser Exp
ruleTypeVar = rule "type" $ do
    -- We've to allow symbolic references with type vars, too.
    nameExps <- many1 $ do
        string "::"
        (parens ruleExpression) <|> (fmap (Val . VStr . concat) $ sequence [ruleTwigil, many1 wordAny])
    -- Optimization: We don't have to construct a symbolic deref syn (":::()"),
    -- but can use a simple Var, if nameExps consists of only one expression
    -- and this expression is a plain string, i.e. it is not a
    -- (...)-expression.
    return $ case nameExps of
        [Val (VStr name)] -> Var $ ":" ++ name
        _                 -> Syn (":::()") nameExps

rulePostTerm :: RuleParser (Exp -> Exp)
rulePostTerm = verbatimRule "term postfix" $ do
    hasDot <- option Nothing $ choice [dotChar, try bangChar]
    let maybeInvocation = case hasDot of
            Just '.' -> (ruleInvocation:)
            Just '!' -> (bangKludged ruleInvocation:)
            _        -> id
    choice $ maybeInvocation
        [ ruleArraySubscript
        , ruleHashSubscript
        , ruleCodeSubscript
        ]
    where
    dotChar = do { ruleDot; return $ Just '.' }
    bangChar = do { char '!'; lookAhead ruleSubName; return $ Just '!' }
    -- XXX - this should happen only in a "trusts" class!
    bangKludged p = do
        f <- p
        return $ \x -> case f x of
            App (Var ('&':name)) (Just inv) [] ->
                Syn "{}" [inv, Val (VStr name)]
            e -> e

ruleInvocation :: RuleParser (Exp -> Exp)
ruleInvocation = ruleInvocationCommon False

-- used only by 'qInterpolatorPostTerm'?
ruleInvocationParens :: RuleParser (Exp -> Exp)
ruleInvocationParens = ruleInvocationCommon True
        
ruleInvocationCommon :: Bool -> RuleParser (Exp -> Exp)
ruleInvocationCommon mustHaveParens = do
    name            <- ruleSubName
    (invs, args)    <- if mustHaveParens
        then parseHasParenParamList
        else do  --  $obj.foo: arg1, arg2    # listop method call
                 -- we require whitespace after the colon (but not before)
                 -- so that @list.map:{...} doesn't get interpreted the
                 -- wrong way.
            listcolon <- option False $ try $ do { char ':'; mandatoryWhiteSpace; return True }
            if listcolon
                then parseNoParenParamList
                else option (Nothing,[]) $ parseParenParamList
    when (isJust invs) $ fail "Only one invocant allowed"
    return $ \x -> App (Var name) (Just x) args

ruleArraySubscript :: RuleParser (Exp -> Exp)
ruleArraySubscript = tryVerbatimRule "array subscript" $ do
    between (symbol "[") (char ']') $ option id $ do
        exp <- ruleExpression; return $ \x -> Syn "[]" [x, exp]

ruleHashSubscript :: RuleParser (Exp -> Exp)
ruleHashSubscript = tryVerbatimRule "hash subscript" $ do
    choice [ ruleHashSubscriptBraces, ruleHashSubscriptQW ]

ruleHashSubscriptBraces :: RuleParser (Exp -> Exp)
ruleHashSubscriptBraces = do
    verbatimBraces $ option id $ do
        exp <- ruleExpression; return $ \x -> Syn "{}" [x, exp]

ruleHashSubscriptQW :: RuleParser (Exp -> Exp)
ruleHashSubscriptQW = do
    exp <- angleBracketLiteral
    return $ \x -> Syn "{}" [x, exp]

ruleCodeSubscript :: RuleParser (Exp -> Exp)
ruleCodeSubscript = tryVerbatimRule "code subscript" $ do
    (invs, args) <- parseHasParenParamList
    return $ \x -> App x invs args

{-|
Match a sub application, returning the appropriate 'App' expression.

Note that this only handles regular sub application (@foo(\$bar)@) and
implicit-invocant calls (@.foo@); regular method invocation (@\$obj.foo@) is
handled by 'ruleInvocation' as a post-term ('rulePostTerm').

The boolean argument is @True@ if we're trying to parse a reduce-metaop
application (e.g. @[+] 1, 2, 3@), and @False@ otherwise.

/NOTE: This is where the dot-slash and dot-colon statement
forms get parsed, so if they need to be changed\/killed, this is the place./
-}
ruleApply :: Bool -- ^ @True@ if we are parsing for the reduce-metaop
          -> RuleParser Exp
ruleApply isFolded = tryVerbatimRule "apply" $ 
    if isFolded
        then ruleApplySub isFolded
        else ruleApplyImplicitMethod <|> ruleApplySub isFolded

ruleApplyImplicitMethod :: RuleParser Exp
ruleApplyImplicitMethod = do
    {- (colonify :: String -> String) is a function that will take a name
       like '&foo', and MIGHT convert it to '&:foo'.  This only happens if
       you're using the '!foo' implicit-invocant syntax, otherwise 'colon' will
       just be 'id'.
    
       (inv :: Maybe Exp) might hold a 'Var' expression indicating the implicit
       invocant.  Of course, if you're not using implicit-invocant syntax, this
       will be 'Nothing'. -}
    implicitInv <- do
        char '.' -- implicit-invocant forms all begin with '.'
        option (Var "$_") $
            -- XXX - This ./method form is going to be removed
            do { char '/'; return (Var "$?SELF") }
    insertIntoPosition '.' 
    fs <- many rulePostTerm
    return (combine (reverse fs) implicitInv)

ruleSubNameWithoutPostfixModifier :: RuleParser String
ruleSubNameWithoutPostfixModifier = try $ do
    name <- ruleSubName
    case name of
        "&if"       -> fail "postfix op"
        "&unless"   -> fail "postfix op"
        "&while"    -> fail "postfix op"
        "&until"    -> fail "postfix op"
        "&for"      -> fail "postfix op"
        _           -> return name

ruleApplySub :: Bool -> RuleParser Exp
ruleApplySub isFolded = do
    name    <- if isFolded
        then ruleFoldOp
        else ruleSubNameWithoutPostfixModifier

    (paramListInv, args) <- choice $
        [ (ruleDot `tryLookAhead` char '(') >> parseHasParenParamList
        , parseParenParamList
        , mandatoryWhiteSpace >> parseNoParenParamList
        , return (Nothing, [])
        ]
    possiblyApplyMacro $ App (Var name) paramListInv args
{-
    -- True for `foo. .($bar)`-style applications
    let takeArguments = do
            (paramListInv, args) <- choice $
                [ (ruleDot `tryLookAhead` char '(') >> parseHasParenParamList
                , parseParenParamList
                , mandatoryWhiteSpace >> parseNoParenParamList
                ] ++ (if isFolded then [return (Nothing, [])] else [])
            possiblyApplyMacro $ App (Var name) paramListInv args
    takeArguments
        <|> possiblyTypeLiteral name
        <|> possiblyApplyMacro (App (Var name) Nothing [])
-}

ruleFoldOp :: RuleParser String
ruleFoldOp = verbatimRule "reduce metaoperator" $ try $ do
    char '['
    keep <- option "" $ string "\\"
    [_, _, _, _, _, infixOps] <- currentTightFunctions
    -- name <- choice $ ops (try . string) (addHyperInfix $ infixOps ++ defaultInfixOps)
    name <- verbatimRule "infix operator" $ do
        choice $ ops (try . string) (addHyperInfix $ infixOps ++ defaultInfixOps)
    char ']'
    possiblyHyper <- option "" ((char '\171' >> return "<<") <|> (string "<<"))
    return $ "&prefix:[" ++ keep ++ name ++ "]" ++ possiblyHyper
    where
    defaultInfixOps = words $ concat
        [ " ** * / % x xx +& +< +> ~& ~< ~> "
        , " + - ~ +| +^ ~| ~^ ?| , Y \xA5 "
        , " & ^ | "
        , " => = "
        , " != == < <= > >= ~~ !~ "
        , " eq ne lt le gt ge =:= === "
        , " && "
        , " || ^^ // "
        , " and or xor err "
        , " .[] .{} "
        ]

-- used only by 'ruleCodeSubscript'!
parseParenParamList :: RuleParser (Maybe Exp, [Exp])
parseParenParamList = parseParenParamListCommon True

parseParenParamListCommon :: Bool -> RuleParser (Maybe Exp, [Exp])
parseParenParamListCommon mustHaveParens = do
    leading     <- option [] $ try $ many namedAdverb
    params      <- option Nothing . fmap Just $ parseHasParenParamList
    trailing    <- option [] $ try $ many pairOrBlockAdverb
    when (mustHaveParens && isNothing params && null trailing && null leading) $ fail ""
    let (inv, args) = fromMaybe (Nothing, []) params
    return (inv, leading ++ args ++ trailing)
    
-- initial helpers for demagicalized pairs
-- to DISABLE special parsing for pairs in argument-lists, replace this
-- definition with `id`
named :: RuleParser Exp -> RuleParser Exp
named parser = try $ do
    result <- parser
    case unwrap result of
        (App (Var "&infix:=>") Nothing [key, val]) -> return (Syn "named" [key, val])
        _                                          -> fail "internal error--was expecting a pair"

namedArg :: RuleParser Exp
namedArg = named pairLiteral

namedArgOr :: RuleParser Exp -> RuleParser Exp
namedArgOr other = try namedArg <|> other

namedAdverb :: RuleParser Exp
namedAdverb = named pairAdverb

pairOrBlockAdverb :: RuleParser Exp
pairOrBlockAdverb = choice [ namedAdverb, blockAdverb ]

blockAdverb :: RuleParser Exp
blockAdverb = do
    char ':' `tryLookAhead` char '{'
    ruleBlockLiteral

parseHasParenParamList :: RuleParser (Maybe Exp, [Exp])
parseHasParenParamList = verbatimParens . enterBracketLevel ParensBracket $ do
    -- formal :: [[Exp]]
    -- outer level of listness provided by `sepEndBy`
    -- the inner (`fix`ed) part returns [Exp]
    formal <- (`sepEndBy` symbol ":") $ fix $ \rec -> do
        rv <- option Nothing $ do
            fmap Just $ choice
                [ try $ do
                    x <- pairOrBlockAdverb
                    -- lookAhead (noneOf ",;")
                    return ([x], optional ruleCommaOrSemicolon)
                , do x <- namedArgOr parseExpWithItemOps
                     a <- option [] $ try $ many pairOrBlockAdverb
                     return (x:a, ruleCommaOrSemicolon)
                ]
        case rv of
            Nothing           -> return []
            Just (exp, trail) -> do
                rest <- option [] $ do { trail; rec }
                return (exp ++ rest)
    processFormals formal
-- FAILED PARSER PATCH
{-
parseHasParenParamList = (<?> "paren arg-list") $ try $ verbatimParens $ do
    invocant        <- option Nothing $ try $ do
        inv <- parseExpWithItemOps
        doParseInvocantColon
        return $ Just inv
        
    leadingAdverbs  <- doParseAdverbs
    
    -- use 'sepEndBy', because `foo($bar, $baz,)` is legal
    arguments       <- parseExpWithItemOps `sepEndBy` ruleComma
    
    trailingAdverbs <- doParseAdverbs
    
    return (invocant, leadingAdverbs ++ arguments ++ trailingAdverbs)
    where
    doParseAdverbs = option [] $ try $ many pairOrBlockAdverb
    doParseInvocantColon = do
        -- make sure it's really an invocant colon, and not a :foo() or ::Foo
        -- colon
        -- To get this to work, I had to tweak 'notFollowedBy'
        notFollowedBy pairOrBlockAdverb
        notFollowedBy $ symbol "::"
        symbol ":"
-}
       

{-
Used by:
~~~~~~~~
parseParamList (after trying parseParenParamList)
ruleInvocationParens (<= qInterpolatorPostTerm)
ruleApply (when `foo .($bar)`?) (after whitespace when there's no implicit-inv)

[09:12] <scook0> so really the only difference is that NoParens has to be 
                 careful not to swallow `{}.blah`?
[09:15] <autrijus> nodnod.
-}
parseNoParenParamList :: RuleParser (Maybe Exp, [Exp])
parseNoParenParamList = do
    formal <- formalSegment `sepEndBy1` (symbol ":")
    processFormals formal
    where
    formalSegment :: RuleParser [Exp]
    formalSegment = do
        rv <- option Nothing . fmap Just . choice $
            [ argListEndingBlock
            , argBlockWith pairOrBlockAdverb
            , argVanilla
            ]
        case rv of
            Nothing           -> return []
            Just (exp, trail) -> do
                rest <- option [] $ do { trail; formalSegment }
                return (exp ++ rest)
    argListEndingBlock = do
        x <- ruleBareOrPointyBlockLiteral
        (ruleComma >> return ([x], return ())) <|> do
            -- Unless terminated by a comma, block always end the list right there.
            return ([x], mzero)
    argBlockWith :: RuleParser a -> RuleParser ([a], RuleParser ())
    argBlockWith rule = do
        x <- rule
        optional ruleComma
        return ([x], return ())
    argVanilla :: RuleParser ([Exp], RuleParser ())
    argVanilla = do
        x <- namedArgOr parseExpWithTightOps
        a <- option [] $ try $ many pairOrBlockAdverb
        return (x:a, ruleComma)
        
ruleParamName :: RuleParser String
ruleParamName = literalRule "parameter name" $ do
    -- Valid param names: $foo, @bar, &baz, %grtz, ::baka
    sigil   <- choice [ oneOf "$@%&" >>= return . (:""), string "::" ]
    if sigil == "&"
        then ruleSubNamePossiblyWithTwigil
        else do twigil <- ruleTwigil
                name   <- case twigil of
                    ""  -> many1 wordAny <|> string "/"
                    "!" -> many wordAny
                    _   -> many1 wordAny
                return $ sigil ++ twigil ++ name

ruleVarName :: RuleParser String
ruleVarName = rule "variable name" ruleVarNameString

ruleVarNameString :: RuleParser String
ruleVarNameString =   try (string "$/")  -- match object
                  <|> try ruleMatchPos
                  <|> try ruleMatchNamed
                  <|> try regularVarName
                  <|> string "$!"  -- error variable

regularVarName :: RuleParser String
regularVarName = do
    sigil   <- oneOf "$@%&"
    if sigil == '&' then ruleSubNamePossiblyWithTwigil else do
    --  ^ placeholder, * global, ? magical, . member, ! private member
    twigil  <- ruleTwigil
    -- doesn't handle names /beginning/ with "::"
    name    <- ruleQualifiedIdentifier
    return $ (sigil:twigil) ++ name

ruleDereference :: RuleParser Exp
ruleDereference = try $ do
    sigil   <- oneOf "$@%&"
    exp     <- ruleDereference <|> ruleSigiledVar <|> braces ruleExpression
    return $ Syn (sigil:"{}") [exp]

ruleSigiledVar :: RuleParser Exp
ruleSigiledVar = (<|> ruleSymbolicDeref) . try $ do
    name <- ruleVarNameString
    let (sigil, rest) = span (`elem` "$@%&:") name
    case rest of
        [] -> return (makeVar name)
        _ | any (not . isWordAny) rest -> return (makeVar name)
        _ | all isDigit rest           -> return (makeVar name)
        -- XXX - Required by Test::Harness which uses @INC instead @*INC
        "INC" | "@" <- sigil           -> return (makeVar name)
        _ -> do
            -- Plain and simple variable -- do a lexical check
            state <- get
            let outerLexPad     = envLexical (fromJust (envOuter (ruleEnv state)))
                outerVisible    = isJust (lookupPad name outerLexPad)
                curPads         = Map.elems (ruleBlockPads state)
                curVisible      = any (Map.member name . padEntries) curPads
                inTopLevel      = isNothing (envOuter (ruleEnv state))
            -- If it's visible in the outer lexical scope, yet not
            -- defined in the current scope, then generate OUTER.
            if not inTopLevel && outerVisible && not curVisible
                then return (Var $ sigil ++ "OUTER::" ++ rest)
                else return (makeVar name)

ruleVar :: RuleParser Exp
ruleVar = ruleSigiledVar

ruleSymbolicDeref :: RuleParser Exp
ruleSymbolicDeref = do
    sigil    <- oneOf "$@%&"
    nameExps <- many1 $ try $ do
        string "::"
        -- nameExp is the expression which will yield the varname.
        -- We've to include ruleTwigil here to make $::?SELF parse.
        -- XXX: This looks slightly odd to me -- is one forced to say
        --  $::("?SELF") instead?
        (parens ruleExpression) <|> (fmap (Val . VStr) $ do
            choice
                [ string "!"  --  $!
                , string "/"  --  $/
                , fmap concat $ sequence [ruleTwigil, many1 wordAny] ])
    return $ Syn (sigil:"::()") nameExps

makeVar :: String -> Exp
makeVar (s:"<>") =
    makeVarWithSigil s $ Var "$/"
makeVar (s:rest) | all (`elem` "1234567890") rest =
    makeVarWithSigil s $ Syn "[]" [Var "$/", Val $ VInt (read rest)]
makeVar (s:'<':name) =
    makeVarWithSigil s $ Syn "{}" [Var "$/", doSplitStr (init name)]
makeVar var = Var var

makeVarWithSigil :: Char -> Exp -> Exp
makeVarWithSigil '$' x = x
makeVarWithSigil s   x = Syn (s:"{}") [x]

ruleLit :: RuleParser Exp
ruleLit = do
    lvl <- gets ruleBracketLevel
    let blk | ConditionalBracket <- lvl = id
            | otherwise                 = (ruleBlockLiteral:)
    choice ( ruleDoBlock : blk
        [ numLiteral
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
        ])

nullaryLiteral :: RuleParser Exp
nullaryLiteral = parseExpWithCachedParser dynParseNullary

{-|
Match the literal @undef@, returning an expression representing the undefined
value.
-}
undefLiteral :: RuleParser Exp
undefLiteral = do
    symbol "undef"
    return $ Val VUndef

numLiteral :: RuleParser Exp
numLiteral = do
    n <- naturalOrRat
    -- XXX - This is a hack to allow \b to work with numbers
    --       because Parser.Number is currently not a RuleParser
    modify $ \state -> state{ ruleChar = '0' }
    case n of
        Left  i -> return . Val $ VInt i
        Right d -> return . Val $ VRat d

ruleBracketedExpression :: RuleParser Exp
ruleBracketedExpression = enterBracketLevel ParensBracket $
    ruleExpression <|> do { whiteSpace; return (Syn "," []) }

arrayLiteral :: RuleParser Exp
arrayLiteral = try $ do
    item <- verbatimBrackets ruleBracketedExpression
    return $ Syn "\\[]" [item]

{-|
Match a pair literal -- either an arrow pair (@a => 'b'@), or an adverbial pair
(@:foo('bar')@).
-}
pairLiteral :: RuleParser Exp
pairLiteral = choice [ pairArrow, pairAdverb ]

tryFollowedBy :: RuleParser a -> RuleParser b -> RuleParser a
tryFollowedBy rule after = try $ do
    rv <- rule
    after
    return rv

tryLookAhead :: RuleParser a -> RuleParser b -> RuleParser a
tryLookAhead rule after = try $ do
    rv <- rule
    lookAhead after
    return rv

pairArrow :: RuleParser Exp
pairArrow = do
    key <- identifier `tryFollowedBy` symbol "=>"
    val <- parseExpWithTightOps
    return (Val (VStr key), val)
    return $ App (Var "&infix:=>") Nothing [Val (VStr key), val]

pairAdverb :: RuleParser Exp
pairAdverb = try $ do
    char ':'
    negatedPair <|> shortcutPair <|> regularPair
    where
    negatedPair = do
        char '!'
        key <- many1 wordAny
        return $ App (Var "&infix:=>") Nothing [Val (VStr key), Val (VBool False)]
    shortcutPair = do
        var <- regularVarName
        let key = reverse (takeWhile isWordAny (reverse var))
        return $ App (Var "&infix:=>") Nothing [Val (VStr key), Var var]
    regularPair = do
        key <- many1 wordAny
        val <- option (Val $ VBool True) $ choice [ valueDot, noValue, valueExp ]
        return $ if (all isDigit key)
            then App (Var "&Pugs::Internals::base") Nothing [Val (VStr key), val]
            else App (Var "&infix:=>") Nothing [Val (VStr key), val]
    valueDot = do
        ruleDot
        valueExp
    noValue = do
        mandatoryWhiteSpace
        return (Val $ VBool True)
    valueExp = lexeme $ choice
        [ verbatimParens ruleBracketedExpression
        , arrayLiteral
        , angleBracketLiteral
        ]

-- Interpolating constructs
qInterpolatorChar :: RuleParser Exp
qInterpolatorChar = do
    char '\\'
    nextchar <- escapeCode -- see Lexer.hs
    return (Val $ VStr nextchar)

qInterpolateDelimiter :: Char -> RuleParser Exp
qInterpolateDelimiter protectedChar = do
    char '\\'
    c <- oneOf (protectedChar:"\\")
    return (Val $ VStr [c])

qInterpolateDelimiterMinimal :: Char -> RuleParser Exp
qInterpolateDelimiterMinimal protectedChar = do
    char '\\'
    c <- oneOf (protectedChar:"\\")
    return (Val $ VStr ['\\',c])

qInterpolateDelimiterBalanced :: Char -> RuleParser Exp
qInterpolateDelimiterBalanced protectedChar = do
    char '\\'
    c <- oneOf (protectedChar:balancedDelim protectedChar:"\\")
    return (Val $ VStr ['\\',c])

qInterpolateQuoteConstruct :: RuleParser Exp
qInterpolateQuoteConstruct = try $ do
    string "\\"
    (qStart, qEnd, flags) <- qStructure
    expr <- interpolatingStringLiteral qStart qEnd (qInterpolator flags)
    return expr

-- If we have dot, always consume it
qInterpolatorPostTerm :: RuleParser (Exp -> Exp)
qInterpolatorPostTerm = do
    choice
        [ ruleDot `tryLookAhead` (oneOf "[{(<\xAB" <|> (ruleSubName >> char '('))
        , notFollowedBy (ruleDot >> return '.')
        ]
    choice
        [ ruleArraySubscript
        , ruleHashSubscript
        , ruleCodeSubscript
        , try ruleInvocationParens
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
            QB_Minimal -> try $ qInterpolateDelimiterMinimal $ qfProtectedChar flags
            QB_Balanced -> try $ qInterpolateDelimiterBalanced $ qfProtectedChar flags
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
    (try qLiteralToEof) <|> do
    (qStart, qEnd, flags) <- getQDelim
    if not (qfHereDoc flags) then
        qLiteral1 qStart qEnd flags
      else do -- XXX an ugly kludge providing crude heredocs
        endMarker <- (fmap string $ many1 wordAny)
        qEnd; ruleWhiteSpaceLine
        qLiteral1 (fail "never match") endMarker flags

qLiteralToEof :: RuleParser Exp
qLiteralToEof = do
    string "q_to_eof()"
    source <- many anyChar
    return $ Val $ VStr $ source

qLiteral1 :: RuleParser String    -- Opening delimiter
             -> RuleParser String -- Closing delimiter
             -> QFlags
             -> RuleParser Exp
qLiteral1 qStart qEnd flags = do
    expr <- interpolatingStringLiteral qStart qEnd (qInterpolator flags)
    -- qEnd
    case qfSplitWords flags of
        -- expr ~~ rx:perl5:g/(\S+)/
        QS_Yes      -> return (doSplit expr)
        QS_Protect  -> return (doSplit expr)
        QS_No       -> return expr
    where
    -- words() regards \xa0 as (breaking) whitespace. But \xa0 is
    -- a nonbreaking ws char.
    doSplit (Ann (Cxt (CxtItem _)) (Val (VStr str))) = doSplitStr str
    doSplit expr = App (Var "&infix:~~") Nothing [expr, rxSplit]
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
        qLiteral1 (symbol "<<") (string ">>") $ qqFlags
            { qfSplitWords = QS_Protect, qfProtectedChar = '>' }
    <|> do
        symbol "<"
        qLiteral1 (symbol "<") (string ">") $ qFlags
            { qfSplitWords = QS_Yes, qfProtectedChar = '>' }
    <|> do
        symbol "\xAB"
        qLiteral1 (symbol "\xAB") (string "\xBB") $ qFlags
            { qfSplitWords = QS_Yes, qfProtectedChar = '\xBB' }

-- Quoting delimitor and flags
-- qfProtectedChar is the character to be
--   protected by backslashes, if
--   qfInterpolateBackslash is Minimal or Single or All
data QS_Flag = QS_No | QS_Yes | QS_Protect deriving (Show, Eq, Ord, Typeable)
data QB_Flag = QB_No | QB_Minimal | QB_Balanced | QB_Single | QB_All deriving (Show, Eq, Ord, Typeable)

data QFlags = MkQFlags
    { qfSplitWords              :: !QS_Flag -- No, Yes, Protect
    , qfInterpolateScalar       :: !Bool
    , qfInterpolateArray        :: !Bool
    , qfInterpolateHash         :: !Bool
    , qfInterpolateFunction     :: !Bool
    , qfInterpolateClosure      :: !Bool
    , qfInterpolateBackslash    :: !QB_Flag -- No, Minimal, Single, All
    , qfProtectedChar           :: !Char
    , qfP5RegularExpression     :: !Bool
    , qfHereDoc                 :: !Bool
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
          useflag "t" qf          = qf { qfHereDoc = True }
          useflag "to" qf         = qf { qfHereDoc = True }

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
openingDelim :: RuleParser Char
openingDelim = anyChar

qStructure :: RuleParser (RuleParser String, RuleParser String, QFlags)
qStructure = 
    do string "q"
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
       return ( (string [delim]), (string [balancedDelim delim]), qflags)
    where
       oneflag = do string ":"
                    many alphaNum


getQDelim :: RuleParser (RuleParser String, RuleParser String, QFlags)
getQDelim = try qStructure
    <|> try (do
        string "<<"
        return (string "<<", string ">>",
            qqFlags { qfSplitWords = QS_Yes, qfProtectedChar = '>' }))
    <|> do
        delim <- oneOf "\"'<\xab"
        case delim of
            '"'     -> return (string "\"",  string "\"",    qqFlags)
            '\''    -> return (string "'",   string "'",   qFlags)
            '<'     -> return (string "<",   string ">",    qFlags
                { qfSplitWords = QS_Yes, qfProtectedChar = '>' })
            '\xab'  -> return (string "\xab", string "\xbb", qqFlags
                { qfSplitWords = QS_Protect, qfProtectedChar = '\xbb' })
            _       -> fail ""


-- | Default flags
qFlags    :: QFlags
qFlags    = MkQFlags QS_No False False False False False QB_Single '\'' False False False
-- | Default flags
qqFlags   :: QFlags
qqFlags   = MkQFlags QS_No True True True True True QB_All '"' False False False
-- | Default flags
rawFlags  :: QFlags
rawFlags  = MkQFlags QS_No False False False False False QB_No 'x' False False False
-- | Default flags
rxP5Flags :: QFlags
rxP5Flags = MkQFlags QS_No True True True True False QB_Balanced '/' True False False
-- | Default flags
rxP6Flags :: QFlags
rxP6Flags = MkQFlags QS_No False False False False False QB_Balanced '/' False False False

-- Regexps

-- | A parser returning a regex, given a hashref of adverbs and a closing delimiter.
rxLiteralAny :: Exp -> Char -> Char -> RuleParser Exp
rxLiteralAny adverbs
    | Syn "\\{}" [Syn "," pairs] <- adverbs
    , not (null [
        True
        | (App (Var "&infix:=>") Nothing [Val (VStr name), _]) <- pairs
        , (name ==) `any` words "P5 Perl5 perl5"
        ])
    = rxLiteral5
    | otherwise
    = rxLiteral6

rxLiteral5 :: Char -- ^ Opening delimiter
           -> Char -- ^ Closing delimiter
           -> RuleParser Exp
rxLiteral5 delimStart delimEnd = qLiteral1 (string [delimStart]) (string [delimEnd]) $
    rxP5Flags { qfProtectedChar = delimStart }

rxLiteral6 :: Char -- ^ Opening delimiter
           -> Char -- ^ Closing delimiter
           -> RuleParser Exp
rxLiteral6 delimStart delimEnd = qLiteral1 (string [delimStart]) (string [delimEnd]) $
    rxP6Flags { qfProtectedChar = delimStart }


ruleAdverbHash :: RuleParser Exp
ruleAdverbHash = do
    pairs <- many pairAdverb
    return $ Syn "\\{}" [Syn "," pairs]

substLiteral :: RuleParser Exp
substLiteral = do
    symbol "s"
    adverbs <- ruleAdverbHash
    ch      <- openingDelim
    let endch = balancedDelim ch
    -- XXX - probe for adverbs to determine p5 vs p6
    expr    <- rxLiteralAny adverbs ch endch
    ch      <- if ch == endch then return ch else do { whiteSpace ; anyChar }
    let endch = balancedDelim ch
    subst   <- qLiteral1 (string [ch]) (string [endch]) qqFlags { qfProtectedChar = endch }
    return $ Syn "subst" [expr, subst, adverbs]

ruleRegexDeclarator :: RuleParser String
ruleRegexDeclarator = symbol "rule" <|> symbol "token" <|> symbol "regex"

rxLiteral :: RuleParser Exp
rxLiteral = do
    sym     <- symbol "rx" <|> do { symbol "m"; return "match" } <|> do
        -- XXX - fill in default adverbs
        ruleRegexDeclarator
        lookAhead $ do { ruleAdverbHash; char '{' }
        return "rx"
    adverbs <- ruleAdverbHash
    ch      <- anyChar
    expr    <- rxLiteralAny adverbs ch (balancedDelim ch)
    return $ Syn sym [expr, adverbs]

rxLiteralBare :: RuleParser Exp
rxLiteralBare = do
    ch      <- char '/'
    expr    <- rxLiteral6 ch (balancedDelim ch)
    return $ Syn "//" [expr, Val undef]

