{-# OPTIONS_GHC -cpp -fglasgow-exts -funbox-strict-fields -fno-full-laziness -fno-cse -fallow-overlapping-instances #-}

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

    -- Circularity: Used in Pugs.Parser.Operator
    parseTerm, parseNoParenParamList, ruleSubName,

    -- Circularity: Used in Pugs.Parser.Literal
    ruleExpression,
    ruleArraySubscript, ruleHashSubscript, ruleCodeSubscript,
    ruleInvocationParens, verbatimVarNameString, ruleVerbatimBlock,
    ruleBlockLiteral, ruleDoBlock, regularVarName, ruleNamedMethodCall,
) where
import Pugs.Internals
import Pugs.AST
import qualified Pugs.Exp as Exp
import Pugs.Types
import Pugs.Version (versnum)
import Pugs.Lexer
import Pugs.Rule

import Pugs.Parser.Types
import Pugs.Parser.Unsafe
import Pugs.Parser.Export
import Pugs.Parser.Operator
import Pugs.Parser.Doc
import Pugs.Parser.Literal
import Pugs.Parser.Util
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Lexical units --------------------------------------------------

ruleBlock :: RuleParser Exp
ruleBlock = do
    lvl <- gets s_bracketLevel
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
    f   <- ruleStatementModifier
    f exp

ruleStatementModifier :: RuleParser (Exp -> RuleParser Exp)
ruleStatementModifier = verbatimRule "statement modifier" . option return $ choice
    [ s_postConditional
    , s_postLoop
    , s_postIterate
    ]

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

data ImplicitSub = ImplicitNil | ImplicitMulti | ImplicitProto deriving (Eq)

ruleSubHead :: RuleParser (Bool, SubType, String)
ruleSubHead = rule "subroutine head" $ do
    prefix <- choice
        [ symbol "multi" >> return ImplicitMulti
        , symbol "proto" >> return ImplicitProto
        , return ImplicitNil
        ]

    -- You're allowed to omit the "sub":
    --   multi sub foo (...) {...}      # legal
    --         sub foo (...) {...}      # legal, too
    let implicitSub | ImplicitNil <- prefix = pzero
                    | otherwise             = return SubRoutine
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
    return (prefix == ImplicitMulti, styp, name)

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
    (withAdvs, name) <- try $ do
        advs <- ruleRegexDeclarator
        fmap ((,) advs) identifier
    adverbs <- fmap withAdvs ruleAdverbHash
    ch      <- char '{'
    expr    <- rxLiteralAny adverbs ch (balancedDelim ch)
    let exp = Syn ":=" [_Var ('<':'*':name), Syn "rx" [expr, adverbs]]
    unsafeEvalExp (_Sym SGlobal ('<':'*':name) exp)
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
        Just SOur -> return $ cast (envPackage env) ++ "::" ++ name
        Nothing   -> return name
        _         -> fail "I only know about package- and global-scoped classes. Sorry."
    traits  <- many $ ruleTrait ["is", "does"]
    let pkgClass = case sym of
                       "package" -> "Package"
                       "module"  -> "Module"
                       "class"   -> "Class"
                       "role"    -> "Class" -- XXX - Wrong - need metamodel
                       "grammar" -> "Grammar"
                       _ -> fail "bug"
        mixinRoles = nub ([ cls | ("does", cls) <- traits])
        parentClasses = nub ("Object":[ cls | ("is", cls) <- traits, cls /= "also" ])
    case () of
        _ | elem name parentClasses -> do
            return (Left $ "Circular class inheritance detected for " ++ sym ++ " '" ++ name ++ "'")
        _ | elem name mixinRoles -> do
            return (Left $ "Circular role composition detected for " ++ sym ++ " '" ++ name ++ "'")
        _ -> do
            unsafeEvalExp (newPackage pkgClass newName parentClasses mixinRoles)
            modify $ \state -> state
                { s_env = (s_env state)
                    { envPackage = cast newName
                    , envClasses = envClasses env `addNode` mkType newName
                    }
                , s_dynParsers = MkDynParsersEmpty
                }
            let pkgVal = Val . VStr $ newName
                kind   = Val . VStr $ sym
            return $ Right (newName, kind, pkgVal, env)

ruleTraitsIsOnly :: RuleParser [String]
ruleTraitsIsOnly = fmap (map snd) . many $ ruleTrait ["is"]

ruleSubDeclaration :: RuleParser Exp
ruleSubDeclaration = rule "subroutine declaration" $ do
    namePos <- getPosition
    (scope, typ, isMulti, styp, name) <- choice
        [ ruleSubScopedWithContext
        , ruleSubScoped
        , ruleSubGlobal
        ]
    optional $ do { symbol "handles"; ruleExpression }
    assoc   <- option A_left . try $ do
        symbol "is"
        symbol "assoc"
        lit <- parens qLiteral
        case unwrap lit of
            Val (VStr str) -> case str of
                "left"  -> return A_left
                "right" -> return A_right
                "non"   -> return A_non
                "chain" -> return A_chain
                "list"  -> return A_list
                _       -> fail $ "Invalid associativity: " ++ str
            _   -> fail $ "Invalid associativity: " ++ show lit
    let returnsOrOf = try (ruleBareTrait "returns" <|> ruleBareTrait "of")
    typ'    <- option typ returnsOrOf
    formal  <- option Nothing $ ruleSubParameters ParensMandatory
    typ''   <- option typ' returnsOrOf
    traits  <- ruleTraitsIsOnly

    -- XXX - We have the prototype now; install it immediately?

    -- bodyPos <- getPosition
    body    <- ruleBlock
    let (fun, names, params) = doExtract styp formal body
    -- Check for placeholder vs formal parameters
    when (isJust formal && (not.null) names) $
        fail "Cannot mix placeholder variables with formal parameters"
    env <- ask
    let sub = VCode $ mkCode
            { isMulti       = isMulti
            , subName       = cast nameQualified
            , subEnv        = Just env
            , subType       = if "primitive" `elem` traits
                then SubPrim else styp
            , subAssoc      = case v_categ var of
                C_infix -> assoc
                _       -> ANil
            , subReturns    = mkType typ''
            , subLValue     = "rw" `elem` traits
            , subParams     = self ++ paramsFor styp formal params
            , subBindings   = []
            , subSlurpLimit = []
            , subBody       = fun
            , subCont       = Nothing
            }
        pkg = cast (envPackage env)
        nameQualified | ':' `elem` name     = name
                      | scope <= SMy        = name
                      | isGlobal            = name
                      | isBuiltin           = (head name:'*':tail name)
                      | otherwise           = (head name:pkg) ++ "::" ++ tail name
        self :: [Param]
        self | styp > SubMethod = []
             | (prm:_) <- params, isInvocant prm = []
             | otherwise = [selfParam . cast $ envPackage env]
        var = cast nameQualified
        mkExp n = Syn ":=" [_Var n, Syn "sub" [Val sub]]
        mkSym n = _Sym scope (mkMulti n) (mkExp n)
        -- Horrible hack! _Sym "&&" is the multi form.
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
        traceM 
            ("You probably don't want to export an operator name; instead\n\
  define a new variant on the new operator (eg. multi sub *infix:<+>): "
                ++ show name ++ " at " ++ show namePos)
            
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
                App (_Var "&push")
                    (Just (Syn "{}" [_Var ("%" ++ pkg ++ "::EXPORTS"), Val $ VStr name]))
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
    return $ ('&':twigil) ++ name

ruleSubName :: RuleParser String
ruleSubName = verbatimRule "subroutine name" $ do
    twigil  <- option "" (string "*")
    name <- ruleOperatorName <|> ruleQualifiedIdentifier
    return $ ('&':twigil) ++ name

ruleOperatorName :: RuleParser String
ruleOperatorName = verbatimRule "operator name" $ do
    fixity  <- choice (map (try . string) fixities) `tryLookAhead` (oneOf "\xAB<{")
    name    <- do
        -- char ':'
        sub <- ruleHashSubscript
        -- Not exactly un-evil
        let (Syn "{}" [_, expr]) = sub (Val VUndef)
        Val (VStr name) <- unsafeEvalExp $
            App (_Var "&*join") 
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
    traits  <- ruleTraitsIsOnly
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
    let sigil'      = (if isOptional then '?' else '!'):sigil1
        defaultExp  = case name of
            ('@':_) -> Val (VList [])
            ('%':_) -> Val (VList [])
            _       -> Noop
    exp <- case opt of
        FormalsSimple -> return defaultExp
        FormalsComplex -> do
            rv <- ruleParamDefault (not isOptional)
            optional $ do
                symbol "-->"
                ruleParamList ParensOptional $ choice
                    [ ruleType
                    , ruleFormalParam FormalsComplex >> return ""
                    ]
            return $ case rv of
                Noop -> defaultExp
                _    -> rv
    return $ foldr appTrait (buildParam typ sigil' name exp) traits
    where
    appTrait "rw"   x = x { isWritable = True }
    appTrait "copy" x = x { isLValue = False, isWritable = True }
    appTrait "lazy" x = x { isLazy = True }
    appTrait "context" x = x { paramName = (paramName x){ v_twigil = TImplicit } }
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
    (aux, trait) <- ruleTrait ["is", "does"]
    lookAhead (eof <|> (oneOf ";}" >> return ()))
    env     <- ask
    let pkg = _Var (':':'*':cast (envPackage env))
    return $ Syn "="
        [ Syn "{}" [pkg, Val (VStr aux)]
        , Syn "," [Syn "@{}" [Syn "{}" [pkg, Val (VStr aux)]], Val (VStr trait)]
        ]

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
    traits  <- ruleTraitsIsOnly
    optional $ do { symbol "handles"; ruleExpression }
    env     <- ask
    -- manufacture an accessor
    let sub = mkPrim
            { isMulti       = False
            , subName       = cast name
            , subEnv        = Nothing
            , subReturns    = if null typ then typeOfSigil (cast sigil) else mkType typ
            , subBody       = fun
            , subParams     = [selfParam $ cast $ envPackage env]
            , subLValue     = "rw" `elem` traits
            , subType       = SubMethod
            }
        exp = Syn ":=" [_Var name, Syn "sub" [Val $ VCode sub]]
        name | twigil == '.' = '&':(pkg ++ "::" ++ key)
             | otherwise     = '&':(pkg ++ "::" ++ (twigil:key))
        fun = Ann (Cxt (cxtOfSigil $ cast sigil)) (Syn "{}" [_Var "&self", Val (VStr key)])
        pkg = cast (envPackage env)
    unsafeEvalExp (_Sym SGlobal name exp)
    return emptyExp

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
    -- state $x = 42 is really syntax sugar for state $x; START { $x = 42 }
    -- XXX always wrap the Pad in a Stmts so that expRule has something to unwrap
    case scope of
        SState -> do
            implicit_first_block <- vcode2startBlock $ VCode mkSub { subBody = rhs }
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
    {-
    optional $ do
        variant <- ruleAuthorPart
        when (map toLower variant /= "pugs") $ do
            pos <- getPosition
            error $ "Perl implementation " ++ tail variant ++ " required--this is only Pugs v" ++ versnum ++ ", stopped at " ++ (show pos)
    -}
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
            then Stmts (_Sym SGlobal (':':'*':pkg)
                            (Syn ":=" [ _Var (':':'*':pkg)
                               , App (_Var "&require_perl5") Nothing [Val $ VStr pkg] ]))
                       (newMetaType pkg) -- Perl5's ::CGI is same as ::CGI.meta
            else (App (_Var "&use") Nothing [Val $ VStr pkg])
        modify $ \state -> state
            { s_env = env
                { envClasses = envClasses env' `addNode` mkType pkg
                , envGlobal  = envGlobal env'
                }
            , s_dynParsers = MkDynParsersEmpty
            }
    try (do { verbatimParens whiteSpace ; return emptyExp}) <|> do
        imp <- option emptyExp ruleExpression
        let sub = _Var $ ('&':pkg) ++ if use then "::import" else "::unimport"
        unsafeEvalExp $ Syn "if"
            [ sub
            , App sub (Just $ Val $ VStr $ pkg) [imp]
            , emptyExp
            ]

        Val (VList exportList) <- unsafeEvalExp $ case lang of
            -- map { ~$_, [::Pkg.can($_)] }, @importlist
            "perl5" -> App (_Var "&map") Nothing [Syn "sub"
                [ Val . VCode $ mkSub
                    { subBody   = Syn ","
                        [ App (_Var "&prefix:<~>") (Just $ _Var "$_") []
                        , Syn "\\[]" [ App (_Var "&can") (Just $ _Var (':':'*':pkg)) [_Var "$_"] ]
                        ]
                    , subParams = [defaultScalarParam]
                    }
                ], imp ]
            -- %Pkg::EXPORTS.kv
            _ -> App (_Var "&kv") (Just $ _Var ('%':pkg ++ "::EXPORTS")) []

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
            return $ App (_Var "&PIL2JS::Internals::use_jsan_module_noimp") Nothing [name]
        , do
            exp <- option emptyExp ruleExpression
            let exp' | exp == emptyExp = []
                     | otherwise       = [exp]
            return $ App (_Var "&PIL2JS::Internals::use_jsan_module_imp") Nothing $ name:exp'
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
            return $ App (_Var "&PIL2JS::Internals::use_perl5_module_noimp") Nothing [name]
        , do
            exp <- option emptyExp ruleExpression
            let exp' | exp == emptyExp = []
                     | otherwise       = [exp]
            return $ App (_Var "&PIL2JS::Internals::use_perl5_module_imp") Nothing $ name:exp'
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
        App (Var var) Nothing exp | var == cast "&infix:=>" -> do
            return $ Syn "inline" exp
        _ -> fail "not yet parsed"

{-|
Match a @require@ declaration, returning a sub call 'Pugs.AST.Exp' that will
load the package at runtime.

(This should probably be merged with 'ruleUseDeclaration' & friends, if
anybody has some tuits.)
-}
ruleRequireDeclaration :: RuleParser Exp
ruleRequireDeclaration = tryRule "require declaration" $ do
    symbol "require"
    (names, _, _) <- rulePackageFullName
    return $ App (_Var "&require") Nothing [Val . VStr $ concat (intersperse "::" names)]

ruleDoBlock :: RuleParser Exp
ruleDoBlock = rule "do block" $ do
    symbol "do"
    enterBracketLevel StatementBracket $ choice
        [ do { rv <- ruleBlock ; notFollowedBy (ruleStatementModifier >> return ' ') ; return rv }
        , ruleBlockDeclaration
        , ruleDeclaration
        , ruleConstruct
        , ruleStatement
        ]

ruleClosureTrait :: Bool -> RuleParser Exp
ruleClosureTrait rhs = tryRule "closure trait" $ do
    let rhsTraits = words " BEGIN CHECK INIT START ENTER FIRST "
    let names = words " BEGIN CHECK INIT END START ENTER LEAVE KEEP UNDO FIRST NEXT LAST PRE POST CATCH CONTROL"
    name    <- choice $ map symbol $ names
    block   <- ruleBlock
    when (rhs && not (name `elem` rhsTraits)) $
        fail (name ++ " may only be used at statement level")
    let (fun, params) = extractPlaceholderVars block []
    -- Check for placeholder vs formal parameters
    when (params /= [] && params /= [cast "$_"]) $
        fail "Closure traits take no formal parameters"
    env <- ask
    let code = VCode mkSub { subName = cast name, subBody = fun, subEnv = Just env } 
    case name of
        "END"   -> do
            -- We unshift END blocks to @*END at compile-time.
            -- They're then run at the end of runtime or at the end of the
            -- whole program.
            unsafeEvalExp $ 
                App (_Var "&unshift")
                    (Just (_Var "@*END"))
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
            -- we need to clone this closure sometimes
        "START" -> vcode2startBlock code
        _       -> do 
            let (VCode code') = code
            addClosureTrait name code'
            return emptyExp --retBlock SubBlock Nothing False block 
            -- XXX Not the right thing to return
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
        [ _Var "@main::END"
        , Syn "sub"
            [ Val . VCode $ mkSub
                { subBody   = App (_Var "$_") Nothing []
                , subParams = [defaultScalarParam]
                }
            ]
        ]
    -- ...and then exit.
    return $ unsafePerformIO $ exitWith exit
possiblyExit x = return x

vcode2startBlock :: Val -> RuleParser Exp
vcode2startBlock code = do
    -- Ok. Now the tricky thing.
    -- This is the general idea:
    -- START { 42 } is transformed into
    -- {
    --   # XXX these should not be $? vars!!!
    --   state $?START_RESULT;
    --   state $?START_RUN;
    --   $?START_RUN++ ?? $?START_RESULT !! $?START_RESULT = { 42 }();
    -- }
    -- These are the two state variables we need.
    -- This will soon add our two state vars to our pad
    lexDiff <- unsafeEvalLexDiff $
        (_Sym SState "$?START_RESULT") . (_Sym SState "$?START_RUN") $ emptyExp
    -- And that's the transformation part.
    return $ Syn "block"        -- The outer block
        [ Pad SState lexDiff $  -- state ($?START_RESULT, $?START_RUN);
            Syn "if"
                [ App (_Var "&postfix:++") Nothing [_Var "$?START_RUN"]
                , _Var "$?START_RESULT"
                , Syn "=" [_Var "$?START_RESULT", App (Syn "sub" [Val code]) Nothing []]
                ]   --  { $?START_RUN++; $?START_RESULT = { 42 }() };
        ]

vcode2initBlock :: Val -> RuleParser Exp
vcode2initBlock code = do
    body <- vcode2startBlock code
    fstcode <- unsafeEvalExp $ Syn "sub" [ Val $ VCode mkSub { subBody = body } ]
    unsafeEvalExp $
        App (_Var "&push") (Just $ _Var "@*INIT") [ fstcode ]
    return $ App fstcode Nothing []

vcode2checkBlock :: Val -> RuleParser Exp
vcode2checkBlock code = do
    body <- vcode2startBlock code
    fstcode <- unsafeEvalExp $ 
        Syn "sub" [ Val $ VCode mkSub { subBody = checkForIOLeak body } ]
    unsafeEvalExp $
        App (_Var "&unshift") (Just $ _Var "@*CHECK") [ fstcode ]
    return $ App fstcode Nothing []

-- Constructs ------------------------------------------------

ruleConstruct :: RuleParser Exp
ruleConstruct = rule "construct" $ choice
    [ ruleForConstruct
    , ruleLoopConstruct
    , ruleRepeatConstruct
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
    cond    <- ruleCondPart
    body    <- enterBracketLevel ParensBracket $ ruleBlockLiteral
    return $ Syn "for" [cond, body]

ruleLoopConstruct :: RuleParser Exp
ruleLoopConstruct = rule "loop construct" $ do
    symbol "loop"
    choice [ ruleSemiLoopConstruct, ruleBareLoopConstruct ]

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

ruleBareLoopConstruct :: RuleParser Exp
ruleBareLoopConstruct = rule "for-like loop construct" $ do
    block <- ruleBlock
    return $ Syn "loop" ([] ++ [block])

ruleRepeatConstruct :: RuleParser Exp
ruleRepeatConstruct = rule "postfix loop construct" $ do
    symbol "repeat"
    choice [ ruleRepeatPostConstruct, ruleRepeatPreConstruct ]

ruleRepeatPostConstruct :: RuleParser Exp
ruleRepeatPostConstruct = rule "repeat postfix construct" $ do
    block   <- ruleBareOrPointyBlockLiteralWithoutDefaultParams
    option (Syn "loop" [block]) $ do
        name <- choice [ symbol "while", symbol "until" ]
        cond <- ruleExpression
        return $ Syn ("post" ++ name) [cond, block]

ruleRepeatPreConstruct :: RuleParser Exp
ruleRepeatPreConstruct = rule "repeat prefix construct" $ do
    name    <- choice [ symbol "while", symbol "until" ]
    cond    <- ruleCondPart
    block   <- enterBracketLevel ParensBracket $ ruleBareOrPointyBlockLiteralWithoutDefaultParams
    return $ Syn ("post" ++ name) [ cond, block ]

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
    sym     <- choice [ symbol "while", symbol "until" ]
    cond    <- ruleCondPart
    body    <- enterBracketLevel ParensBracket $ ruleBareOrPointyBlockLiteralWithoutDefaultParams
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
s_postConditional :: RuleParser (Exp -> RuleParser Exp)
s_postConditional = rule "postfix conditional" $ do
    cond <- choice $ map symbol ["if", "unless"]
    exp <- ruleExpression
    return $ \body -> return $ Syn cond [exp, body, emptyExp]

{-|
Match a statement's /looping/ statement-modifier,
e.g. '@procrastinate while $bored@' or '@eat until $full@'.

Returns a function that will take the statement proper, and enclose it in an
appropriate 'Pugs.AST.Internals.Syn' (either @\"while\"@ or @\"until\"@).
-}
s_postLoop :: RuleParser (Exp -> RuleParser Exp)
s_postLoop = rule "postfix loop" $ do
    cond    <- choice $ map symbol ["while", "until"]
    exp     <- ruleExpression
    return $ \body -> do
        block <- retBlockWithoutDefaultParams SubBlock Nothing False body
        return $ Syn cond [exp, block]

{-|
Match a statement's /iterating/ statement-modifier,
e.g. '@say for 1..10@'.

Returns a function that will take the statement proper, and enclose it in
@'Pugs.AST.Internals.Syn' \"for\"@.
-}
s_postIterate :: RuleParser (Exp -> RuleParser Exp)
s_postIterate = rule "postfix iteration" $ do
    cond <- choice $ map symbol ["for", "given"]
    exp <- ruleExpression
    return $ \body -> do
        block <- retBlock SubBlock Nothing False body
        return $ Syn ("postfix:" ++ cond) [exp, block]

ruleBareOrPointyBlockLiteralWithoutDefaultParams :: RuleParser Exp
ruleBareOrPointyBlockLiteralWithoutDefaultParams = rule "bare or pointy block construct" $ do
    (styp, formal, lvalue) <- option (SubBlock, Nothing, False) ruleBlockFormalPointy
    -- Cancel out the default $_ on blocks
    body    <- ruleBlock
    retBlockWithoutDefaultParams styp formal lvalue body

retBlockWithoutDefaultParams :: SubType -> Maybe [Param] -> Bool -> Exp -> RuleParser Exp
retBlockWithoutDefaultParams styp formal lvalue body = do
    blk <- retVerbatimBlock styp formal lvalue body
    return $ runIdentity (transformExp deParam blk)
    where
    deParam (Syn "sub" [Val (VCode sub@MkCode{ subParams = prms })]) = do
        return (Syn "sub" [Val $ VCode sub{ subParams = maybe [] (const $ prms) formal}])
    deParam x = return x

ruleBareOrPointyBlockLiteral :: RuleParser Exp
ruleBareOrPointyBlockLiteral = rule "bare or pointy block construct" $
    ruleBlockVariants [ ruleBlockFormalPointy ]

ruleBlockLiteral :: RuleParser Exp
ruleBlockLiteral = rule "block construct" $
    ruleBlockVariants [ ruleBlockFormalPointy, ruleBlockFormalStandard ]

ruleBlockVariants :: [RuleParser (SubType, Maybe [Param], Bool)] -> RuleParser Exp
ruleBlockVariants variants = do
    (styp, formal, lvalue) <- option (SubBlock, Nothing, False)
        $ choice variants
    body <- ruleBlock
    retBlock styp formal lvalue body


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
    let sub = mkCode
            { isMulti       = False
            , subName       = __"<anon>"
            , subEnv        = Just env
            , subType       = styp
            , subAssoc      = ANil
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
    traits <- ruleTraitsIsOnly
    return $ (styp, params, "rw" `elem` traits)

ruleBlockFormalPointy :: RuleParser (SubType, Maybe [Param], Bool)
ruleBlockFormalPointy = rule "pointy block parameters" $ do
    symbol "->"
    params <- ruleSubParameters ParensOptional
    traits <- ruleTraitsIsOnly
    return $ (SubPointy, params, "rw" `elem` traits)


ruleVarDecl :: RuleParser Exp
ruleVarDecl = rule "variable declaration" $ do
    scope   <- ruleScope
    -- XXX - the treatment in the cases below is wrong; "is context"
    --       should be made part of the Pad annotations, as with the
    --       constraints; for now we abuse ruleFormalParam to add an
    --       extra "+" as part of the name when "is context" is seen,
    --       so we can rewrite the declarator to SEnv, but it's Wrong.
    (nameTypes, exp, seenIsContextXXX) <- try oneDecl <|> manyDecl
    let makeBinding (name, typ)
            | ('$':_) <- name, typ /= anyType   = mkSym . bindSym
            | otherwise                         = mkSym
            where
            mkSym   = _Sym scope' name
            bindSym = Stmts (Syn "=" [_Var name, Val (VType typ)])
        scope' = if seenIsContextXXX then SEnv else scope -- XXX Hack
    lexDiff <- unsafeEvalLexDiff $ combine (map makeBinding nameTypes) emptyExp
    -- Now hoist the lexDiff to the current block
    addBlockPad scope' lexDiff
    return (Ann (Decl scope') exp)
    where
    oneDecl = do
        param <- ruleFormalParam FormalsSimple
        let var  = paramName param
            name = cast var{ v_twigil = TNil }
            seenIsContextXXX = v_twigil var == TImplicit
            typ  = typeOfCxt (paramContext param)
            nameType = (name, typ)
        return ([nameType], makeExpFromNameType nameType, seenIsContextXXX)
    manyDecl = do
        defType <- option "" $ ruleType
        params  <- verbatimParens . enterBracketLevel ParensBracket $
            ruleFormalParam FormalsComplex `sepBy1` ruleComma
        let vars  = map paramName params
            names = map (\v -> cast v{ v_twigil = TNil }) vars
            types = map (maybeDefaultType . typeOfCxt . paramContext) params
            maybeDefaultType t
                | t == anyType, defType /= ""   = mkType defType
                | otherwise                     = t
            seenIsContextXXX = any ((== TImplicit) . v_twigil) vars
            nameTypes = names `zip` types
        return (nameTypes, Syn "," $ map makeExpFromNameType nameTypes, seenIsContextXXX)
    -- Note that the reassignment below is _wrong_ when scope is SState.
    makeExpFromNameType (name@('$':_), typ)
        | typ /= anyType
        = Syn "=" [_Var name, Val (VType typ)]
    makeExpFromNameType (name, _)
        = _Var name

parseTerm :: RuleParser Exp
parseTerm = rule "term" $! do
    term <- choice
        [ ruleDereference
        , ruleSignatureVal  -- must come before ruleTypeVar
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
        -- Hack - use an empty Syn to defeat isScalarLValue checking
        --        so that ($x) = f() gives list context.
        , fmap (Ann Parens) (verbatimParens ruleBracketedExpression)
        ] 
    cls  <- getPrevCharClass
    case cls of
        SpaceClass -> return term
        _ -> do
            -- s_postTerm returns an (Exp -> Exp) that we apply to the original term
            fs <- many s_postTerm
            return $! combine (reverse fs) term

ruleSignatureVal :: RuleParser Exp
ruleSignatureVal = rule "signature value" $ do
    between (symbol ":(") (lexeme $ char ')') ruleSignature

data Paramdec = MkParamdec
    { p_param      :: SigParam
    , p_isNamed    :: Bool
    , p_isRequired :: Bool
    , p_isSlurpy   :: Bool
    }
    deriving (Show)

-- XXX - Unused?
_dummyParam :: SigParam
_dummyParam = MkParam
    { p_variable    = varNullScalar
    , p_types       = []
    , p_constraints = []
    , p_unpacking   = Nothing
    , p_default     = MkParamDefault Nothing
    , p_label       = nullID
    , p_slots       = Map.empty
    , p_hasAccess   = AccessRO
    , p_isRef       = False
    , p_isContext   = False
    , p_isLazy      = False
    }

ruleSignature :: RuleParser Exp
ruleSignature = rule "signature" $ do
    -- Note that :(:$x) is naturally one named parameter here.
    (inv, params) <- option (Nothing, []) $ do
        first <- ruleParam
        option (Nothing, [first]) $ do
            sep     <- lexeme (oneOf ":,")
            rest    <- ruleParam `sepEndBy` (lexeme $ char ',')
            return $ case sep of
                ':' -> (Just $ p_param first, rest)
                ',' -> (Nothing, first:rest)
                _   -> error "Can't happen"
    whiteSpace
    reqPosC <- validateRequired True params
    let reqNms   = Set.fromList
            [ p_label p | MkParamdec{ p_param = p, p_isNamed = True, p_isRequired = True } <- params]
        nmSt     = Map.fromList
            [ (p_label p, p) | MkParamdec{ p_param = p, p_isNamed = True } <- params]
        posLs    = [ p | MkParamdec{ p_param = p, p_isNamed = False } <- params ]
        slpScLs  = []
        slpArrLs = Nothing
        slpHsh   = Nothing
        slpCd    = Nothing
        slpCapt  = Nothing
    return $ Val $ VV $ val $ case inv of
        Nothing -> SigSubSingle    reqPosC reqNms posLs nmSt slpScLs slpArrLs slpHsh slpCd slpCapt
        Just i  -> SigMethSingle i reqPosC reqNms posLs nmSt slpScLs slpArrLs slpHsh slpCd slpCapt
    where
        validateRequired _     []     = return 0
        validateRequired False (x:_)
            | isReqPos x              = fail $ "Required parameter cannot come after optional ones: " ++ show x
        validateRequired _     (x:xs) = do
            next <- validateRequired (isReqPos x) xs
            return $ (fromEnum $ isReqPos x) + next
        isReqPos x = p_isRequired x && (not $ p_isNamed x)

ruleParam :: RuleParser Paramdec
ruleParam = rule "parameter" $ do
    staticTypes            <- rStaticTypes
    isSlurpy               <- option False (char '*' >> return True)
    (name, label, isNamed) <- rParamName
    isOptional             <- choice
        [ lexeme $ char '!' >> return False
        , lexeme $ char '?' >> return True
        , whiteSpace >> lookAhead anyChar >>= return . ('=' ==) -- XXX: is this horribly inefficient?
        ]
    def           <- rDefault isOptional
    traits        <- many $ ruleTrait ["is", "does"]
    unpacking     <- rPostVarUnpacking
    code          <- rCode
    {- setTrait scans the traits list for "interesting" values, weeding
     - them out. The last interesting value is returned.
     - We can't let-shadow 'traits', because it's an action :-(
     - XXX - Eventually rewrite with foldr. -}
    let (traits',   access') = setTrait access  AccessRO traits
    let (traits'',  ref')    = setTrait ref     False    traits'
    let (traits''', lazy')   = setTrait lazy    False    traits''
    let (traits'''',context')= setTrait context False    traits'''
    let slots = Map.fromList [(cast t, val $ ((cast True) :: PureBit)) | ("is", t) <- traits'''']
    let isRequired = (not isSlurpy) && ((not isOptional) || (Map.member (cast "required") slots))
    when (isOptional && isRequired) failReqDef -- XXX is required(False)
    let p = MkParam { p_variable    = cast name
                    , p_types       = staticTypes
                    , p_constraints = code
                    , p_unpacking   = unpacking
                    , p_default     = def
                    , p_label       = label
                    , p_slots       = slots
                    , p_hasAccess   = access'
                    , p_isRef       = ref'
                    , p_isLazy      = lazy'
                    , p_isContext   = context'
                    }
    return MkParamdec
        { p_param       = p
        , p_isRequired  = isRequired
        , p_isSlurpy    = isSlurpy
        , p_isNamed     = isNamed
        }
    where
    rStaticTypes = do
        ty <- many $ lexeme ruleQualifiedIdentifier
        return $ map (MkType . cast) ty
    rParamName = choice
        [ do -- named parameter
            char ':'
            choice
                [ do  -- with implicit label
                    name <- regularVarName
                    return (name, label name, True)
                , do  -- with explicit label
                    explicitLabel <- many1 identLetter
                    name <- verbatimParens regularVarName
                    return (name, cast explicitLabel, True)
                ]
        , do -- positional parameter
            name <- regularVarWithOptionalName
            return (name, label name, False)
        ]
        where
        label = cast $ dropWhile (not . isAlpha)
    rDefault True = lexeme $ option (MkParamDefault Nothing) $ do
        symbol "="
        fmap (MkParamDefault . Just . Exp.EE . Exp.MkExpEmeritus) parseTerm
    rDefault False = do
        ch <- lookAhead anyChar
        when (ch == '=') failReqDef
        return $ MkParamDefault Nothing
    rPostVarUnpacking = lexeme $ option Nothing $ try $ do
        optional $ char ':'
        (Val (VV (sig'))) <- verbatimParens ruleSignature
        (sig :: Sig) <- castVal sig'
        return $ Just sig
    rCode = lexeme $ do
        many $ do
            symbol "where"
            lexeme $ ruleVerbatimBlock
        return [] -- We don't have Exp -> Pugs.Val.Code, too bad.
    setTrait :: (a -> Maybe b) -> b -> [a] -> ([a], b)
    setTrait f d' l = doSetTrait d' [] l where
        doSetTrait d acc []     = (acc, d)
        doSetTrait d acc (x:xs) = case f x of
            Just rv -> doSetTrait rv acc xs
            Nothing -> doSetTrait d (acc ++ [x]) xs
    access ("is", "ro")   = Just AccessRO
    access ("is", "rw")   = Just AccessRW
    access ("is", "copy") = Just AccessCopy
    access _              = Nothing
    ref    ("is", "ref")  = Just True
    ref    _              = Nothing
    lazy   ("is", "lazy") = Just True
    lazy   _              = Nothing
    context   ("is", "context") = Just True
    context   _                 = Nothing
    failReqDef = fail "required parameters cannot have default values"

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
    return $ case mergeSimpleName nameExps of
        Just name -> Val (VType (mkType name)) -- _Var $ ":" ++ name
        _         -> Syn (":::()") nameExps
    where
    mergeSimpleName :: [Exp] -> Maybe String
    mergeSimpleName [] = Nothing
    mergeSimpleName [Val (VStr name)] = Just name
    mergeSimpleName (Val (VStr name):xs) = do
        rest <- mergeSimpleName xs
        Just $ name ++ "::" ++ rest
    mergeSimpleName _ = Nothing

s_postTerm :: RuleParser (Exp -> Exp)
s_postTerm = verbatimRule "term postfix" $ do
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
            App (Var var) (Just inv) [] | ('&':name) <- cast var ->
                Syn "{}" [inv, Val (VStr name)]
            e -> e

ruleInvocation :: RuleParser (Exp -> Exp)
ruleInvocation = ruleInvocationCommon False

-- used only by 'qInterpolatorPostTerm'?
ruleInvocationParens :: RuleParser (Exp -> Exp)
ruleInvocationParens = ruleInvocationCommon True

ruleNamedMethodCall :: RuleParser (Maybe Char, String)
ruleNamedMethodCall = do
    let quantifieableName   = simpleMethName <|> ruleVarName 
        simpleMethName      = fmap ('&':) (ruleOperatorName <|> ruleQualifiedIdentifier)
    choice
        [ fmap ((,) Nothing) quantifieableName                                  -- .meth
        , try (oneOf "*+?" >>= \q -> fmap ((,) (Just q)) quantifieableName )    -- .+meth
        , fmap ((,) Nothing) (parseExpWithCachedParser dynParsePrePost)         -- .+
        ]

ruleInvocationCommon :: Bool -> RuleParser (Exp -> Exp)
ruleInvocationCommon mustHaveParens = do
    (quant, name)   <- ruleNamedMethodCall
    (invs, args)    <- if mustHaveParens
        then do
            parseHasParenParamList                          -- .foo()
            <|> (lookAhead (ruleDot >> ruleInvocationParens) >> return (Nothing, [])) -- .foo.bar()
        else do  --  $obj.foo: arg1, arg2    # listop method call
                 -- we require whitespace after the colon (but not before)
                 -- so that @list.map:{...} doesn't get interpreted the
                 -- wrong way.
            listcolon <- option False $ try $ do { char ':'; mandatoryWhiteSpace; return True }
            if listcolon
                then parseNoParenParamList
                else option (Nothing,[]) $ parseParenParamList
    when (isJust invs) $ fail "Only one invocant allowed"
    return $ \x -> case name of
        ('&':_) -> App (_Var name) (Just x) args                                        -- $x.meth
        _       -> Syn "CCallDyn" (Val (castV (maybeToList quant)):_Var name:x:args)    -- $x.$meth

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
    return $ case exp of
        Syn "," []  -> id   -- %x<> means the same as %x{}
        _           -> \x -> Syn "{}" [x, exp]

ruleCodeSubscript :: RuleParser (Exp -> Exp)
ruleCodeSubscript = tryVerbatimRule "code subscript" $ do
    (invs, args) <- parseHasParenParamList
    return $ \x -> App x invs args

{-|
Match a sub application, returning the appropriate 'App' expression.

Note that this only handles regular sub application (@foo(\$bar)@) and
implicit-invocant calls (@.foo@); regular method invocation (@\$obj.foo@) is
handled by 'ruleInvocation' as a post-term ('s_postTerm').

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
    lookAhead (char '.')
    prevChar <- gets s_char
    fs <- many s_postTerm
    when (prevChar == '}') $ do
        pos <- getPosition
        traceM ("Warning: '{...}.method' treated as '{...}; .method' at " ++ show pos)
    return (combine (reverse fs) (_Var "$_"))

ruleSubNameWithoutPostfixModifier :: RuleParser String
ruleSubNameWithoutPostfixModifier = try $ do
    name <- ruleSubName
    case name of
        "&if"       -> fail "postfix op"
        "&unless"   -> fail "postfix op"
        "&while"    -> fail "postfix op"
        "&until"    -> fail "postfix op"
        "&given"    -> fail "postfix op"
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
    possiblyApplyMacro $ App (_Var name) paramListInv args
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
        (App (Var var) Nothing [key, val])
            | var == cast "&infix:=>" -> return (Syn "named" [key, val])
        _                             -> fail "internal error--was expecting a pair"

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

The only difference with parseParamList is that NoParens has to be careful not
to swallow `{}.blah`.

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

-- XXX - Eventually return "Var" here and verbatimVarNameString can be the "String" form
ruleVarName :: RuleParser String
ruleVarName = lexeme verbatimVarNameString

verbatimVarNameString :: RuleParser String
verbatimVarNameString = (<?> "variable name") $ choice
    [ try (string "$/")     -- match object
    , try ruleMatchPos
    , try ruleMatchNamed
    , try regularVarName
    , string "$!"           -- error variable
    ]

ruleSigil :: RuleParser VarSigil
ruleSigil = fmap cast (oneOf "$@%&")
    
regularVarName :: RuleParser String
regularVarName = do
    sigil   <- ruleSigil
    regularVarNameForSigil sigil

regularVarWithOptionalName :: RuleParser String
regularVarWithOptionalName = do
    sigil   <- ruleSigil
    option (show sigil) (regularVarNameForSigil sigil)

-- Twigil: ^ placeholder, * global, ? magical, . member, ! private member
-- XXX - ruleQualifiedIdentifier doesn't handle names /beginning/ with "::"
regularVarNameForSigil :: VarSigil -> RuleParser String
regularVarNameForSigil SCode = ruleSubNamePossiblyWithTwigil
regularVarNameForSigil sigil = do
    twi <- ruleTwigil
    idt <- ruleQualifiedIdentifier
    return $ show sigil ++ twi ++ idt

ruleDereference :: RuleParser Exp
ruleDereference = try $ do
    sigil   <- oneOf "$@%&"
    exp     <- ruleDereference <|> ruleSigiledVar <|> verbatimParens ruleExpression
    return $ Syn (sigil:"{}") [exp]

ruleSigiledVar :: RuleParser Exp
ruleSigiledVar = (<|> ruleSymbolicDeref) . try $ do
    name <- verbatimVarNameString
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
            let outerLexPad     = envLexical (fromJust outerEnv)
                outerVisible    = isJust (lookupPad (cast name) outerLexPad)
                curPads         = Map.elems (s_blockPads state)
                curVisible      = any (Map.member (cast name) . padEntries) curPads
                outerEnv        = envOuter (s_env state)
                inTopLevel      = case outerEnv of
                    Just env -> isNothing (envOuter env)
                    _        -> True
            -- If it's visible in the outer lexical scope, yet not
            -- defined in the current scope, we remember that fact.
            when (not inTopLevel && outerVisible && not curVisible) $
                addOuterVar (cast name)
            return (makeVar name)

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
        (verbatimParens ruleExpression) <|> (fmap (Val . VStr) $ do
            choice
                [ string "!"  --  $!
                , string "/"  --  $/
                , fmap concat $ sequence [ruleTwigil, many1 wordAny] ])
    return $ Syn (sigil:"::()") nameExps

