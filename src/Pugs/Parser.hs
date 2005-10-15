{-# OPTIONS_GHC -cpp -fglasgow-exts -funbox-strict-fields -fno-full-laziness -fno-cse #-}
{-# OPTIONS_GHC -#include "../UnicodeC.h" #-}

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
    mkPos,

    module Pugs.Lexer,
    module Pugs.Parser.Types,
    module Pugs.Parser.Unsafe,
) where
import Pugs.Internals
import Pugs.AST
import Pugs.Types
import Pugs.Version (versnum)
import Pugs.Lexer
import Pugs.Rule
import Pugs.Rule.Expr
import Pugs.Pretty
import qualified Data.Set as Set

import Pugs.Parser.Types
import Pugs.Parser.Number
import Pugs.Parser.Unsafe

fixities :: [String]
fixities = words $ " prefix: postfix: infix: circumfix: coerce: self: term: "
    ++ " postcircumfix: rule_modifier: trait_verb: trait_auxiliary: "
    ++ " scope_declarator: statement_control: infix_postfix_meta_operator: "
    ++ " postfix_prefix_meta_operator: prefix_postfix_meta_operator: "
    ++ " infix_circumfix_meta_operator: "

isOperatorName :: String -> Bool
isOperatorName ('&':name) = any hasOperatorPrefix [name, tail name]
    where
    hasOperatorPrefix :: String -> Bool
    hasOperatorPrefix name = any (`isPrefixOf` name) fixities
isOperatorName _ = False

-- Lexical units --------------------------------------------------

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

{-|
Record the current parser position, invoke the given subrule, then record the
parser's new position and encapsulate the subrule's result in a
'Pugs.AST.Internals.Pos' indicating the source region matched by the rule.

Also applies 'unwrap' to the result of the given parser.
-}
expRule :: RuleParser Exp -- ^ Sub-rule to invoke
        -> RuleParser Exp
expRule rule = do
    pos1 <- getPosition
    exp  <- rule
    pos2 <- getPosition
    return $ Pos (mkPos pos1 pos2) (unwrap exp)

{-|
Create a Pugs 'Pugs.AST.Pos' (for storing in the AST) from two Parsec
@SourcePos@ positions, being the start and end respectively of the current
region.
-}
mkPos :: SourcePos -- ^ Starting position of the region
      -> SourcePos -- ^ Ending position of the region
      -> Pos
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
    env     <- getRuleEnv
    pre     <- many ruleEmptyExp
    body    <- option emptyExp ruleStatementList
    post    <- many ruleEmptyExp
    let body' = foldl mergeStmts (foldl (flip mergeStmts) body pre) post
    env'    <- getRuleEnv
    putRuleEnv env'{ envLexical = envLexical env }
    return $ case unwrap body' of
        (Syn "sub" _)   -> mergeStmts emptyExp body'
        _               -> body'

{-|
Match an opening brace (@{@), followed by a 'ruleBlockBody', followed by a
closing brace (@}@) and nothing else (see 'ruleWhiteSpaceLine').

In other words, the closing brace must be the last meaningful character on
its line.
-}
ruleStandaloneBlock :: RuleParser Exp
ruleStandaloneBlock = tryRule "standalone block" $ do
    body <- bracesAlone ruleBlockBody
    retBlock SubBlock Nothing False body
    where
    bracesAlone p  = between (symbol "{") closingBrace p
    closingBrace = do
        char '}'
        ruleWhiteSpaceLine

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
ruleStatementList = rule "statements" $ choice
    [ ruleDocBlock
    , nonSep  ruleBlockDeclaration
    , semiSep ruleDeclaration
    , nonSep  ruleConstruct
    , semiSep ruleStatement
    ]
    where
    nonSep  = doSep many  -- must be followed by 0+ semicolons
    semiSep = doSep many1 -- must be followed by 1+ semicolons
    doSep sepCount rule = do
        whiteSpace
        -- pos     <- getPosition
        exp        <- rule
        appendRest <- option id $ do
            sepCount $ symbol ";"
            -- try to parse more statement-list, recursively
            rest <- option Noop ruleStatementList
            -- function to append recursive results to this iteration's results
            return $ (`mergeStmts` rest)
        return $ appendRest exp

{-|
Assert that we're at the beginning of a line, but consume no input (and produce
no result).

Used by 'ruleDocIntroducer', because POD-style regions must have their \'@=@\'
at the beginning of a line.
-}
ruleBeginOfLine :: RuleParser ()
ruleBeginOfLine = do
    pos <- getPosition
    when (sourceColumn pos /= 1) $ fail ""
    return ()

{-|
Match a single \'@=@\', but only if it occurs as the first character of a line.
-}
ruleDocIntroducer :: RuleParser Char
ruleDocIntroducer = (<?> "intro") $ do
    ruleBeginOfLine
    char '='

{-|
Match \'@=cut@\', followed by a newline (see 'ruleWhiteSpaceLine').

The \'@=@\' must be the first character of the line ('ruleDocIntroducer').
-}
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
    , rulePackageBlockDeclaration
    ]

ruleDeclaration :: RuleParser Exp
ruleDeclaration = rule "declaration" $ choice
    [ rulePackageDeclaration
    , ruleVarDeclaration
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

{-|
Try to match a colon character.  If one is found, return a function that will
take a variable name (including sigil) and insert a colon after the sigil.
If no colon is found, return @id@.

Used to match the private-method colon in 'ruleInvocation' and
'ruleInvocationParens'.
-}
maybeColon :: RuleParser ([Char] -> [Char])
maybeColon = option id $ do
    char ':'
    return $ \(sigil:name) -> (sigil:':':name)

-- | Scope, context, isMulti, styp, name
type SubDescription = (Scope, String, Bool, SubType, String)

ruleSubScopedWithContext :: RuleParser SubDescription
ruleSubScopedWithContext = rule "scoped subroutine with context" $ do
    scope   <- ruleScope
    cxt     <- identifier
    (isMulti, styp, name) <- ruleSubHead
    return (scope, cxt, isMulti, styp, name)

ruleSubScoped :: RuleParser SubDescription
ruleSubScoped = rule "scoped subroutine" $ do
    scope <- ruleScope
    (isMulti, styp, name) <- ruleSubHead
    return (scope, "Any", isMulti, styp, name)

ruleSubGlobal :: RuleParser SubDescription
ruleSubGlobal = rule "global subroutine" $ do
    (isMulti, styp, name) <- ruleSubHead
    return (SGlobal, "Any", isMulti, styp, name)

doExtract :: SubType -> Maybe [Param] -> Exp -> (Exp, [String], [Param])
doExtract SubBlock formal body = (fun, names', params)
    where
    (fun, names) = extract body []
    names' | isJust formal
           = filter (/= "$_") names
           | otherwise
           = names
    params = map nameToParam (sort names') ++ (maybe [] id formal)
doExtract SubPointy formal body = (body, [], maybe [] id formal)
doExtract SubMethod formal body = (body, [], maybe [] id formal)
doExtract _ formal body = (body, names', params)
    where
    (_, names) = extract body []
    names' | isJust formal
           = filter (/= "$_") names
           | otherwise
           = filter (== "$_") names
    params = map nameToParam (sort names') ++ (maybe [] id formal)

ruleRuleDeclaration :: RuleParser Exp
ruleRuleDeclaration = rule "rule declaration" $ try $ do
    symbol "rule"
    name    <- identifier
    adverbs <- ruleAdverbHash
    ch      <- char '{'
    expr    <- rxLiteralAny adverbs ch (balancedDelim ch)
    actual  <- possiblyApplyMacro $
                   App (Var ("&rx_")) Nothing
                       [adverbs, expr,
                        (Val $ VStr [ch]), (Val $ VStr [(balancedDelim ch)])]
    let exp = Syn ":=" [Var ('<':'*':name), actual]
    unsafeEvalExp (Sym SGlobal ('<':'*':name) exp)
    return emptyExp

rulePackageBlockDeclaration :: RuleParser Exp
rulePackageBlockDeclaration = rule "package block declaration" $ do
    (_, kind, pkgVal, env) <- try $ do
        rv <- rulePackageHead
        lookAhead (char '{')
        return rv
    body <- between (symbol "{") (char '}') ruleBlockBody
    env' <- getRuleEnv
    putRuleEnv env'{ envPackage = envPackage env }
    return $ Syn "namespace" [kind, pkgVal, body]

rulePackageDeclaration :: RuleParser Exp
rulePackageDeclaration = rule "package declaration" $ try $ do
    (_, kind, pkgVal, _) <- rulePackageHead
    return $ Syn "package" [kind, pkgVal]

rulePackageHead :: RuleParser (String, Exp, Exp, Env)
rulePackageHead = do
    sym <- choice $ map symbol (words "package module class role grammar")
    name    <- ruleQualifiedIdentifier
    _       <- option "" $ ruleVersionPart -- v
    _       <- option "" $ ruleAuthorPart  -- a
    whiteSpace
    traits  <- many $ ruleTrait
    let pkgClass = case sym of
                       "package" -> "Package"
                       "module"  -> "Module"
                       "class"   -> "Class"
                       "role"    -> "Class" -- XXX - Wrong - need metamodel
                       "grammar" -> "Grammar"
                       _ -> fail "bug"
    unsafeEvalExp (newPackage pkgClass name $ nub ("Object":traits))
    env <- getRuleEnv
    putRuleEnv env{ envPackage = name,
                    envClasses = envClasses env `addNode` mkType name }
    let pkgVal = Val . VStr $ name -- ++ v ++ a
        kind   = Val . VStr $ sym
    return (name, kind, pkgVal, env)

ruleSubDeclaration :: RuleParser Exp
ruleSubDeclaration = rule "subroutine declaration" $ do
    namePos <- getPosition
    (scope, typ, isMulti, styp, name) <- tryChoice
        [ ruleSubScopedWithContext
        , ruleSubScoped
        , ruleSubGlobal
        ]
    typ'    <- option typ $ try $ ruleBareTrait "returns"
    formal  <- option Nothing $ ruleSubParameters ParensMandatory
    typ''   <- option typ' $ try $ ruleBareTrait "returns"
    traits  <- many $ ruleTrait
    -- bodyPos <- getPosition
    body    <- ruleBlock
    let (fun, names, params) = doExtract styp formal body
    -- Check for placeholder vs formal parameters
    when (isJust formal && (not.null) names) $
        fail "Cannot mix placeholder variables with formal parameters"
    env <- getRuleEnv
    let subExp = Val . VCode $ MkCode
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
        mkExp n = Syn ":=" [Var n, Syn "sub" [subExp]]
        mkSym n = Sym scope (mkMulti n) (mkExp n)
        -- Horrible hack! Sym "&&" is the multi form.
        mkMulti | isMulti   = ('&':)
                | otherwise = id
        isGlobal = case name of
            (_:'*':_)   -> True
            _           -> False
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
    if "unsafe" `elem` traits && safeMode then return emptyExp else case scope of
        SGlobal | isExported -> do
            let caller  = maybe "main" envPackage (envCaller env)
                nameExported = (head name:caller) ++ "::" ++ tail name
            unsafeEvalExp $ mkSym nameExported
            unsafeEvalExp $ mkSym nameQualified
            return emptyExp
        SGlobal -> do
            unsafeEvalExp $ mkSym nameQualified
            return emptyExp
        _ -> do
            lexDiff <- unsafeEvalLexDiff $ mkSym nameQualified
            return $ Pad scope lexDiff $ mkExp name

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
    name    <- ruleOperatorName <|> ruleQualifiedIdentifier
    return $ "&" ++ twigil ++ name

ruleOperatorName :: RuleParser String
ruleOperatorName = do
    fixity <- choice (map (try . string) fixities)
    name <- identifier 
            <|> do sub <- ruleHashSubscript
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
        
maybeParensBool :: RuleParser a -> RuleParser (a, Bool)
maybeParensBool p = choice
    [ do rv <- parens p; return (rv, True)
    , do rv <- p; return (rv, False)
    ]

ruleFormalParam :: RuleParser Param
ruleFormalParam = rule "formal parameter" $ do
    typ     <- option "" $ ruleType
    sigil   <- option "" $ choice . map symbol $ words " ? * + ++ "
    name    <- ruleParamName -- XXX support *[...]
    traits  <- many ruleTrait
    let sigil' | not ("required" `elem` traits) = sigil
               | (sigil == "+") = "++"  -- promote "+$x is required" to "++$x"
               | (sigil == "?") = ""    -- well, heh, confused?
               | otherwise      = sigil -- required anyway
    let required = (sigil' /=) `all` ["?", "+"]
    exp     <- ruleParamDefault required
    optional $ do
        symbol "-->"
        ruleParamList ParensOptional $ tryChoice
            [ do { ruleFormalParam; return "" }
            , ruleType
            ]
    return $ foldr appTrait (buildParam typ sigil' name exp) traits
    where
    appTrait "rw"   x = x { isWritable = True }
    appTrait "copy" x = x { isLValue = False, isWritable = True }
    appTrait "lazy" x = x { isLazy = True }
    appTrait _      x = x -- error "unknown trait"

ruleParamDefault :: Bool -> RuleParser Exp
ruleParamDefault True  = return Noop
ruleParamDefault False = rule "default value" $ option Noop $ do
    symbol "="
    parseExpWithItemOps

ruleTrustsDeclaration :: RuleParser Exp
ruleTrustsDeclaration = do
    symbol "trusts"
    lexeme ruleQualifiedIdentifier
    return emptyExp

ruleTraitDeclaration :: RuleParser Exp
ruleTraitDeclaration = try $ do
    trait   <- ruleTrait
    env     <- getRuleEnv
    -- XXX horrible hack! "is eval(...), ..." should *not* be parsed as a trait
    -- declaration. So we check whether we're really statement-level, i.e.
    --   is eval(...) [eof]   # trait
    --   is eval(...);        # trait
    --   is eval(...) }       # trait
    --   is eval(...), ...    # sub call
    lookAhead $ try eof <|> (oneOf ";}" >> return ())
    let pkg = Var (':':envPackage env)
    return $ Syn "=" [Syn "{}" [pkg, Val (VStr "traits")], Syn "," [Syn "{}" [pkg, Val (VStr "traits")], Val (VStr trait)]]

ruleMemberDeclaration :: RuleParser Exp
ruleMemberDeclaration = do
    symbol "has"
    typ  <- option "" $ lexeme ruleQualifiedIdentifier
    attr <- ruleVarName
    case attr of
        (sigil:twigil:key) | twigil `elem` ".:" -> do
            traits  <- many $ ruleTrait
            optional $ do { symbol "handles"; ruleExpression }
            env     <- getRuleEnv
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
    typename    <- choice
        [ lexeme ruleQualifiedIdentifier
        , return ""
        ]  -- Type
    (decl, lhs) <- choice
        [ do -- pos  <- getPosition
             name <- ruleVarName
             return ((Sym scope name), Var name)
        , do names <- parens . (`sepEndBy` symbol ",") $
                ruleVarName <|> do { undefLiteral; return "" }
             let mkVar v = if null v then Val undef else Var v
             return (combine (map (Sym scope) names), Syn "," (map mkVar names))
        ]
    many ruleTrait -- XXX
    -- pos <- getPosition
    (sym, expMaybe) <- option ("=", Nothing) $ do
        sym <- tryChoice $ map string $ words " = .= := ::= "
        when (sym == "=") $ do
           lookAhead (satisfy (/= '='))
           return ()
        whiteSpace
        exp <- ruleExpression
        -- Slightly hacky. Here "my Foo $foo .= new(...)" is rewritten into
        -- "my Foo $foo = Foo.new(...)".
        -- And note that IIRC not the type object should be the invocant, but
        -- an undef which knows to dispatch .new to the real class.
        let exp' | Pos _ (App sub Nothing args) <- exp, sym == ".=" && typename /= ""
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
            env  <- getRuleEnv
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

{-|
Match a @no@ declaration, i.e. the opposite of @use@ (see
'ruleUseDeclaration').

Works by matching \'@no@\', then trying 'ruleNoVersion' and
@'ruleUsePackage' False@.
-}
ruleNoDeclaration :: RuleParser Exp
ruleNoDeclaration = rule "no declaration" $ do
    symbol "no"
    tryChoice [ ruleNoVersion >> return emptyExp
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
    tryChoice [ ruleUseVersion >> return emptyExp
              , ruleUsePackage True
              ]

rulePerlVersion :: RuleParser String
rulePerlVersion = rule "perl version" $ do
    option ' ' $ char 'v'
    many1 (choice [ digit, char '.' ])

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
    env <- getRuleEnv
    when use $ do   -- for &no, don't load code
        val <- unsafeEvalExp $
            if lang == "perl5"
                then Stmts (Sym SGlobal (':':'*':pkg) (Syn ":=" [ Var (':':'*':pkg), App (Var "&require_perl5") Nothing [Val $ VStr pkg] ])) (Syn "env" [])
                else App (Var "&use") Nothing [Val . VStr $ concat (intersperse "/" names) ++ ".pm"]
        case val of
            Val (VControl (ControlEnv env')) -> putRuleEnv env
                { envClasses = envClasses env' `addNode` mkType pkg
                , envGlobal  = envGlobal env'
                }
            _  -> error $ pretty val
    try (verbatimParens whiteSpace) <|> do
        imp <- option emptyExp ruleExpression
        let sub = Var $ ('&':pkg) ++ if use then "::import" else "::unimport"
        unsafeEvalExp $ Syn "if"
            [ App (Var "&name") (Just sub) [] -- XXX Hack
            , App sub (Just $ Val $ VStr $ pkg) [imp]
            , emptyExp
            ]
        return ()
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
    (names, _, _) <- tryChoice
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
ruleInlineDeclaration = tryRule "inline declaration" $ do
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
ruleRequireDeclaration = tryRule "require declaration" $ do
    symbol "require"
    (names, _, _) <- rulePackageFullName
    return $ App (Var "&require") Nothing [Val . VStr $ concat (intersperse "/" names) ++ ".pm"]

ruleDoBlock :: RuleParser Exp
ruleDoBlock = rule "do block" $ try $ do
    symbol "do"
    choice
        -- do { STMTS }
        [ try $ between (symbol "{") (char '}') ruleBlockBody
        -- do STMTS
        , ruleBlockDeclaration
        , ruleDeclaration
        , ruleConstruct
        , ruleStatement
        ]

ruleClosureTrait :: Bool -> RuleParser Exp
ruleClosureTrait rhs = rule "closure trait" $ do
    let names | rhs       = " BEGIN CHECK INIT FIRST "
              | otherwise = " BEGIN CHECK INIT FIRST END "
    name    <- tryChoice $ map symbol $ words names
    block   <- ruleBlock
    let (fun, names) = extract block []
    -- Check for placeholder vs formal parameters
    when (not $ null names) $
        fail "Closure traits take no formal parameters"
    let code = VCode mkSub { subName = name, subBody = fun } 
    case name of
        "END"   -> do
            -- We unshift END blocks to @*END at compile-time.
            -- They're then run at the end of runtime or at the end of the
            -- whole program.
            unsafeEvalExp $ App (Var "&unshift") (Just $ Var "@*END") [Syn "sub" [Val code]]
            return Noop
        "BEGIN" -> do
            -- We've to exit if the user has written code like BEGIN { exit }.
            val <- possiblyExit =<< unsafeEvalExp (checkForIOLeak fun)
            -- And install any pragmas they've requested.
            env <- getRuleEnv
            let idat = unsafePerformIO $ liftSTM $ readTVar $ envInitDat env
            install $ initPragmas idat
            return val
        "CHECK" -> vcode2checkBlock code
        "INIT"  -> vcode2initBlock code
        "FIRST" -> vcode2firstBlock code
        _       -> fail ""
    where
        install [] = return $ ()
        install prag = do
            env' <- getRuleEnv
            let env'' = envCaller env'  -- not sure about this.
            case env'' of
                Just target -> do
                    putRuleEnv target { envPragmas = prag ++ envPragmas target }
                _ -> fail "no caller env to install pragma in"

{-| Wraps a call to @&Pugs::Internals::check_for_io_leak@ around the input
    expression. @&Pugs::Internals::check_for_io_leak@ should @die()@ if the
    expression returned an IO handle. -}
-- Please remember to edit Prelude.pm, too, if you rename the name of the
-- checker function.
checkForIOLeak :: Exp -> Exp
checkForIOLeak exp =
    App (Var "&Pugs::Internals::check_for_io_leak") Nothing
        [ Val $ VCode mkSub { subBody = exp } ]
    
-- | If we've executed code like @BEGIN { exit }@, we've to run all @\@*END@
--   blocks and then exit. Returns the input expression if there's no need to
--   exit.
{-# NOINLINE possiblyExit #-}
possiblyExit :: Exp -> RuleParser Exp
possiblyExit (Val (VControl (ControlExit exit))) = do
    -- Run all @*END blocks...
    unsafeEvalExp $ Syn "for"
        [ Var "@*END"
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
                Nothing
                [ Var "$?FIRST_RUN"
                , Stmts (App (Var "&postfix:++") Nothing [Var "$?FIRST_RUN"])
                        (Syn "=" [Var "$?FIRST_RESULT", App (Val code) Nothing []])
                ])   --  { $?FIRST_RUN++; $?FIRST_RESULT = { 42 }() };
            (Var "$?FIRST_RESULT") --  $?FIRST_RESULT;
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
    block <- ruleBlockLiteral <|> parseExpWithItemOps
    retSyn "for" [list, block]

ruleLoopConstruct :: RuleParser Exp
ruleLoopConstruct = rule "loop construct" $ do
    symbol "loop"
    tryChoice [ ruleSemiLoopConstruct, rulePostLoopConstruct ]

ruleSemiLoopConstruct :: RuleParser Exp
ruleSemiLoopConstruct = rule "for-like loop construct" $ do
    conds <- maybeParens $ try $ do
        a <- option emptyExp ruleExpression
        symbol ";"
        b <- option emptyExp ruleExpression
        symbol ";"
        c <- option emptyExp ruleExpression
        return [a,b,c]
    block <- ruleBlock
    retSyn "loop" (conds ++ [block])

rulePostLoopConstruct :: RuleParser Exp
rulePostLoopConstruct = rule "postfix loop construct" $ do
    block <- ruleBlock
    option (Syn "loop" [block]) $ do
        name <- choice [ symbol "while", symbol "until" ]
        cond <- ruleExpression
        retSyn ("post" ++ name) [cond, block]

ruleCondConstruct :: RuleParser Exp
ruleCondConstruct = rule "conditional construct" $ do
    csym <- choice [ symbol "if", symbol "unless" ]
    ruleCondBody $ csym

ruleCondBody :: String -> RuleParser Exp
ruleCondBody csym = rule "conditional expression" $ do
    cond <- ruleCondPart
    body <- ruleBlock
    bodyElse <- option emptyExp ruleElseConstruct
    retSyn csym [cond, body, bodyElse]

ruleCondPart :: RuleParser Exp
ruleCondPart = maybeParens $ do
    ruleTypeVar <|> ruleTypeLiteral <|> parseExpWithItemOps <|> ruleExpression

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
    retSyn sym [ cond, body ]

ruleGivenConstruct :: RuleParser Exp
ruleGivenConstruct = rule "given construct" $ do
    sym <- symbol "given"
    topic <- ruleCondPart
    body <- ruleBlock
    retSyn sym [ topic, body ]

ruleWhenConstruct :: RuleParser Exp
ruleWhenConstruct = rule "when construct" $ do
    sym <- symbol "when"
    match <- ruleCondPart
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
ruleExpression = (<?> "expression") $ parseExpWithOps

{-|
Match a statement's /conditional/ statement-modifier,
e.g. '@say \"hello\" if \$cheerful@' or '@die unless +\@arguments@'.

Returns a function that will take the statement proper, and enclose it in an
appropriate 'Pugs.AST.Internals.Syn' (either @\"if\"@ or @\"unless\"@).
-}
rulePostConditional :: RuleParser (Exp -> RuleParser Exp)
rulePostConditional = rule "postfix conditional" $ do
    cond <- tryChoice $ map symbol ["if", "unless"]
    exp <- ruleExpression
    return $ \body -> retSyn cond [exp, body, emptyExp]

{-|
Match a statement's /looping/ statement-modifier,
e.g. '@procrastinate while $bored@' or '@eat until $full@'.

Returns a function that will take the statement proper, and enclose it in an
appropriate 'Pugs.AST.Internals.Syn' (either @\"while\"@ or @\"until\"@).
-}
rulePostLoop :: RuleParser (Exp -> RuleParser Exp)
rulePostLoop = rule "postfix loop" $ do
    cond <- tryChoice $ map symbol ["while", "until"]
    exp <- ruleExpression
    return $ \body -> retSyn cond [exp, body]

{-|
Match a statement's /iterating/ statement-modifier,
e.g. '@say for 1..10@'.

Returns a function that will take the statement proper, and enclose it in
@'Pugs.AST.Internals.Syn' \"for\"@.
-}
rulePostIterate :: RuleParser (Exp -> RuleParser Exp)
rulePostIterate = rule "postfix iteration" $ do
    cond <- tryChoice $ map symbol ["for"]
    exp <- ruleExpression
    return $ \body -> do
        block <- retBlock SubBlock Nothing False body
        retSyn cond [exp, block]

ruleBlockLiteral :: RuleParser Exp
ruleBlockLiteral = rule "block construct" $ do
    (typ, formal, lvalue) <- option (SubBlock, Nothing, False) $ choice
        [ ruleBlockFormalPointy
        , ruleBlockFormalStandard
        ]
    body <- ruleBlock
    retBlock typ formal lvalue body

extractHash :: Exp -> Maybe Exp
extractHash (Syn "block" [exp]) = extractHash (unwrap exp)
extractHash exp@(App (Var "&pair") _ _) = Just exp
extractHash exp@(App (Var "&infix:=>") _ _) = Just exp
extractHash exp@(Syn "," (App (Var "&pair") _ _:_)) = Just exp
extractHash exp@(Syn "," (App (Var "&infix:=>") _ _:_)) = Just exp
extractHash _ = Nothing

retBlock :: SubType -> Maybe [Param] -> Bool -> Exp -> RuleParser Exp
retBlock SubBlock Nothing _ exp | Just hashExp <- extractHash (unwrap exp) = return $ Syn "\\{}" [hashExp]
retBlock typ formal lvalue body = retVerbatimBlock typ formal lvalue body

retVerbatimBlock :: SubType -> Maybe [Param] -> Bool -> Exp -> RuleParser Exp
retVerbatimBlock styp formal lvalue body = expRule $ do
    let (fun, names, params) = doExtract styp formal body
    -- Check for placeholder vs formal parameters
    when (isJust formal && (not.null) names) $
        fail "Cannot mix placeholder variables with formal parameters"
    env <- getRuleEnv
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

paramsFor :: SubType -> Maybe [Param] -> [Param] -> [Param]
paramsFor SubMethod formal params 
    | isNothing (find (("%_" ==) . paramName) params)
    = paramsFor SubRoutine formal params ++ [defaultHashParam]
paramsFor styp Nothing []       = defaultParamFor styp
paramsFor _ _ params            = params

defaultParamFor :: SubType -> [Param]
defaultParamFor SubBlock    = [defaultScalarParam]
defaultParamFor SubPointy   = []
defaultParamFor _           = [defaultArrayParam]

ruleBlockFormalStandard :: RuleParser (SubType, Maybe [Param], Bool)
ruleBlockFormalStandard = rule "standard block parameters" $ do
    styp <- choice
        [ do { try $ symbol "sub";   return SubRoutine }
        , do { try $ symbol "coro";  return SubCoroutine }
        , do {       symbol "macro"; return SubMacro }
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



















-- Not yet transcribed ------------------------------------------------


tightOperators :: RuleParser (RuleOperatorTable Exp)
tightOperators = do
  [_, optionary, namedUnary, preUnary, postUnary, infixOps] <- currentTightFunctions
  return $
    [ methOps  " . .+ .? .* .+ .() .[] .{} .<<>> .= "   -- Method postfix
    , postOps  " ++ -- " ++ preOps " ++ -- "            -- Auto-Increment
    , rightOps " ** "                                   -- Exponentiation
    , preSyn "*"                                        -- Symbolic Unary
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
      ++ noneOps " cmp <=> .. ^.. ..^ ^..^ "            -- Non-chaining Binary
      ++ postOps "..."                                  -- Infinite range
    , chainOps $
               " != == < <= > >= ~~ !~ " ++
               " eq ne lt le gt ge =:= eqv "            -- Chained Binary
    , leftOps  " && "                                   -- Tight And
    , leftOps  " || ^^ // "                             -- Tight Or
    , [ternOp "??" "!!" "if"]                           -- Ternary
    -- Assignment
    , rightOps " => "                                -- Pair constructor
      ++ rightSyn (
               " = := ::= " ++
               " ~= += -= *= /= %= x= Y= ¥= **= xx= ||= &&= //= ^^= " ++
               " +&= +|= +^= ~&= ~|= ~^= ?|= ?^= |= ^= &= ")
    ]

looseOperators :: RuleParser (RuleOperatorTable Exp)
looseOperators = do
    -- names <- currentListFunctions
    return $
        [ -- preOps names                               -- List Operator
          leftOps  " ==> "                              -- Pipe Forward
        , leftOps  " and "                              -- Loose And
        , leftOps  " or xor err "                       -- Loose Or
        ]

operators :: RuleParser (RuleOperatorTable Exp)
operators = do
    tight <- tightOperators
    loose <- looseOperators
    return $ concat $
        [ tight
        , [ listSyn  " , ", listOps " Y ¥ " ]           -- Comma
        , loose
    --  , [ listSyn  " ; " ]                            -- Terminator
        ]

-- not a parser!
litOperators :: RuleParser (RuleOperatorTable Exp)
litOperators = do
    tight <- tightOperators
    loose <- looseOperators
    return $ tight ++ loose

-- read just the current state (ie, not a parser)
{-# NOINLINE currentFunctions #-}
currentFunctions :: RuleParser [(Var, VStr, Params)]
currentFunctions = do
    env     <- getRuleEnv
    return . concat . unsafePerformSTM $ do
        glob <- readTVar $ envGlobal env
        let funs  = padToList glob ++ padToList (envLexical env)
            pkg   = envPackage env
        forM [ fun | fun@(('&':_), _) <- funs ] $ \(name, tvars) -> do
            case inScope pkg (dropWhile (not . isAlphaNum) $ name) of
                Nothing     -> return []
                Just name'  -> fmap catMaybes $ forM tvars $ \(_, tvar) -> do
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
    where
    inScope pkg name | Just (post, pre) <- breakOnGlue "::" (reverse name) =
        if pkg == reverse pre then Just (reverse post) else Nothing
    inScope _ name = Just name

-- read just the current state
currentTightFunctions :: RuleParser [String]
currentTightFunctions = do
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
    -- Hack: Filter out &infix:<,> (which are most Preludes for PIL -> *
    -- compilers required to define), because else basic function application
    -- (foo(1,2,3) will get parsed as foo(&infix:<,>(1,&infix:<,>(2,3))) (bad).
    return $ map (encodeUTF8 . unwords . filter (/= ",") . nub) $
        [nullary, optionary, namedUnary, preUnary, postUnary, infixOps]

-- was: parseOpWith
parseExpWithCachedParser :: (DynParsers -> RuleParser Exp) -> RuleParser Exp
parseExpWithCachedParser f = do
    state <- getState
    case ruleDynParsers state of
        MkDynParsersEmpty   -> refillCache state f
        p                   -> f p
    where
    refillCache state f = do
        ops         <- operators
        opsTight    <- tightOperators
        opsLit      <- litOperators
        let [parse, parseTight, parseLit] = map
                (expRule . (\o -> buildExpressionParser o parseTerm (Syn "" [])))
                [ops, opsTight, opsLit]
            opParsers = MkDynParsers parse parseTight parseLit
        setState state{ ruleDynParsers = opParsers }
        f opParsers

-- was: parseOp
parseExpWithOps :: RuleParser Exp
parseExpWithOps = parseExpWithCachedParser dynParseOp

-- was: parseTightOp
parseExpWithTightOps :: RuleParser Exp
parseExpWithTightOps = parseExpWithCachedParser dynParseTightOp

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

ops :: (String -> a) -> String -> [a]
ops f s = [f n | n <- sortBy revLength (nub . words $ decodeUTF8 s)]
    where
    revLength x y = compare (length y) (length x)

doApp :: String -> [Exp] -> Exp
doApp str args = App (Var str) Nothing args

doAppSym :: String -> [Exp] -> Exp
doAppSym name@(_:'p':'r':'e':'f':'i':'x':':':_) args = App (Var name) Nothing args
doAppSym (sigil:name) args = App (Var (sigil:("prefix:"++name))) Nothing args
doAppSym _ _ = error "doAppSym: bad name"

preSyn      :: String -> [RuleOperator Exp]
preSyn      = ops $ makeOp1 Prefix "" Syn
preOps      :: String -> [RuleOperator Exp]
preOps      = (ops $ makeOp1 Prefix "&prefix:" doApp) . addHyperPrefix
preSymOps   :: String -> [RuleOperator Exp]
preSymOps   = (ops $ makeOp1 Prefix "&prefix:" doAppSym) . addHyperPrefix
postOps     :: String -> [RuleOperator Exp]
postOps     = (ops $ makeOp1 Postfix "&postfix:" doApp) . addHyperPostfix
optOps      :: String -> [RuleOperator Exp]
optOps      = (ops $ makeOp1 OptionalPrefix "&prefix:" doApp) . addHyperPrefix
leftOps     :: String -> [RuleOperator Exp]
leftOps     = (ops $ makeOp2 AssocLeft "&infix:" doApp) . addHyperInfix
rightOps    :: String -> [RuleOperator Exp]
rightOps    = (ops $ makeOp2 AssocRight "&infix:" doApp) . addHyperInfix
noneOps     :: String -> [RuleOperator Exp]
noneOps     = ops $ makeOp2 AssocNone "&infix:" doApp
listOps     :: String -> [RuleOperator Exp]
listOps     = ops $ makeOp2 AssocLeft "&infix:" doApp
chainOps    :: String -> [RuleOperator Exp]
chainOps    = (ops $ makeOp2 AssocLeft "&infix:" doApp) . addHyperInfix
rightSyn    :: String -> [RuleOperator Exp]
rightSyn    = ops $ makeOp2 AssocRight "" Syn
noneSyn     :: String -> [RuleOperator Exp]
noneSyn     = ops $ makeOp2 AssocNone "" Syn
listSyn     :: String -> [RuleOperator Exp]
listSyn     = ops $ makeOp0 AssocList "" Syn

{-|
Take a list of infix-operator names (as a space-separated string), and return
a similar string also containing both Texas-style and French-style infixed
hyperized forms.

For example, the string @\"+ -\"@ would be transformed into
@\"+ >>+\<\< »+« - >>-\<\< »-«\"@.
-}
addHyperInfix :: String -> String
addHyperInfix = unwords . concatMap hyperForm . words
    where
    hyperForm op = [op, ">>" ++ op ++ "<<", "»" ++ op ++ "«"]

{-|
Similar to 'addHyperInfix', but for prefix ops.

For example, @\"++ --\"@ would become
@\"++ ++\<\< ++« -- --\<\< --«\"@.
-}
addHyperPrefix :: String -> String
addHyperPrefix = unwords . concatMap hyperForm . words
    where
    hyperForm op = [op, op ++ "<<", op ++ "«"]

{-|
Similar to 'addHyperInfix', but for postfix ops.

For example, @\"++ --\"@ would become
@\"++ >>++ »++ -- >>-- »--\"@.
-}
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
    when (isWordAny $ last name) $ try $ choice
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
           RuleOperator a
makeOp2 prec sigil con name = (`Infix` prec) $ do
    symbol name
    return $ \x y -> con (sigil ++ name) [x,y]

makeOp0 :: Assoc -> 
           String -> 
           (String -> [a] -> a) -> 
           String -> 
           RuleOperator a
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
        , verbatimParens ruleExpression
        ]
    -- rulePostTerm returns an (Exp -> Exp) that we apply to the original term
    fs <- many rulePostTerm
    return $ combine (reverse fs) term

ruleTypeVar :: RuleParser Exp
ruleTypeVar = rule "type" $ try $ do
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

ruleTypeLiteral :: RuleParser Exp
ruleTypeLiteral = rule "type" $ do
    env     <- getRuleEnv
    name    <- tryChoice [
        do { symbol n; notFollowedBy (alphaNum <|> char ':'); return n }
        | (MkType n) <- flatten (envClasses env) ]
    return $ Var (':':name)

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
ruleInvocation = ruleInvocationCommon False

-- used only by 'qInterpolatorPostTerm'?
ruleInvocationParens :: RuleParser (Exp -> Exp)
ruleInvocationParens = ruleInvocationCommon True
        
ruleInvocationCommon :: Bool -> RuleParser (Exp -> Exp)
ruleInvocationCommon mustHaveParens = do
    colonify    <- maybeColon
    hasEqual    <- option False $ do { char '='; whiteSpace; return True }
    name        <- do { str <- ruleSubName; return $ colonify str }
    (invs,args) <- if mustHaveParens
        then verbatimParens $ parseNoParenParamList
        -- then parseHasParenParamList -- FAILED PARSER PATCH
        else option (Nothing,[]) $ parseParenParamList
    when (isJust invs) $ fail "Only one invocant allowed"
    return $ \x -> if hasEqual
        then Syn "=" [x, App (Var name) (Just x) args]
        else App (Var name) (Just x) args

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
    -- FAILED PARSER PATCH
    --(invs, args) <- parseHasParenParamList -- XXX doesn't handle trailing 
    --                                       --       adverbs outside parens
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
ruleApply isFolded = tryVerbatimRule "apply" $ do
    {- (colon :: String -> String) is a function that will take a name
       like '&foo', and MIGHT convert it to '&:foo'.  This only happens if
       you're using the '.:foo' implicit-invocant syntax, otherwise 'colon' will
       just be 'id'.
    
       (inv :: Maybe Exp) might hold a 'Var' expression indicating the implicit
       invocant.  Of course, if you're not using implicit-invocant syntax, this
       will be 'Nothing'. -}
    (colonify, implicitInv) <- option (id, Nothing) $ do
        when isFolded $ fail "" -- can't have implicit-invocant for [+]
        char '.'                -- implicit-invocant forms all begin with '.'
        option (id, Just $ Var "$_") $ choice
            [ do { char '/'; return (id, (Just $ Var "$?SELF")) }
            , do char ':'
                 return ( \(sigil:name) -> (sigil:':':name)
                        , Just $ Var "$?SELF"
                        )
            ]

    name    <- if isFolded
        then ruleFoldOp
        else fmap colonify ruleSubName
                   
    when ((name ==) `any` words " &if &unless &while &until &for ") $
        fail "reserved word"
        
    -- True for `foo .($bar)`-style applications
    hasDot  <- option False $ try $ do { whiteSpace; char '.'; return True }
    (paramListInv, args) <- if hasDot
        then parseNoParenParamList
        else if isJust implicitInv
            then parseParenParamListMaybe -- if we have an implicit invocant,
                                          -- then we need to follow method-call
                                          -- syntax rules
            else parseParenParamList <|> do { whiteSpace; parseNoParenParamList }
    -- FAILED PARSER PATCH
    {-(paramListInv, args) <- tryChoice
        -- foo .()
        [ whiteSpace >> char '.' >> parseHasParenParamList
        , if isJust implicitInv
            -- .foo()
            then parseParenParamListMaybe
            else tryChoice
                -- foo $bar, $baz
                -- (turns out we really need whiteSpace, not skipMany1 space)
                [ skipMany1 space >> parseNoParenParamList
                -- foo($bar, $baz):goo
                , parseParenParamListMaybe
                ]
        ] -}
    inv     <- mergeMaybes implicitInv paramListInv
    possiblyApplyMacro $ App (Var name) inv args
    where
    mergeMaybes :: Monad m => Maybe a -> Maybe a -> m (Maybe a)
    mergeMaybes (Just _) (Just _) = fail "can't have more than one invocant"
    mergeMaybes x y               = return $ x `mplus` y -- like $x // $y in P6

ruleFoldOp :: RuleParser String
ruleFoldOp = verbatimRule "reduce metaoperator" $ do
    char '['
    [_, _, _, _, _, infixOps] <- currentTightFunctions
    name <- tryChoice $ ops string (addHyperInfix $ infixOps ++ defaultInfixOps)
    char ']'
    possiblyHyper <- many $ (char '\171' >> return "<<") <|> (string "<<")
    return $ "&prefix:[" ++ name ++ "]" ++ concat possiblyHyper
    where
    defaultInfixOps = concat
        [ " ** * / % x xx +& +< +> ~& ~< ~> "
        , " + - ~ +| +^ ~| ~^ ?| , "
        , " & ^ | "
        , " => "
        , " != == < <= > >= ~~ !~ "
        , " eq ne lt le gt ge =:= eqv "
        , " && "
        , " || ^^ // "
        , " and or xor err "
        , " .[] .{} "
        ]


-- used only by 'ruleCodeSubscript'!
parseParamList :: RuleParser (Maybe Exp, [Exp])
parseParamList = parseParenParamList <|> parseNoParenParamList

parseParenParamList :: RuleParser (Maybe Exp, [Exp])
parseParenParamList = parseParenParamListCommon True

parseParenParamListMaybe :: RuleParser (Maybe Exp, [Exp])
parseParenParamListMaybe = parseParenParamListCommon False
    
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
named parser = do
    result <- parser
    case unwrap result of
        (App (Var "&infix:=>") Nothing [key, val]) -> return (Syn "named" [key, val])
        _                                          -> fail "internal error--was expecting a pair"

namedArg :: RuleParser Exp
namedArg = named (pairLiteral <|> complexNamed)
    where
    -- complexNamed parses a pair literal which has a complex expression (as
    -- opposed to a simply identifier) as its LHS, e.g.
    -- "a" => 5 or @array => 5.
    -- XXX should this be merged with pairLiteral?
    complexNamed = do
        -- ("a" => 5), with the parens, is not a named arg.
        notFollowedBy $ char '('
        exp <- parseExpWithTightOps
        case unwrap exp of
            (App (Var "&infix:=>") Nothing [_, _]) -> return exp
            _ -> fail "internal error--was expecting a pair"

namedArgOr :: RuleParser Exp -> RuleParser Exp
namedArgOr other = try namedArg <|> other

namedAdverb :: RuleParser Exp
namedAdverb = named pairAdverb

pairOrBlockAdverb :: RuleParser Exp
pairOrBlockAdverb = tryChoice [ namedAdverb, blockAdverb ]

blockAdverb :: RuleParser Exp
blockAdverb = do
    char ':'
    ruleBlockLiteral

-- used only by 'parseParenParamListCommon'
parseHasParenParamList :: RuleParser (Maybe Exp, [Exp])
parseHasParenParamList = verbatimParens $ do
    -- formal :: [[Exp]]
    -- outer level of listness provided by `sepEndBy`
    -- the inner (`fix`ed) part returns [Exp]
    formal <- (`sepEndBy` symbol ":") $ fix $ \rec -> do
        rv <- option Nothing $ do
            fmap Just $ tryChoice
                [ do x <- pairOrBlockAdverb
                     lookAhead (satisfy (/= ','))   
                     return ([x], return "")
                , do x <- namedArgOr parseExpWithItemOps
                     a <- option [] $ try $ many pairOrBlockAdverb
                     return (x:a, symbol ",")
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
    arguments       <- parseExpWithItemOps `sepEndBy` (symbol ",")
    
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
    formal <- (<|> return []) $ do
        -- Autrijus says that 'dotForbidden' is needed so that 
        -- `foo {}.blah` gets parsed as `foo ({}.blah)`
        -- rather than `(foo {}).blah` or something else
        x <- formalSegment dotForbidden
        (<|> return [x]) $ do
            sep
            xs <- formalSegment dotAllowed `sepEndBy` sep
            return (x:xs)
    processFormals formal
    where
    sep = symbol ":"
    formalSegment :: (Char -> Bool) -> RuleParser [Exp]
    formalSegment dot = do
        rv <- option Nothing (fmap Just $ tryChoice (argBlockish dot))
        case rv of
            Nothing           -> return []
            Just (exp, trail) -> do
                rest <- option [] $ do { trail; formalSegment dot }
                return (exp ++ rest)
    argBlockish :: (Char -> Bool) -> [RuleParser ([Exp], RuleParser String)]
    argBlockish dot =
        [ argBlockWith ruleBlockLiteral dot
        , argBlockWith pairOrBlockAdverb dot
        , argVanilla
        ]
    argBlockWith :: Monad m => RuleParser a -> (Char -> Bool) -> RuleParser ([a], m String)
    argBlockWith rule pred = do
        x <- rule
        lookAhead $ satisfy pred
        return ([x], return "")
    dotAllowed = (/= '.')
    dotForbidden = (not . (`elem` ".,"))
    argVanilla :: RuleParser ([Exp], RuleParser String)
    argVanilla = do
        x <- namedArgOr parseExpWithTightOps
        a <- option [] $ try $ many pairOrBlockAdverb
        return (x:a, symbol ",")

processFormals :: Monad m => [[Exp]] -> m (Maybe Exp, [Exp])
processFormals formal = case formal of
    []      -> return (Nothing, [])
    [args]  -> return (Nothing, unwind args)
    [invs,args] | [inv] <- unwind invs -> return (Just inv, unwind args)
    _                   -> fail "Only one invocant allowed"
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
    , paramDefault  = Noop
    }

ruleParamName :: RuleParser String
ruleParamName = literalRule "parameter name" $ do
    -- Valid param names: $foo, @bar, &baz, %grtz, ::baka
    sigil   <- choice [ oneOf "$@%&" >>= return . (:""), string "::" ]
    if sigil == "&"
        then ruleSubName
        else do twigil <- ruleTwigil
                name   <- many1 wordAny
                return $ sigil ++ twigil ++ name

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
    -- doesn't handle names /beginning/ with "::"
    name    <- ruleQualifiedIdentifier
    return $ (sigil:twigil) ++ name

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
    possiblyApplyMacro $ App (Var ('&':name)) Nothing []

{-|
Match the literal @undef@, returning an expression representing the undefined
value.
-}
undefLiteral :: RuleParser Exp
undefLiteral = try $ do
    symbol "undef"
    return $ Val VUndef

numLiteral :: RuleParser Exp
numLiteral = do
    n <- naturalOrRat
    case n of
        Left  i -> return . Val $ VInt i
        Right d -> return . Val $ VRat d

emptyListLiteral :: RuleParser Exp
emptyListLiteral = tryRule "empty list" $ do
    verbatimParens whiteSpace
    return $ Syn "," []

emptyArrayLiteral :: RuleParser Exp
emptyArrayLiteral = tryRule "empty array" $ do
    verbatimBrackets whiteSpace
    return $ Syn "\\[]" [emptyExp]

arrayLiteral :: RuleParser Exp
arrayLiteral = try $ do
    item <- verbatimBrackets ruleExpression
    return $ Syn "\\[]" [item]

{-|
Match a pair literal -- either an arrow pair (@a => 'b'@), or an adverbial pair
(@:foo('bar')@).
-}
pairLiteral :: RuleParser Exp
pairLiteral = tryChoice [ pairArrow, pairAdverb ]

pairArrow :: RuleParser Exp
pairArrow = do
    key <- identifier
    symbol "=>"
    val <- parseExpWithTightOps
    return (Val (VStr key), val)
    return $ App (Var "&infix:=>") Nothing [Val (VStr key), val]

pairAdverb :: RuleParser Exp
pairAdverb = do
    string ":"
    key <- many1 wordAny
    val <- option (Val $ VInt 1) $ tryChoice [ valueDot, noValue, valueExp ]
    return $ App (Var "&infix:=>") Nothing [Val (VStr key), val]
    where
    valueDot = do
        skipMany1 (satisfy isSpace)
        symbol "."
        option (Val $ VInt 1) $ valueExp
    noValue = do
        skipMany1 (satisfy isSpace)
        return (Val $ VInt 1)
    valueExp = lexeme $ choice
        [ verbatimParens ruleExpression
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

qInterpolateMinimal :: Char -> RuleParser Exp
qInterpolateMinimal _protectedChar = do
    char '\\'
    c <- anyChar
    return (Val $ VStr ['\\',c])

qInterpolateQuoteConstruct :: RuleParser Exp
qInterpolateQuoteConstruct = try $ do
    string "\\"
    (qStart, qEnd, flags) <- qStructure
    expr <- interpolatingStringLiteral qStart qEnd (qInterpolator flags)
    return expr

qInterpolatorPostTerm :: RuleParser (Exp -> Exp)
qInterpolatorPostTerm = try $ do
    option ' ' $ char '.'
    choice
        [ try ruleInvocationParens
        , try ruleArraySubscript
        , try ruleHashSubscript
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
            QB_Minimal -> try $ qInterpolateMinimal $ qfProtectedChar flags
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
    (qStart, qEnd, flags) <- getQDelim
    if not (qfHereDoc flags) then
        qLiteral1 qStart qEnd flags
      else do -- XXX an ugly kludge providing crude heredocs
        endMarker <- (fmap string $ many1 wordAny)
        qEnd; ruleWhiteSpaceLine
        qLiteral1 (fail "never match") endMarker flags

qLiteral1 :: RuleParser String    -- Opening delimiter
             -> RuleParser String -- Closing delimiter
             -> QFlags
             -> RuleParser Exp
qLiteral1 qStart qEnd flags = do
    expr <- interpolatingStringLiteral qStart qEnd (qInterpolator flags)
    -- qEnd
    case qfSplitWords flags of
        -- expr ~~ rx:perl5:g/(\S+)/
        QS_Yes      -> doSplit expr
        QS_Protect  -> doSplit expr
        QS_No       -> return expr
    where
    -- words() regards \xa0 as (breaking) whitespace. But \xa0 is
    -- a nonbreaking ws char.
    doSplit (Cxt (CxtItem _) (Val (VStr str))) = return $ doSplitStr str
    doSplit expr = doSplitRx expr

    doSplitRx expr = do
      rxSplit <- possiblyApplyMacro $ App (Var ("&rx_")) Nothing
            [ Val $ VList
                [ castV (VStr "P5", VInt 1)
                , castV (VStr "g", VInt 1)
                , castV (VStr "stringify", VInt 1)
                ]
            , Val $ VStr "(\\S+)"
            , Val $ VStr "/"
            , Val $ VStr "/"
            ]
      return $ App (Var "&infix:~~") Nothing [expr, rxSplit]


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
        symbol "\xab"
        qLiteral1 (symbol "\xab") (string "\xbb") $ qFlags
            { qfSplitWords = QS_Yes, qfProtectedChar = '\xbb' }

-- Quoting delimitor and flags
-- qfProtectedChar is the character to be
--   protected by backslashes, if
--   qfInterpolateBackslash is Minimal or Single or All
data QS_Flag = QS_No | QS_Yes | QS_Protect deriving (Show, Eq, Ord, Typeable)
data QB_Flag = QB_No | QB_Minimal | QB_Single | QB_All deriving (Show, Eq, Ord, Typeable)

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
rxP5Flags = MkQFlags QS_No True True True True False QB_Minimal '/' True False False
-- | Default flags
rxP6Flags :: QFlags
rxP6Flags = MkQFlags QS_No False False False False False QB_Minimal '/' False False False

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
    rxP5Flags { qfProtectedChar = delimEnd }

rxLiteral6 :: Char -- ^ Opening delimiter
           -> Char -- ^ Closing delimiter
           -> RuleParser Exp
rxLiteral6 delimStart delimEnd = qLiteral1 (string [delimStart]) (string [delimEnd]) $
    rxP6Flags { qfProtectedChar = delimEnd }


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
    expr    <- rxLiteralAny adverbs ch endch
    ch      <- if ch == endch then return ch else do { whiteSpace ; anyChar }
    let endch = balancedDelim ch
    subst   <- qLiteral1 (string [ch]) (string [endch]) qqFlags { qfProtectedChar = endch }
    return $ Syn "subst" [expr, subst, adverbs]

rxLiteral :: RuleParser Exp
rxLiteral = try $ tryChoice 
    [ do sym     <- symbol "rx" <|> symbol "m" <|> do
             symbol "rule"
             lookAhead $ do { ruleAdverbHash; char '{' }
             return "rx"
         adverbs <- ruleAdverbHash
         ch      <- anyChar
         expr    <- rxLiteral6 ch (balancedDelim ch)
         possiblyApplyMacro $
             App (Var ("&" ++ sym ++ "_")) Nothing  -- "&rx_" or "&m_"
             [adverbs, expr,
              (Val $ VStr [ch]), (Val $ VStr [(balancedDelim ch)])]
    , do symbol "pugs_internals_rx"
         adverbs <- ruleAdverbHash
         ch      <- anyChar
         expr    <- rxLiteralAny adverbs ch (balancedDelim ch)
         return $ Syn "rx" [expr, adverbs]
    , do symbol "pugs_internals_m"
         adverbs <- ruleAdverbHash
         ch      <- anyChar
         expr    <- rxLiteralAny adverbs ch (balancedDelim ch)
         return $ Syn "match" [expr, adverbs]
    ]

rxLiteralBare :: RuleParser Exp
rxLiteralBare = try $ tryChoice 
    [ do ch      <- char '/'
         expr    <- rxLiteral6 ch (balancedDelim ch)
         possiblyApplyMacro $ App (Var "&rxbare_") Nothing [expr]
    , do symbol "pugs_internals_rxbare"
         ch      <- char '/'
         expr    <- rxLiteral6 ch (balancedDelim ch)
         return $ Syn "//" [expr, Val undef]
    ]

{-|
Match the given literal string (as a lexeme), returning the second argument in
a 'Pugs.AST.Internals.Val' expression.

Used by 'ruleLit' for @NaN@ and @Inf@.
-}
namedLiteral :: String -- Literal string to match
             -> Val    -- Value to return
             -> RuleParser Exp
namedLiteral n v = do { symbol n; return $ Val v }

{-|
Match one of the \'yada-yada-yada\' placeholder expressions (@...@, @???@ or
@!!!@), returning a call to @&fail@, @&warn@ or @&die@ respectively.
-}
yadaLiteral :: RuleParser Exp
yadaLiteral = expRule $ do
    sym  <- choice . map symbol $ words " ... ??? !!! "
    return $ App (Var $ doYada sym) Nothing [Val $ VStr (sym ++ " - not yet implemented")]
    where
    doYada "..." = "&fail_" -- XXX rename to fail() eventually
    doYada "???" = "&warn"
    doYada "!!!" = "&die"
    doYada _ = error "Bad yada symbol"

methOps             :: a -> [b]
methOps _ = []

ternOp :: String -> String -> String -> RuleOperator Exp
ternOp pre post syn = (`Infix` AssocRight) $ do
    symbol pre
    y <- parseExpWithTightOps
    symbol post
    return $ \x z -> Syn syn [x, y, z]

{-|
Simply a shorthand for @return \$ Syn sym args@.
-}
retSyn :: String -- ^ Type of 'Pugs.AST.Internals.Syn' to produce
       -> [Exp]  -- ^ List of subexpressions for the 'Pugs.AST.Internals.Syn'
       -> RuleParser Exp
retSyn sym args = do
    return $ Syn sym args
    
