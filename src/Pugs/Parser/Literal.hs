{-# OPTIONS_GHC -fglasgow-exts -fallow-overlapping-instances #-}
module Pugs.Parser.Literal where

import Pugs.Internals
import Pugs.AST
import Pugs.Lexer
import Pugs.Rule
import Pugs.Types

import Pugs.Parser.Types
import Pugs.Parser.Operator
import Pugs.Parser.Util
import Pugs.Parser.Number
import {-# SOURCE #-} Pugs.Parser

ruleLit :: RuleParser Exp
ruleLit = do
    lvl <- gets s_bracketLevel
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
    modify $ \state -> state{ s_char = '0' }
    case n of
        Left  i -> return . Val $ VInt i
        Right d -> return . Val $ VRat d

arrayLiteral :: RuleParser Exp
arrayLiteral = try $ do
    item <- verbatimBrackets ruleBracketedExpression
    return $ Syn "\\[]" [item]

ruleBracketedExpression :: RuleParser Exp
ruleBracketedExpression = enterBracketLevel ParensBracket $
    ruleExpression <|> do { whiteSpace; return (Syn "," []) }

{-|
Match a pair literal -- either an arrow pair (@a => 'b'@), or an adverbial pair
(@:foo('bar')@).
-}
pairLiteral :: RuleParser Exp
pairLiteral = choice [ pairArrow, pairAdverb ]

pairArrow :: RuleParser Exp
pairArrow = do
    key <- identifier `tryFollowedBy` symbol "=>"
    val <- parseExpWithTightOps
    return (Val (VStr key), val)
    return $ App (_Var "&infix:=>") Nothing [Val (VStr key), val]

pairAdverb :: RuleParser Exp
pairAdverb = try $ do
    char ':'
    negatedPair <|> shortcutPair <|> regularPair
    where
    negatedPair = do
        char '!'
        key <- many1 wordAny
        return $ App (_Var "&infix:=>") Nothing [Val (VStr key), Val (VBool False)]
    shortcutPair = do
        var <- regularVarName
        let key = reverse (takeWhile isWordAny (reverse var))
        return $ App (_Var "&infix:=>") Nothing [Val (VStr key), _Var var]
    regularPair = do
        key <- many1 wordAny
        val <- option (Val $ VBool True) $ choice [ valueDot, noValue, valueExp ]
        return $ if (all isDigit key)
            then App (_Var "&Pugs::Internals::base") Nothing [Val (VStr key), val]
            else App (_Var "&infix:=>") Nothing [Val (VStr key), val]
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


{-|
Match one of the \'yada-yada-yada\' placeholder expressions (@...@, @???@ or
@!!!@), returning a call to @&fail@, @&warn@ or @&die@ respectively.
-}
yadaLiteral :: RuleParser Exp
yadaLiteral = expRule $ do
    sym  <- choice . map symbol $ words " ... ??? !!! "
    return $ App (_Var $ doYada sym) Nothing [Val $ VStr (sym ++ " - not yet implemented")]
    where
    doYada "..." = "&fail_" -- XXX rename to fail() eventually
    doYada "???" = "&warn"
    doYada "!!!" = "&die"
    doYada _ = error "Bad yada symbol"

{-|
Match the given literal string (as a lexeme), returning the second argument in
a 'Pugs.AST.Internals.Val' expression.

Used by 'ruleLit' for @NaN@ and @Inf@.
-}
namedLiteral :: String -- Literal string to match
             -> Val    -- Value to return
             -> RuleParser Exp
namedLiteral n v = do { symbol n; return $ Val v }

ruleCommaOrSemicolon :: RuleParser ()
ruleCommaOrSemicolon = do
    lexeme (oneOf ",;")
    return ()

ruleTwigil :: RuleParser String
ruleTwigil = option "" . choice . map string $ words " ^ * ? . ! + ; "

ruleMatchPos :: RuleParser String
ruleMatchPos = do
    sigil   <- oneOf "$@%"
    digits  <- many1 digit
    return $ (sigil:digits)

ruleMatchNamed :: RuleParser String
ruleMatchNamed = do
    sigil   <- oneOf "$@%"
    twigil  <- char '<'
    name    <- many (do { char '\\'; anyChar } <|> satisfy (/= '>'))
    char '>'
    return $ (sigil:twigil:name) ++ ">"

ruleDot :: RuleParser ()
ruleDot = verbatimRule "dot" $ do
    try (char '.' >> notFollowedBy (char '.')) <|> ruleLongDot
    optional $ oneOf "*+?"

ruleLongDot :: RuleParser ()
ruleLongDot = do
    try (char '\\' >> notFollowedBy (char '('))
    whiteSpace
    char '.'
    return ()

-- zero-width, non-consuming word boundary assertion (\b)
ruleWordBoundary :: RuleParser ()
ruleWordBoundary = do
    cls <- getPrevCharClass
    look $ if (cls == SpaceClass) then (/=) else (==)
    return ()
    where
    look op = lookAhead (satisfy (\c -> SpaceClass `op` charClassOf c))

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
        QS_No       -> return $ case qfExecute flags of
            True -> App (_Var "&Pugs::Internals::runShellCommand") Nothing [expr]
            _    -> expr
    where
    -- words() regards \xa0 as (breaking) whitespace. But \xa0 is
    -- a nonbreaking ws char.
    doSplit (Ann (Cxt CxtItem{}) (Val (VStr str))) = doSplitStr str
    doSplit expr = App (_Var "&infix:~~") Nothing [expr, rxSplit]
    rxSplit = Syn "rx" $
        [ Val $ VStr "(\\S+)"
        , Val $ VList
            [ castV (VStr "P5", VInt 1)
            , castV (VStr "g", VInt 1)
            , castV (VStr "stringify", VInt 1)
            ]
        ]


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
    , qfExecute                 :: !Bool
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
          useflag "n" _           = rawFlags
          useflag "none" _        = rawFlags
          useflag "q" _           = qFlags
          useflag "single" _      = qFlags
          useflag "double" _      = qqFlags
          useflag "qq" _          = qqFlags -- support qq//
          useflag "exec" _        = qqFlags { qfExecute = True }
          useflag "x" _           = qqFlags { qfExecute = True }

        -- in case of unknown flag, we simply abort the parse.
          useflag _ qf            = qf { qfFailed = True }


{- | XXX can be later defined to exclude alphanumerics, maybe also exclude
closing delims from being openers (disallow q]a]) -}
openingDelim :: RuleParser (Int, Char)
openingDelim = do
    ch  <- anyChar
    rep <- many (char ch)
    return (length rep + 1, ch)

qStructure :: RuleParser (RuleParser String, RuleParser String, QFlags)
qStructure = 
    do char 'q'
       flags <- do
           firstFlag <- option ' ' alphaNum
           allFlags  <- many oneFlag
           case firstFlag of
               'q' -> return ("qq":allFlags) -- Special case: qq() means q:qq()
               ' ' -> return allFlags
               _   -> return ([firstFlag]:allFlags)

       notFollowedBy alphaNum
       whiteSpace
       (rep, delim) <- openingDelim
       let qflags = getQFlags flags $ balancedDelim delim
       when (qfFailed qflags) $ fail ""
       return ( (string (replicate rep delim)), (string (replicate rep $ balancedDelim delim)), qflags)
    where
    oneFlag = do
        char ':'
        many alphaNum


getQDelim :: RuleParser (RuleParser String, RuleParser String, QFlags)
getQDelim = try qStructure
    <|> try (do
        string "<<"
        return (string "<<", string ">>",
            qqFlags { qfSplitWords = QS_Yes, qfProtectedChar = '>' }))
    <|> do
        delim <- oneOf "`\"'<\xab"
        case delim of
            '"'     -> return (string "\"",  string "\"",    qqFlags)
            '\''    -> return (string "'",   string "'",   qFlags)
            '<'     -> return (string "<",   string ">",    qFlags
                { qfSplitWords = QS_Yes, qfProtectedChar = '>' })
            '`'     -> return (string "`",   string "`",    qqFlags
                { qfExecute = True, qfProtectedChar = '`' })
            '\xab'  -> return (string "\xab", string "\xbb", qqFlags
                { qfSplitWords = QS_Protect, qfProtectedChar = '\xbb' })
            _       -> fail ""


-- | Default flags
qFlags    :: QFlags
qFlags    = MkQFlags QS_No False False False False False QB_Single '\'' False False False False
-- | Default flags
qqFlags   :: QFlags
qqFlags   = MkQFlags QS_No True True True True True QB_All '"' False False False False
-- | Default flags
rawFlags  :: QFlags
rawFlags  = MkQFlags QS_No False False False False False QB_No 'x' False False False False
-- | Default flags
rxP5Flags :: QFlags
rxP5Flags = MkQFlags QS_No True True True True False QB_Balanced '/' True False False False
-- | Default flags
rxP6Flags :: QFlags
rxP6Flags = MkQFlags QS_No False False False False False QB_Balanced '/' False False False False

-- Regexps

-- | A parser returning a regex, given a hashref of adverbs and a closing delimiter.
rxLiteralAny :: Exp -> Char -> Char -> RuleParser Exp
rxLiteralAny adverbs
    | Syn "\\{}" [Syn "," pairs] <- adverbs
    , not (null [
        True
        | (App (Var var) Nothing [Val (VStr name), _]) <- pairs
        , var == cast "&infix:=>"
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
    adverbs     <- ruleAdverbHash
    (rep, ch)   <- openingDelim
    let endch = balancedDelim ch
    -- XXX - probe for adverbs to determine p5 vs p6
    expr    <- rxLiteralAny adverbs ch endch
    ch      <- if ch == endch then return ch else do { whiteSpace ; anyChar }
    let endch = balancedDelim ch
    subst   <- qLiteral1 (string (replicate rep ch)) (string (replicate rep endch)) qqFlags { qfProtectedChar = endch }
    return $ Syn "subst" [expr, subst, adverbs]

ruleRegexDeclarator :: RuleParser (Exp -> Exp)
ruleRegexDeclarator = choice
    [ symbol "rule"     >> return (adv "ratchet" . adv "sigspace")
    , symbol "token"    >> return (adv "ratchet")
    , symbol "regex"    >> return id
    ]
    where
    adv x (Syn "\\{}" [Syn "," pairs]) = Syn "\\{}"
        [Syn "," (App (_Var "&infix:=>") Nothing [Val (VStr x), Val (VBool True)] : pairs)]
    adv _ _ = internalError "unexpected regex adverb specifier"

rxLiteral :: RuleParser Exp
rxLiteral = do
    (withAdvs, decl) <- choice
        [ symbol "rx" >> return (id, "rx")
        , symbol "m"  >> return (id, "match")
        , do advs <- ruleRegexDeclarator
             lookAhead (ruleAdverbHash >> char '{')
             return (advs, "rx")
        ]
    adverbs <- fmap withAdvs ruleAdverbHash
    ch      <- anyChar
    expr    <- rxLiteralAny adverbs ch (balancedDelim ch)
    return $ Syn decl [expr, adverbs]

rxLiteralBare :: RuleParser Exp
rxLiteralBare = do
    ch      <- char '/'
    expr    <- rxLiteral6 ch (balancedDelim ch)
    return $ Syn "//" [expr, Val undef]

