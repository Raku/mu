{-# OPTIONS_GHC -cpp -fglasgow-exts -funbox-strict-fields -fno-full-laziness -fno-cse -fallow-overlapping-instances #-}

module Pugs.Parser.Operator where
import Pugs.Internals
import Pugs.AST
import Pugs.Types
import Pugs.Lexer
import Pugs.Rule
import Pugs.Rule.Expr
import qualified Data.Set as Set

import Pugs.Parser.Types
import Pugs.Parser.Unsafe

operators :: (?parseExpWithTightOps :: RuleParser Exp) =>
    RuleParser (RuleOperatorTable Exp)
operators = do
    tight <- tightOperators
    loose <- looseOperators
    return $ concat $
        [ tight
        , [ listSyn [","] ]                             -- List constructor
        , [ listOps ["Y", "\xA5", "==>", "<=="] ]       -- List infix
        , loose
    --  , [ listSyn  " ; " ]                            -- Terminator
        ]

-- Not yet transcribed ------------------------------------------------

tightOperators :: (?parseExpWithTightOps :: RuleParser Exp) =>
    RuleParser (RuleOperatorTable Exp)
tightOperators = do
  [_, optionary, namedUnary, preUnary, postUnary, infixOps] <- currentTightFunctions
  return $
    [ methOps   (words " . .+ .? .* .+ .() .[] .{} .<<>> .= ")  -- Method postfix
    , postOps   (words " ++ -- ")
      ++ preOps (words " ++ -- ")                               -- Auto-Increment
    , rightOps  (words " ** ")                                  -- Exponentiation
    , optPreSyn ["*"]                                           -- Symbolic Unary
      ++ preOps (words " = ! + - ~ ? +^ ~^ ?^ \\ ^")
      ++ preSymOps preUnary
      ++ postOps postUnary
    , leftOps   (words " * / % x xx +& +< +> ~& ~< ~> ")        -- Multiplicative
    , leftOps   (words " + - ~ +| +^ ~| ~^ ?| ")                -- Additive
      ++ leftOps (filter (/= ",") infixOps)                     -- User defined ops
    , listOps   ["&"]                                           -- Junctive And
    , listOps   (words " ^ | ")                                 -- Junctive Or
    , optOps optionary                                          -- Named Unary
      ++ preOps (filter (\x -> (x /= "true") && (x /= "not")) namedUnary)
      ++ optSymOps (map (\x -> ['-', x]) fileTestOperatorNames)
    , noneSyn   (words " but does ")                            -- Traits
      ++ noneOps (words " leg cmp <=> .. ^.. ..^ ^..^ till ^till till^ ")  -- Non-chaining Binary
      ++ postOps (words "...")                                  -- Infinite range
    , chainOps (words " != == < <= > >= ~~ eqv eq ne lt le gt ge =:= === ")
                                                                -- Chained Binary
    , leftOps  ["&&"]                                           -- Tight And
    , leftOps  (words " || ^^ // ")                             -- Tight Or
    , [ternOp "??" "!!" "if"]                                   -- Ternary
    -- Assignment
    , (rightOps ["=>"] ++) .                                    -- Pair constructor
      (DependentPostfix listAssignment :) .
      (DependentPostfix immediateBinding :) .
      (rightAssignSyn :) .
      (rightDotAssignSyn :) $
      rightSyn (words (
               " := ~= += -= *= /= %= x= Y= \xA5= **= xx= ||= &&= //= ^^= " ++
               " +<= +>= ~<= ~>= +&= +|= +^= ~&= ~|= ~^= ?|= ?^= |= ^= &= "))
    , preOps ["true", "not"]                                    -- Loose unary
    ]

listAssignment :: (?parseExpWithTightOps :: RuleParser Exp) => Exp -> RuleParser Exp
listAssignment x = do
    try $ do
        char '='
        guard (not (isScalarLValue x))
        notFollowedBy (oneOf "=>" <|> (char ':' >> char '='))
        whiteSpace
    y   <- ?parseExpWithTightOps
    rhs <- option y $ do
        -- If we see comma, then convert this to a Syn ",".
        ruleComma
        ys <- ?parseExpWithTightOps `sepEndBy` ruleComma
        return (Syn "," (y:ys))
    return (Syn "=" [forceParens x, rhs])
    where
    -- XXX - Special casing ($x) = 1,2,3 to ($x,) = 1,2,3
    forceParens exp@(Ann Parens inner)
        | Syn "," _ <- unwrap exp   = exp
        | otherwise                 = Ann Parens (Syn "," [inner])
    forceParens (Ann x inner)       = Ann x (forceParens inner)
    forceParens (Sym x y inner)     = Sym x y (forceParens inner)
    forceParens (Pad x y inner)     = Pad x y (forceParens inner)
    forceParens exp                 = exp

immediateBinding :: (?parseExpWithTightOps :: RuleParser Exp) => Exp -> RuleParser Exp
immediateBinding x = do
    symbol "::="
    y <- ?parseExpWithTightOps
    unsafeEvalExp (Syn ":=" [x, y])
    return x

looseOperators :: RuleParser (RuleOperatorTable Exp)
looseOperators = do
    -- names <- currentListFunctions
    return $
        [ -- preOps names                               -- List Operator
          leftOps  ["==>"]                              -- Pipe Forward
        , leftOps  ["and"]                              -- Loose And
        , leftOps  (words " or xor err ")                       -- Loose Or
        ]

-- not a parser!
litOperators :: (?parseExpWithTightOps :: RuleParser Exp) =>
    RuleParser (RuleOperatorTable Exp)
litOperators = do
    tight <- tightOperators
    loose <- looseOperators
    return $ tight ++ loose

-- read just the current state (ie, not a parser)
-- {-# NOINLINE currentFunctions #-}
currentFunctions :: RuleParser [(Var, VStr, Params)]
currentFunctions = do
    env     <- getRuleEnv
    return . concat . inlinePerformSTM $ do
        glob <- readTVar $ envGlobal env
        let funs  = padToList glob ++ padToList (envLexical env)
            pkg   = envPackage env
        forM [ fun | fun@(var, _) <- funs, v_sigil var == SCode ] $ \(var, tvars) -> do
            if not (inScope pkg var) then return [] else do
                fmap catMaybes $ forM tvars $ \(_, tvar) -> do
                    ref <- readTVar tvar
                    -- read from ref
                    return $ case ref of
                        MkRef (ICode cv)
                            | relevantToParsing (code_assoc cv) (code_type cv)
                            -> Just (var, code_assoc cv, code_params cv)
                        MkRef (IScalar sv)
                            | Just (VCode cv) <- scalar_const sv
                            , relevantToParsing (code_assoc cv) (code_type cv)
                            -> Just (var, code_assoc cv, code_params cv)
                        _ -> Nothing
    where
    inScope pkg var
        | isGlobalVar var           = True
        | not (isQualifiedVar var)  = True
        | pkg == varPkg             = True
        | listPkg == varPkg         = True -- XXX wrong - special case for List::*
        | otherwise                 = False
        where
        varPkg = v_package var
    relevantToParsing "pre" SubPrim      = True
    relevantToParsing _     SubPrim      = False
    relevantToParsing _     SubMethod    = False
    relevantToParsing ""    _            = False
    relevantToParsing _     _            = True

-- XXX Very bad hacky kluge just for Parser.Operator
--     Switch to macro export for push(@x, 1) instead!
listPkg :: Pkg
listPkg = cast (mkType "List")

-- read just the current state
currentTightFunctions :: RuleParser [[String]]
currentTightFunctions = do
    funs    <- currentFunctions
    let (unary, rest) = (`partition` funs) $ \x -> case x of
            (_, "pre", [MkParam{ paramContext = CxtItem{}, isNamed = False }]) -> True
            _ -> False
        (maybeNullary, notNullary) = (`partition` funs) $ \x -> case x of
            (_, "pre", []) -> True
            _ -> False
        rest' = (`filter` rest) $ \x -> case x of
            (_, _, (_:_:_)) -> True
            (_, _, [MkParam { paramContext = CxtSlurpy{}, paramName = MkVar{ v_sigil = sig }}])
                | sig == SArray || sig == SArrayMulti-> True
            _ -> False
        namesFrom = map (\(var, _, _) -> v_name var)
        restNames = Set.fromList $ namesFrom rest'
        notNullaryNames = Set.fromList $ namesFrom notNullary
        nullary = filter (not . (`Set.member` notNullaryNames)) $ namesFrom maybeNullary
        optionary = map v_name optionary'
        (optionary', unary') = mapPair (map snd) . partition fst . sort $
            [ (isOptional param, var) | (var, _, [param]) <- unary
            , not (v_name var `Set.member` restNames)
            ]
        (namedUnary, preUnary, postUnary) = foldr splitUnary ([],[],[]) unary'
        splitUnary MkVar{ v_categ = C_prefix, v_name = name } (n, pre, post)
            = (n, (name:pre), post)
        splitUnary MkVar{ v_categ = C_postfix, v_name = name } (n, pre, post)
            = (n, pre, (name:post))
        splitUnary var (n, pre, post)
            = ((v_name var:n), pre, post)
        -- Then we grep for the &infix:... ones.
        (infixs, _) = (`partition` rest) $ \x -> case x of
            (MkVar{ v_categ = C_infix }, _, _) -> True
            _  -> False
        infixOps = map (\(var, _, _) -> v_name var) infixs
        mapPair f (x, y) = (f x, f y)
    -- Finally, we return the names of the ops.
    -- But we've to s/^infix://, as we've to return (say) "+" instead of "infix:+".
    -- Hack: Filter out &infix:<,> (which are most Preludes for PIL -> *
    -- compilers required to define), because else basic function application
    -- (foo(1,2,3) will get parsed as foo(&infix:<,>(1,&infix:<,>(2,3))) (bad).
    return $ map (map (cast :: ID -> String) . nub)
        [nullary, optionary, namedUnary, preUnary, postUnary, infixOps]

fileTestOperatorNames :: String
fileTestOperatorNames = "rwxoRWXOezsfdlpSbctugkTBMAC"

preSyn      :: [String] -> [RuleOperator Exp]
preSyn      = ops $ makeOp1 Prefix "" Syn
optPreSyn   :: [String] -> [RuleOperator Exp]
optPreSyn   = ops $ makeOp1 OptionalPrefix "" Syn
preOps      :: [String] -> [RuleOperator Exp]
preOps      = (ops $ makeOp1 Prefix "&prefix:" doApp) . addHyperPrefix
preSymOps   :: [String] -> [RuleOperator Exp]
preSymOps   = (ops $ makeOp1 Prefix "&prefix:" doAppSym) . addHyperPrefix
optSymOps   :: [String] -> [RuleOperator Exp]
optSymOps   = (ops $ makeOp1 OptionalPrefix "&prefix:" doAppSym) . addHyperPrefix
postOps     :: [String] -> [RuleOperator Exp]
postOps     = (ops $ makeOp1 Postfix "&postfix:" doApp) . addHyperPostfix
optOps      :: [String] -> [RuleOperator Exp]
optOps      = (ops $ makeOp1 OptionalPrefix "&prefix:" doApp) . addHyperPrefix
leftOps     :: [String] -> [RuleOperator Exp]
leftOps     = (ops $ makeOp2 AssocLeft "&infix:" doApp) . addHyperInfix
rightOps    :: [String] -> [RuleOperator Exp]
rightOps    = (ops $ makeOp2 AssocRight "&infix:" doApp) . addHyperInfix
noneOps     :: [String] -> [RuleOperator Exp]
noneOps     = ops $ makeOp2 AssocNone "&infix:" doApp
listOps     :: [String] -> [RuleOperator Exp]
listOps     = ops $ makeOp2 AssocLeft "&infix:" doApp
chainOps    :: [String] -> [RuleOperator Exp]
chainOps    = (ops $ makeOp2 AssocLeft "&infix:" doApp) . addHyperInfix . withNegation
    where
    withNegation []             = []
    withNegation (x@('!':_):xs) = x:withNegation xs
    withNegation (x:xs)         = x:('!':x):withNegation xs
rightSyn    :: [String] -> [RuleOperator Exp]
rightSyn    = ops $ makeOp2 AssocRight "" Syn
noneSyn     :: [String] -> [RuleOperator Exp]
noneSyn     = ops $ makeOp2 AssocNone "" Syn
listSyn     :: [String] -> [RuleOperator Exp]
listSyn     = ops $ makeOp0 AssocList "" Syn
rightAssignSyn :: RuleOperator Exp
rightAssignSyn = makeOp2Assign AssocRight "" Syn
rightDotAssignSyn :: RuleOperator Exp
rightDotAssignSyn = makeOp2DotAssign AssocRight "" Syn

-- 0x10FFFF is the max number "chr" can take.
ops :: (String -> a) -> [String] -> [a]
ops f = map (f . tail) . sort . map (\x -> (chr (0x10FFFF - length x):x))


-- chainOps    = ops $ makeOpChained

makeOp1 :: (RuleParser (Exp -> Exp) -> RuleOperator Exp) -> 
        String -> 
        (String -> [Exp] -> Exp) -> 
        String -> 
        RuleOperator Exp
makeOp1 prec sigil con name = prec $ try $ do
    symbol name
    -- `int(3)+4` should not be parsed as `int((3)+4)`
    lookAheadLiterals
    where
    lookAheadLiterals
        | "-" <- name =
            -- Horrible, horrible kluge to make "-e" etc work across prec levels.
            (try parseFileTestOp >>= makeFileTestOp)
                <|> conOp fullName
        | isWordAny (last name) = choice autoquoters
        | otherwise = conOp fullName
    autoquoters = 
        [ char '(' >> unexpected "(" 
        , string "=>" >> unexpected "=>"
        , conOp fullName
        ]
    parseFileTestOp = do
        rv <- oneOf fileTestOperatorNames
        lookAhead (satisfy (not . isWordAny))
        whiteSpace
        return rv
    fullName
        | isAlpha (head name)
        , "&prefix:" <- sigil
        = ('&':name)
        | otherwise
        = sigil ++ name
    makeFileTestOp ch = conOp ("&prefix:-" ++ [ch])
    conOp name = return $ \x -> case x of
        Syn "" []   -> con name []
        _           -> con name [x]

-- Just for the "state $foo = 1" rewriting
makeOp2Assign :: Assoc -> String -> (String -> [Exp] -> Exp) -> RuleOperator Exp
makeOp2Assign prec _ con = (`Infix` prec) $ do
    symbol "="
    return $ \invExp argExp -> stateAssignHack (con "=" [invExp, argExp])

stateAssignHack :: Exp -> Exp
stateAssignHack exp@(Syn "=" [lhs, _]) | isStateAssign lhs = 
    let pad = unsafePerformSTM $! do
                state_first_run <- newTVar =<< (fmap scalarRef $! newTVar (VInt 0))
                state_fresh     <- newTVar False
                return $! mkPad [(cast "$?STATE_START_RUN", [(state_fresh, state_first_run)])] in
    Syn "block"
        [ Pad SState pad $!
            Syn "if"
                [ App (_Var "&postfix:++") Nothing [_Var "$?STATE_START_RUN"]
                , lhs
                , exp
                ]
        ]
    where
    isStateAssign (Ann (Decl SState) _) = True
    isStateAssign (Ann _ exp)           = isStateAssign exp
    isStateAssign _                     = False
stateAssignHack others = others

-- Just for the ".=" rewriting
makeOp2DotAssign :: Assoc -> String -> (String -> [Exp] -> Exp) -> RuleOperator Exp
makeOp2DotAssign prec _ con = (`Infix` prec) $ do
    symbol ".="
    insertIntoPosition '.' -- "$x .= foo" becomes "$x .= .foo"
    return $ \invExp argExp -> case argExp of
        App meth _ args -> stateAssignHack (con "=" [invExp, App meth (Just invExp) args])
        _               -> Val (VError (VStr "the right-hand-side of .= must be a function application") [])

makeOp2 :: Assoc -> 
           String -> 
           (String -> [Exp] -> Exp) -> 
           String -> 
           RuleOperator Exp
makeOp2 prec sigil con name = (`Infix` prec) $ do
    symbol name
    return $ \x y -> con (sigil ++ name) [x,y]

makeOp0 :: Assoc -> 
           String -> 
           (String -> [Exp] -> Exp) -> 
           String -> 
           RuleOperator Exp
makeOp0 prec sigil con name = (`InfixList` prec) $ do
    many1 $ do
        string name
        whiteSpace
    return . con $ sigil ++ name

doApp :: String -> [Exp] -> Exp
doApp str args = App (_Var str) Nothing args

{-|
Take a list of infix-operator names (as a space-separated string), and return
a similar string also containing both Texas-style and French-style infixed
hyperized forms.

For example, the string @\"+ -\"@ would be transformed into
@\"+ >>+\<\< »+« - >>-\<\< »-«\"@.
-}
addHyperInfix :: [String] -> [String]
addHyperInfix = concatMap hyperForm
    where
    hyperForm op = [op, ">>" ++ op ++ "<<", "\xBB" ++ op ++ "\xAB"]

{-|
Similar to 'addHyperInfix', but for prefix ops.

For example, @\"++ --\"@ would become
@\"++ ++\<\< ++« -- --\<\< --«\"@.
-}
addHyperPrefix :: [String] -> [String]
addHyperPrefix = concatMap hyperForm
    where
    hyperForm op = [op, op ++ "<<", op ++ "\xAB"]

{-|
Similar to 'addHyperInfix', but for postfix ops.

For example, @\"++ --\"@ would become
@\"++ >>++ »++ -- >>-- »--\"@.
-}
addHyperPostfix :: [String] -> [String]
addHyperPostfix = concatMap hyperForm
    where
    hyperForm op = [op, ">>" ++ op, "\xBB" ++ op]

methOps             :: a -> [b]
methOps _ = []

doAppSym :: String -> [Exp] -> Exp
doAppSym name@(_:'p':'r':'e':'f':'i':'x':':':_) args = App (_Var name) Nothing args
doAppSym (sigil:name) args = App (_Var (sigil:("prefix:"++name))) Nothing args
doAppSym _ _ = error "doAppSym: bad name"


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
    return $ Ann (Pos (mkPos pos1 pos2)) (unwrap exp)

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

ternOp :: (?parseExpWithTightOps :: RuleParser Exp) =>
    String -> String -> String -> RuleOperator Exp
ternOp pre post syn = (`Infix` AssocRight) $ do
    symbol pre
    y <- ?parseExpWithTightOps
    symbol post
    return $ \x z -> Syn syn [x, y, z]

