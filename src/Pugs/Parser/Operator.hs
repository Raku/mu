{-# OPTIONS_GHC -cpp -fglasgow-exts -funbox-strict-fields -fno-full-laziness -fno-cse -fallow-overlapping-instances -fno-warn-orphans #-}

module Pugs.Parser.Operator where
import Pugs.Internals
import Pugs.AST
import Pugs.Types
import Pugs.Lexer
import Pugs.Rule
import qualified Data.Set as Set
import qualified Data.HashTable as Hash

import Pugs.Parser.Types
import Pugs.Parser.Unsafe

listCons :: [RuleOperator Exp]
listCons = listSyn [","]                             -- List constructor

listInfix :: [RuleOperator Exp]
listInfix = listOps ["Y", "\xA5", "==>", "<=="]       -- List infix

-- Not yet transcribed ------------------------------------------------

tightOperators :: (?parseExpWithTightOps :: RuleParser Exp) =>
    RuleParser (Set ID, RuleOperatorTable Exp)
tightOperators = do
  tights <- currentTightFunctions
  return $ (,) (r_term tights)
    [ methOps   (words " . .+ .? .* .+ .() .[] .{} .<<>> .= ")  -- Method postfix
    , postOps   (words " ++ -- ")
      ++ preOps (words " ++ -- ")                               -- Auto-Increment
    , rightOps  (words " ** ")                                  -- Exponentiation
    , optPreSyn ["*"]                                           -- Symbolic Unary
      ++ preOps (words " = ! + - ~ ? +^ ~^ ?^ \\ ^")
      ++ preSymOps (fromSet $ r_pre tights)
      ++ postOps (fromSet $ r_post tights)
    , leftOps   (words " * / % x xx +& +< +> ~& ~< ~> ")        -- Multiplicative
    , leftOps   (words " + - ~ +| +^ ~| ~^ ?| ")                -- Additive
      ++ leftOps (fromSet $ r_infix tights)                      -- User defined ops
    , listOps   ["&"]                                           -- Junctive And
    , listOps   (words " ^ | ")                                 -- Junctive Or
    , optOps (fromSet $ r_opt tights)                            -- Named Unary
      ++ preOps (filter (\x -> (x /= "true") && (x /= "not")) (fromSet $ r_named tights))
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

fromSet :: Set ID -> [String]
fromSet = cast . Set.toList

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

data CurrentFunction = MkCurrentFunction
    { f_var     :: !Var
    , f_assoc   :: !SubAssoc
    , f_params  :: !Params
    }

-- read just the current state (ie, not a parser)
-- {-# NOINLINE currentFunctions #-}
currentFunctions :: RuleParser [CurrentFunction]
currentFunctions = do
    env <- getRuleEnv
    return $! inlinePerformSTM $! do
        glob <- readTVar $ envGlobal env
        let syms  = padToList glob ++ padToList (envLexical env)
            pkg   = envPackage env
            funs  = [ map (\(_, tvar) -> filterFun var tvar) tvars
                    | (var@MkVar{ v_sigil = SCode }, tvars) <- syms
                    , inScope pkg var
                    ] 
        fmap catMaybes (sequence (concat funs))

{-# NOINLINE _RefToFunction #-}
_RefToFunction :: Hash.HashTable Int (Maybe CurrentFunction)
_RefToFunction = unsafePerformIO (Hash.new (==) Hash.hashInt)

filterFun :: Var -> TVar VRef -> STM (Maybe CurrentFunction)
filterFun var tvar = do
    let key = unsafeCoerce# tvar
    res <- unsafeIOToSTM (Hash.lookup _RefToFunction key)
    case res of
        Just rv -> return rv
        Nothing -> do
            ref <- readTVar tvar
            let rv = case ref of
                    MkRef (ICode cv)
                        | relevantToParsing (code_type cv) (code_assoc cv)
                        -> Just (MkCurrentFunction var (code_assoc cv) (code_params cv))
                    MkRef (IScalar sv)
                        | Just (VCode cv) <- scalar_const sv
                        , relevantToParsing (code_type cv) (code_assoc cv)
                        -> Just (MkCurrentFunction var (code_assoc cv) (code_params cv))
                    _ -> Nothing
            unsafeIOToSTM (Hash.insert _RefToFunction key rv)
            return rv

inScope :: Pkg -> Var -> Bool
inScope pkg var
    | isGlobalVar var           = True
    | not (isQualifiedVar var)  = True
    | pkg == varPkg             = True
    | listPkg == varPkg         = True -- XXX wrong - special case for List::*
    | otherwise                 = False
    where
    varPkg = v_package var

relevantToParsing :: SubType -> SubAssoc -> Bool
relevantToParsing SubMethod  _      = False
relevantToParsing SubPrim    ANil   = True
relevantToParsing SubPrim    _      = False
relevantToParsing _          ANil   = False
relevantToParsing _          _      = True

-- XXX Very bad hacky kluge just for Parser.Operator
--     Switch to macro export for push(@x, 1) instead!
listPkg :: Pkg
listPkg = cast (mkType "List")

-- read just the current state
currentTightFunctions :: RuleParser TightFunctions
currentTightFunctions = do
    funs    <- currentFunctions
    let finalResult = foldr splitUnary initResult unary
        initResult  = MkTightFunctions emptySet emptySet emptySet emptySet terms infixOps
        (unary, notUnary)   = partition matchUnary funs
        slurpyNames         = namesFrom (filter matchSlurpy notUnary)
        (maybeTerm, notTerm)= partition matchTerm funs
        terms               = namesFrom maybeTerm Set.\\ namesFrom notTerm
        infixOps            = Set.fromList
            [ name
            | MkCurrentFunction { f_var = MkVar { v_categ = C_infix, v_name = name } } <- notUnary
            , name /= commaID
            ]
        splitUnary :: CurrentFunction -> TightFunctions -> TightFunctions
        splitUnary (MkCurrentFunction MkVar{ v_categ = cat, v_name = n } _ [param])
            res@MkTightFunctions
                { r_opt = opt, r_named = named, r_pre = pre, r_post = post }
                | n `Set.member` slurpyNames    = res
                | isOptional param              = res{ r_opt    = Set.insert n opt }
                | C_prefix <- cat               = res{ r_pre    = Set.insert n pre }
                | C_postfix <- cat              = res{ r_post   = Set.insert n post }
                | otherwise                     = res{ r_named  = Set.insert n named }
        splitUnary _ res = res
    return finalResult

namesFrom :: [CurrentFunction] -> Set ID
namesFrom = Set.fromList . map (v_name . f_var)

commaID :: ID
commaID = cast ","

data TightFunctions = MkTightFunctions
    { r_opt   :: !(Set ID)
    , r_named :: !(Set ID)
    , r_pre   :: !(Set ID)
    , r_post  :: !(Set ID)
    , r_term  :: !(Set ID)
    , r_infix :: !(Set ID)
    }

emptySet :: Set ID
emptySet = Set.empty

matchUnary :: CurrentFunction -> Bool
matchUnary MkCurrentFunction
    { f_assoc = ANil, f_params = [MkParam
        { paramContext = CxtItem{}, isNamed = False }] } = True
matchUnary _ = False

matchTerm :: CurrentFunction -> Bool
matchTerm MkCurrentFunction
    { f_assoc = ANil, f_params = [] } = True
matchTerm _ = False

matchSlurpy :: CurrentFunction -> Bool
matchSlurpy MkCurrentFunction
    { f_params = (_:_:_) } = True
matchSlurpy MkCurrentFunction
    { f_params = [MkParam
        { paramContext = CxtSlurpy{}, paramName = MkVar{ v_sigil = sig } }] }
            = sig == SArray || sig == SArrayMulti
matchSlurpy _ = False

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

{-# INLINE ops #-}
{-# SPECIALISE ops :: (String -> RuleOperator Exp) -> [String] -> [RuleOperator Exp] #-}
{-# SPECIALISE ops :: (String -> RuleParser String) -> [String] -> [RuleParser String] #-}
-- 0x10FFFF is the max number "chr" can take, so we use it for longest-token sorting.
-- buildExpressionParser will then use that information to make a longest-token match.
ops :: (String -> a) -> [String] -> [a]
ops f = map (f . tail) . sort . map (\x -> (chr (0x10FFFF - length x):x))

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




emptyTerm :: Exp
emptyTerm = Syn "" []

type UnaryOperator      = RuleParser (Exp -> Exp)
type BinaryOperator     = RuleParser (Exp -> Exp -> Exp)
type ListOperator       = RuleParser ([Exp] -> Exp)
type DependentOperator  = Exp -> RuleParser Exp

data OpRow = MkOpRow
    { o_rassoc          :: ![BinaryOperator]
    , o_lassoc          :: ![BinaryOperator]
    , o_nassoc          :: ![BinaryOperator]
    , o_prefix          :: ![UnaryOperator]
    , o_postfix         :: ![UnaryOperator]
    , o_optPrefix       :: ![UnaryOperator]
    , o_listAssoc       :: ![ListOperator]
    , o_depPostfix      :: ![DependentOperator]
    }

-----------------------------------------------------------
-- Convert an OperatorTable and basic term parser into
-- a full fledged expression parser
-----------------------------------------------------------
buildExpressionParser :: RuleOperatorTable Exp -> RuleParser Exp -> RuleParser Exp
buildExpressionParser = flip (foldl makeParser)

{-# INLINE makeParser #-}
makeParser :: RuleParser Exp -> [RuleOperator Exp] -> RuleParser Exp
makeParser term ops = do
    x <- termP
    rassocP x <|> lassocP x <|> nassocP x <|> listAssocP x <|> return x <?> "operator"
    where
    MkOpRow rassoc lassoc nassoc prefix postfix optPrefix listAssoc depPostfix
        = foldr splitOp (MkOpRow [] [] [] [] [] [] [] []) ops
    rassocOp          = choice rassoc
    lassocOp          = choice lassoc
    nassocOp          = choice nassoc
    prefixOp          = choice prefix <?> ""
    postfixOp         = choice postfix <?> ""
    optPrefixOp       = choice optPrefix <?> ""
    listAssocOp       = choice listAssoc
    depPostfixOp x    = choice (map ($ x) depPostfix) <?> ""

    ambig assoc op    = try
        (op >> fail ("ambiguous use of a " ++ assoc ++ " associative operator"))
    ambigRight        = ambig "right" rassocOp
    ambigLeft         = ambig "left" lassocOp
    ambigNon          = ambig "non" nassocOp

    foldOp = foldr (.) id
    termP = do
        pres    <- many $ (fmap Left prefixOp) <|> (fmap Right optPrefixOp)
        -- Here we handle optional-prefix operators.
        x       <- if null pres then term else case last pres of
            Left _  -> term
            _       -> option emptyTerm term
        x'      <- depPostP x
        posts   <- many postfixOp
        return $ foldOp posts $ foldOp (map liftEither pres) x'

    liftEither (Left x) = x
    liftEither (Right x) = x
    depPostP x = (<|> return x) $ do
        x' <- depPostfixOp x 
        depPostP x'

    rassocP x  = (do
        f   <- rassocOp
        y   <- rassocP1 =<< termP
        return (f x y)) <|> ambigLeft <|> ambigNon

    rassocP1 x = rassocP x  <|> return x

    lassocP x  = (do
        f   <- lassocOp
        y   <- termP
        lassocP1 (f x y)) <|> ambigRight <|> ambigNon

    lassocP1 x = lassocP x <|> return x

    nassocP x  = do
        f <- nassocOp
        y <- termP
        ambigRight <|> ambigLeft <|> ambigNon <|> return (f x y)

    listAssocP x  = do
        f   <- listAssocOp
        xs  <- option [] $ listAssocP1 =<< termP
        return (f (x:xs))

    listAssocP0 x  = do
        listAssocOp
        xs  <- option [] $ listAssocP1 =<< termP
        return (x:xs)
    listAssocP1 x = listAssocP0 x <|> return [x]

{-# INLINE splitOp #-}
splitOp :: RuleOperator Exp -> OpRow -> OpRow
splitOp col row@(MkOpRow rassoc lassoc nassoc prefix postfix optPrefix listAssoc depPostfix) = case col of
    Infix op AssocNone      -> row{ o_nassoc    = op:nassoc }
    Infix op AssocLeft      -> row{ o_lassoc    = op:lassoc }
    Infix op AssocRight     -> row{ o_rassoc    = op:rassoc }
    InfixList op AssocList  -> row{ o_listAssoc = op:listAssoc }
    Prefix op               -> row{ o_prefix    = op:prefix }
    Postfix op              -> row{ o_postfix   = op:postfix }
    OptionalPrefix op       -> row{ o_optPrefix = op:optPrefix }
    DependentPostfix op     -> row{ o_depPostfix= op:depPostfix }
    -- FIXME: add AssocChain
    _ -> internalError $ "Unhandled operator type" ++ show (op_assoc col)

