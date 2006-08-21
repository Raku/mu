{-# OPTIONS_GHC -cpp -fglasgow-exts -funbox-strict-fields -fno-full-laziness -fno-cse -fallow-overlapping-instances -fno-warn-orphans #-}

module Pugs.Parser.Operator where
import Pugs.Internals
import Pugs.AST
import Pugs.Types
import Pugs.Lexer
import Pugs.Rule
import {-# SOURCE #-} Pugs.Parser
import qualified Data.Set as Set
import qualified Data.ByteString.Char8 as Str
import qualified Data.HashTable as Hash
--import qualified Judy.IntMap as L
import qualified Judy.CollectionsM as C

import Pugs.Parser.Types
import Pugs.Parser.Unsafe

listCons :: [RuleOperator Exp]
listCons = listSyn (opWords ",")                         -- List constructor

listInfix :: [RuleOperator Exp]
listInfix = listOps (opWords "Y \xA5 ==> <==")  -- List infix

opWords :: String -> Set OpName
opWords xs = Set.fromList (map (MkOpName . cast) (words xs))

newtype OpName = MkOpName ID
    deriving (Show, Eq, Typeable, (:>:) String, (:>:) ByteString, (:<:) ByteString)

instance Ord OpName where
    compare (MkOpName (MkID a x)) (MkOpName (MkID b y))
        = compare (Str.length y) (Str.length x) `mappend` compare b a

-- Not yet transcribed into a full optable parser with dynamic precedence --

tightOperators :: RuleParser (Set OpName, RuleOperatorTable Exp)
tightOperators = do
  tights <- currentTightFunctions
  return $ (,) (r_term tights)
    ( termLevel                     -- Terms and circumfixes
    : methLevel                     -- Method postfix
    : incrLevel                     -- Auto-Increment
    : expoLevel                     -- Exponentiation
    : (preSymOps (r_pre tights)     -- Symbolic Unary (user-definable)
        ++ postOps (r_post tights)
        ++ symbLevel)
    : multLevel                     -- Multiplicative
    : (leftOps (r_infix tights)     -- Additive (user-definable)
        ++ addiLevel
      )
    : junaLevel                     -- Junctive And
    : junoLevel                     -- Junctive Or
    : (optOps (r_opt tights)        -- Named Unary (user-definable)
      ++ preOps (r_named tights Set.\\ opWords " true not ")
      ++ fileTestOps
      )
    : staticLevels
    )

termLevel, methLevel, incrLevel, expoLevel, symbLevel, multLevel, addiLevel, junaLevel, junoLevel :: [RuleOperator Exp]
termLevel = circumOps (Set.singleton (MkOpName (cast "\\( )")))
methLevel = methOps (opWords " . .+ .? .* .+ .() .[] .{} .<<>> .= ")
incrLevel = postOps (opWords " ++ -- ")
            ++ preOps (opWords " ++ -- ")                               
expoLevel = rightOps (opWords " ** ")
symbLevel = optPreSyn (opWords " * ") ++ preOps (opWords " = ! + - ~ ? +^ ~^ ?^ \\ ^")
multLevel = leftOps (opWords " * / % x xx +& +< +> ~& ~< ~> ?& ")
addiLevel = leftOps (opWords " + - ~ +| +^ ~| ~^ ?| ")
junaLevel = listOps (opWords " & ")
junoLevel = listOps (opWords " ^ | ")

-- The lower levels of immutable ops.  This will be replaced once we have
-- user-defineable precedences.
staticLevels :: [[RuleOperator Exp]]
staticLevels =
    [ noneSyn   (opWords " but does ")                            -- Traits
      ++ noneOps (opWords " leg cmp <=> .. ^.. ..^ ^..^ ff ^ff ff^ ^ff^ fff ^fff fff^ ^fff^ ")  -- Non-chaining Binary
      ++ postOps (opWords "...")                                 -- Infinite range
    , chainOps (opWords " != == < <= > >= ~~ eqv eq ne lt le gt ge =:= === ")
                                                                -- Chained Binary
    , leftOps  (opWords "&&")                                    -- Tight And
    , leftOps  (opWords " || ^^ // ")                            -- Tight Or
    , [ternOp "??" "!!" "if"]                                   -- Ternary
    -- Assignment
    , (rightOps (opWords " => ") ++) .                           -- Pair constructor
      (DependentPostfix listAssignment :) .
      (DependentPostfix immediateBinding :) .
      (rightAssignSyn :) .
      (rightDotAssignSyn :) $
      rightSyn (opWords (
               " := ~= += -= *= /= %= x= Y= \xA5= **= xx= ||= &&= //= ^^= " ++
               " +<= +>= ~<= ~>= +&= +|= +^= ~&= ~|= ~^= ?|= ?^= |= ^= &= "))
    , preOps (opWords " true not ")                              -- Loose unary
    ]

fileTestOps :: [RuleOperator Exp]
fileTestOps = optSymOps (Set.fromAscList (map (MkOpName . cast . (\x -> ['-', x])) fileTestOperatorNames))

fromSet :: Set OpName -> [String]
fromSet = cast . Set.toAscList

listAssignment :: Exp -> RuleParser Exp
listAssignment x = do
    try $ do
        char '='
        guard (not (isScalarLValue x))
        notFollowedBy (oneOf "=>" <|> (char ':' >> char '='))
        whiteSpace
    y   <- parseExpWithTightOps
    rhs <- option y $ do
        -- If we see comma, then convert this to a Syn ",".
        ruleComma
        ys <- parseExpWithTightOps `sepEndBy` ruleComma
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

immediateBinding :: Exp -> RuleParser Exp
immediateBinding x = do
    symbol "::="
    y <- parseExpWithTightOps
    unsafeEvalExp (Syn ":=" [x, y])
    return x

looseOperators :: RuleParser (RuleOperatorTable Exp)
looseOperators = do
    -- names <- currentListFunctions
    return $
        [ -- preOps names                               -- List Operator
          leftOps  (opWords " ==> ")                     -- Pipe Forward
        , leftOps  (opWords " and ")                     -- Loose And
        , leftOps  (opWords " or xor err ")              -- Loose Or
        ]

data CurrentFunction = MkCurrentFunction
    { f_var     :: !Var
    , f_assoc   :: !SubAssoc
    , f_params  :: !Params
    }

-- Read just the current state (i.e. not actually consuming anything)
currentFunctions :: RuleParser [CurrentFunction]
currentFunctions = do
    env <- getRuleEnv
    let funs = catMaybes $! inlinePerformSTM $! do
        glob <- readTVar $ envGlobal env
        let syms  = padToList (filterPad cur glob)
                    ++ padToList (filterPad cur (envLexical env))
            pkg   = envPackage env
            cur var@MkVar{ v_sigil = SCode} = inScope pkg var
            cur _ = False
            vars  = concat [ map (\(_, tvar) -> (var, tvar)) tvars
                           | (var, tvars) <- syms
                           ]
        mapM (uncurry filterFun) vars
    return (length funs `seq` funs)

{-# NOINLINE _RefToFunction #-}
--_RefToFunction :: L.IntMap (TVar VRef) CurrentFunction
_RefToFunction :: Hash.HashTable Int CurrentFunction
_RefToFunction = unsafePerformIO C.new

{- -- Unused instance for IntMap
instance Enum (TVar VRef) where
    toEnum = unsafeCoerce#
    fromEnum = unsafeCoerce#
-}

filterFun :: Var -> TVar VRef -> STM (Maybe CurrentFunction)
filterFun var tvar = do
    let key = unsafeCoerce# tvar
    res <- unsafeIOToSTM (C.lookup key _RefToFunction)
    case res of
        Just rv -> return (rv `seq` res)
        Nothing -> do
            ref <- readTVar tvar
            case ref of
                MkRef (ICode cv)
                    | relevantToParsing (code_type cv) (code_assoc cv) -> do
                        let rv = MkCurrentFunction var (code_assoc cv) (code_params cv)
                        unsafeIOToSTM (C.insert key rv _RefToFunction)
                        return (rv `seq` Just rv)
                MkRef (IScalar sv)
                    | Just (VCode cv) <- scalar_const sv
                    , relevantToParsing (code_type cv) (code_assoc cv) -> do
                        let rv = MkCurrentFunction var (code_assoc cv) (code_params cv)
                        unsafeIOToSTM (C.insert key rv _RefToFunction)
                        return (rv `seq` Just rv)
                _ -> return Nothing

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
        initResult  = MkTightFunctions emptySet emptySet emptySet emptySet termSet infixOps
        (unary, notUnary)   = partition matchUnary funs
        slurpyNames         = namesFrom (filter matchSlurpy notUnary)
        (maybeTerm, notTerm)= partition matchTerm funs
        terms               = namesFrom maybeTerm Set.\\ namesFrom notTerm
        termSet             = Set.map MkOpName terms
        infixOps            = Set.fromList
            [ MkOpName name
            | MkCurrentFunction { f_var = MkVar { v_categ = C_infix, v_name = name } } <- notUnary
            , name /= commaID
            ]
        splitUnary :: CurrentFunction -> TightFunctions -> TightFunctions
        splitUnary (MkCurrentFunction MkVar{ v_categ = cat, v_name = n } _ [param])
            res@MkTightFunctions
                { r_opt = opt, r_named = named, r_pre = pre, r_post = post }
                | n `Set.member` slurpyNames    = res
                | isOptional param              = res{ r_opt    = Set.insert (MkOpName n) opt }
                | C_prefix <- cat               = res{ r_pre    = Set.insert (MkOpName n) pre }
                | C_postfix <- cat              = res{ r_post   = Set.insert (MkOpName n) post }
                | otherwise                     = res{ r_named  = Set.insert (MkOpName n) named }
        splitUnary _ res = res
    return finalResult

namesFrom :: [CurrentFunction] -> Set ID
namesFrom = Set.fromList . map (v_name . f_var)

commaID :: ID
commaID = cast ","

data TightFunctions = MkTightFunctions
    { r_opt   :: !(Set OpName)
    , r_named :: !(Set OpName)
    , r_pre   :: !(Set OpName)
    , r_post  :: !(Set OpName)
    , r_term  :: !(Set OpName)
    , r_infix :: !(Set OpName)
    }

emptySet :: Set OpName
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

circumOps, rightSyn, chainOps, noneSyn, listSyn, preSyn, optPreSyn, preOps, preSymOps, optSymOps, postOps, optOps, leftOps, rightOps, noneOps, listOps :: Set OpName -> [RuleOperator Exp]
preSyn      = ops  $ makeOp1 Prefix "" Syn
optPreSyn   = ops  $ makeOp1 OptionalPrefix "" Syn
preOps      = (ops $ makeOp1 Prefix "&prefix:" doApp) . addHyperPrefix
preSymOps   = (ops $ makeOp1 Prefix "&prefix:" doAppSym) . addHyperPrefix
optSymOps   = (ops $ makeOp1 OptionalPrefix "&prefix:" doAppSym) . addHyperPrefix
postOps     = (ops $ makeOp1 Postfix "&postfix:" doApp) . addHyperPostfix
optOps      = (ops $ makeOp1 OptionalPrefix "&prefix:" doApp) . addHyperPrefix
leftOps     = (ops $ makeOp2 AssocLeft "&infix:" doApp) . addHyperInfix
rightOps    = (ops $ makeOp2 AssocRight "&infix:" doApp) . addHyperInfix
noneOps     = ops  $ makeOp2 AssocNone "&infix:" doApp
listOps     = ops  $ makeOp2 AssocLeft "&infix:" doApp
chainOps    = (ops $ makeOp2 AssocLeft "&infix:" doApp) . addHyperInfix . addNegation
rightSyn    = ops $ makeOp2 AssocRight "" Syn
noneSyn     = ops $ makeOp2 AssocNone "" Syn
listSyn     = ops $ makeOp0 AssocList "" Syn
circumOps   = ops $ makeCircumOp "&circumfix:"
rightAssignSyn :: RuleOperator Exp
rightAssignSyn = makeOp2Assign AssocRight "" Syn
rightDotAssignSyn :: RuleOperator Exp
rightDotAssignSyn = makeOp2DotAssign AssocRight "" Syn

{-# INLINE ops #-}
{-# SPECIALISE ops :: (String -> RuleOperator Exp) -> Set OpName -> [RuleOperator Exp] #-}
{-# SPECIALISE ops :: (String -> RuleParser String) -> Set OpName -> [RuleParser String] #-}
-- 0x10FFFF is the max number "chr" can take, so we use it for longest-token sorting.
-- buildExpressionParser will then use that information to make a longest-token match.
ops :: (String -> a) -> Set OpName -> [a]
ops f = map f . cast . Set.toAscList

makeOp1 :: (RuleParser (Exp -> Exp) -> RuleOperator Exp) -> 
        String -> 
        (String -> [Exp] -> Exp) -> 
        String -> 
        RuleOperator Exp
makeOp1 fixity sigil con name = fixity $ try $ do
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


makeCircumOp :: String -> String -> RuleOperator Exp
makeCircumOp sigil op = Term . try $
    between (lexeme $ string opener) (string closer) $
        enterBracketLevel ParensBracket $ do
            (invs, args) <- option (Nothing, []) parseNoParenParamList
            possiblyApplyMacro $ App (_Var name) invs args
    where
    name = sigil ++ opener ++ " " ++ closer
    [opener, closer] = words op

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
addHyperInfix :: Set OpName -> Set OpName
addHyperInfix xs = xs `Set.union` hyperTexan `Set.union` hyperFrench
    where
    hyperTexan = Set.mapMonotonic texan xs
    hyperFrench = Set.mapMonotonic french xs
    texan x = cast (Str.concat [__">>", cast x, __"<<"])
    french x = cast (Str.concat [__"\194\187", cast x, __"\194\171"])

{-|
Similar to 'addHyperInfix', but for prefix ops.

For example, @\"++ --\"@ would become
@\"++ ++\<\< ++« -- --\<\< --«\"@.
-}
addHyperPrefix :: Set OpName -> Set OpName
addHyperPrefix xs = xs `Set.union` hyperTexan `Set.union` hyperFrench
    where
    hyperTexan = Set.mapMonotonic texan xs
    hyperFrench = Set.mapMonotonic french xs
    texan x = cast (cast x +++ __"<<")
    french x = cast (cast x +++ __"\194\171")

{-|
Similar to 'addHyperInfix', but for postfix ops.

For example, @\"++ --\"@ would become
@\"++ >>++ »++ -- >>-- »--\"@.
-}
addHyperPostfix :: Set OpName -> Set OpName
addHyperPostfix xs = xs `Set.union` hyperTexan `Set.union` hyperFrench
    where
    hyperTexan = Set.mapMonotonic texan xs
    hyperFrench = Set.mapMonotonic french xs
    texan x = cast (__">>" +++ cast x)
    french x = cast (__"\194\187" +++ cast x)

addNegation :: Set OpName -> Set OpName
addNegation xs = xs `Set.union` Set.mapMonotonic negation xs
    where
    negation x = let buf = cast x in
        if Str.head buf == '!'
            then x
            else cast (Str.cons '!' (cast x))

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

ternOp :: String -> String -> String -> RuleOperator Exp
ternOp pre post syn = (`Infix` AssocRight) $ do
    symbol pre
    y <- parseExpWithTightOps
    symbol post
    return $ \x z -> Syn syn [x, y, z]




emptyTerm :: Exp
emptyTerm = Syn "" []

type TermOperator       = RuleParser Exp
type UnaryOperator      = RuleParser (Exp -> Exp)
type BinaryOperator     = RuleParser (Exp -> Exp -> Exp)
type ListOperator       = RuleParser ([Exp] -> Exp)
type DependentOperator  = Exp -> RuleParser Exp

data OpRow = MkOpRow
    { o_rassoc      :: ![BinaryOperator]
    , o_lassoc      :: ![BinaryOperator]
    , o_nassoc      :: ![BinaryOperator]
    , o_prefix      :: ![UnaryOperator]
    , o_postfix     :: ![UnaryOperator]
    , o_optPrefix   :: ![UnaryOperator]
    , o_listAssoc   :: ![ListOperator]
    , o_depPostfix  :: ![DependentOperator]
    , o_term        :: ![TermOperator]
    }

-----------------------------------------------------------
-- Convert an OperatorTable and basic term parser into
-- a full fledged expression parser
-----------------------------------------------------------
buildExpressionParser :: RuleOperatorTable Exp -> RuleParser Exp -> RuleParser Exp
buildExpressionParser = flip (foldl makeParser)

{-# INLINE makeParser #-}
makeParser :: RuleParser Exp -> [RuleOperator Exp] -> RuleParser Exp
makeParser simpleTerm ops = do
    x <- termP
    rassocP x <|> lassocP x <|> nassocP x <|> listAssocP x <|> return x <?> "operator"
    where
    MkOpRow rassoc lassoc nassoc prefix postfix optPrefix listAssoc depPostfix term
        = foldr splitOp (MkOpRow [] [] [] [] [] [] [] [] []) ops
    rassocOp          = {-# SCC "rassocOp" #-}      choice rassoc
    lassocOp          = {-# SCC "lassocOp" #-}      choice lassoc
    nassocOp          = {-# SCC "nassocOp" #-}      choice nassoc
    prefixOp          = {-# SCC "prefixOp" #-}      choice prefix <?> ""
    postfixOp         = {-# SCC "postfixOp" #-}     choice postfix <?> ""
    optPrefixOp       = {-# SCC "optPrefixOp" #-}   choice optPrefix <?> ""
    listAssocOp       = {-# SCC "listAssocOp" #-}   choice listAssoc
    depPostfixOp x    = {-# SCC "depPostfixOp" #-}  choice (map ($ x) depPostfix) <?> ""
    termOp            = {-# SCC "termOp" #-}        choice term <|> simpleTerm

    ambig assoc op    = try
        (op >> fail ("ambiguous use of a " ++ assoc ++ " associative operator"))
    ambigRight        = ambig "right" rassocOp
    ambigLeft         = ambig "left" lassocOp
    ambigNon          = ambig "non" nassocOp

    foldOp = foldr (.) id
    termP = {-# SCC "termP" #-} do
        pres    <-  many $ (fmap Left prefixOp) <|> (fmap Right optPrefixOp)
        -- Here we handle optional-prefix operators.
        x       <- if null pres then termOp else case last pres of
            Left _  -> termOp
            _       -> option emptyTerm termOp
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
splitOp col row@(MkOpRow rassoc lassoc nassoc prefix postfix optPrefix listAssoc depPostfix term) = case col of
    Infix op AssocNone      -> row{ o_nassoc    = op:nassoc }
    Infix op AssocLeft      -> row{ o_lassoc    = op:lassoc }
    Infix op AssocRight     -> row{ o_rassoc    = op:rassoc }
    InfixList op AssocList  -> row{ o_listAssoc = op:listAssoc }
    Prefix op               -> row{ o_prefix    = op:prefix }
    Postfix op              -> row{ o_postfix   = op:postfix }
    OptionalPrefix op       -> row{ o_optPrefix = op:optPrefix }
    DependentPostfix op     -> row{ o_depPostfix= op:depPostfix }
    Term op                 -> row{ o_term      = op:term }
    -- FIXME: add AssocChain
    _ -> internalError $ "Unhandled operator type" ++ show (op_assoc col)


refillCache :: RuleState -> (DynParsers -> RuleParser Exp) -> RuleParser Exp
refillCache state f = do
    (terms, opsTight)   <- tightOperators
    opsLoose            <- looseOperators
    let tightExprs  = buildExpressionParser opsTight parseTerm
        parseTight  = expRule tightExprs
        parseFull   = expRule (buildExpressionParser opsFull tightExprs)
        parseLit    = expRule (buildExpressionParser opsLoose tightExprs)
        opParsers   = MkDynParsers parseFull parseTight parseLit parseNullary
        opsFull     = listCons:listInfix:opsLoose
        parseNullary = try $ do
            name <- choice . map symbol . fromSet $ terms
            notFollowedBy (char '(' <|> (char ':' >> char ':'))
            possiblyApplyMacro $ App (_Var ('&':name)) Nothing []
    setState state{ s_dynParsers = opParsers }
    f opParsers

-- was: parseOp
parseExpWithOps :: RuleParser Exp
parseExpWithOps = parseExpWithCachedParser dynParseOp

-- was: parseTightOp
parseExpWithTightOps :: RuleParser Exp
parseExpWithTightOps = parseExpWithCachedParser dynParseTightOp

-- Parse something in item context -- i.e. everything minus list-associative ones
parseExpWithItemOps :: RuleParser Exp
parseExpWithItemOps = parseExpWithCachedParser dynParseLitOp

-- was: parseOpWith
parseExpWithCachedParser :: (DynParsers -> RuleParser Exp) -> RuleParser Exp
parseExpWithCachedParser f = do
    state <- getState
    case s_dynParsers state of
        MkDynParsersEmpty   -> refillCache state f
        p                   -> f p

ruleFoldOp :: RuleParser String
ruleFoldOp = verbatimRule "reduce metaoperator" $ try $ do
    char '['
    keep <- option "" $ string "\\"
    -- XXX - Instead of a lookup, add a cached parseInfix here!
    MkTightFunctions{ r_infix = infixOps } <- currentTightFunctions
    -- name <- choice $ ops (try . string) (addHyperInfix $ infixOps ++ defaultInfixOps)
    name <- verbatimRule "infix operator" $ do
        choice $ ops (try . string)
            (addHyperInfix (infixOps `Set.union` defaultInfixOps))
    char ']'
    possiblyHyper <- option "" ((char '\171' >> return "<<") <|> (string "<<"))
    return $ "&prefix:[" ++ keep ++ name ++ "]" ++ possiblyHyper
    where
    -- XXX !~~ needs to turn into metaop plus ~~
    defaultInfixOps = opWords $ concat
        [ " ** * / % x xx +& +< +> ~& ~< ~> "
        , " + - ~ +| +^ ~| ~^ ?| , Y \xA5 "
        , " & ^ | "
        , " => = "
        , " != == < <= > >= ~~ !~~ "
        , " eq ne lt le gt ge =:= === "
        , " && "
        , " || ^^ // "
        , " and or xor err "
        , " .[] .{} "
        ]

