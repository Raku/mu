{-# OPTIONS_GHC -cpp -fglasgow-exts -funbox-strict-fields -fno-full-laziness -fno-cse -fallow-overlapping-instances -fno-warn-orphans #-}

module Pugs.Parser.Operator where
import Pugs.Internals
import Pugs.AST
import Pugs.Types
import Pugs.Lexer
import Pugs.Rule
import {-# SOURCE #-} Pugs.Parser
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.ByteString.UTF8 as Str
import qualified Data.ByteString.Char8 as Buf -- XXX
import qualified Data.HashTable as H
import GHC.Int (Int32(I32#))

import Pugs.Parser.Types
import Pugs.Parser.Unsafe

listCons :: [RuleOperator Exp]
listCons = listSyn (opWords ",")                         -- List constructor

listInfix :: [RuleOperator Exp]
listInfix = listOps (opWords "Z X minmax ==> <==")  -- List infix

opWords :: String -> Set OpName
opWords xs = Set.fromList (map (MkOpName . cast) (words xs))

newtype OpName = MkOpName ID
    deriving (Show, Eq, Typeable, (:>:) String, (:>:) ByteString, (:<:) ByteString, (:>:) ID, (:<:) String, (:<:) ID)

instance Ord OpName where
    compare (MkOpName x) (MkOpName y)
        = compare (Buf.length (idBuf y)) (Buf.length (idBuf x)) `mappend` compare (idKey y) (idKey x)

instance ((:<:) OpName) ByteString where
    castBack (MkOpName id) = castBack id

-- Not yet transcribed into a full optable parser with dynamic precedence

tightOperators :: RuleParser (TightFunctions, RuleOperatorTable Exp)
tightOperators = do
  tights <- currentTightFunctions
  return $ (,) tights
    ( termLevel                     -- Terms and circumfixes
    : methLevel                     -- Method postfix
    : incrLevel                     -- Auto-Increment
    : expoLevel                     -- Exponentiation
    : (preSymOps (r_pre tights)     -- Symbolic Unary (user-definable)
        ++ postOps (r_post tights)
        ++ symbLevel)
    : multLevel                     -- Multiplicative
    : Map.foldWithKey foldInfix addiLevel (r_infix tights) -- Additive (user-definable)
    : replLevel                     -- Replication
    : concLevel                     -- Concatenation
    : junaLevel                     -- Junctive And
    : junoLevel                     -- Junctive Or
    : (optOps (r_opt tights)        -- Named Unary (user-definable)
      ++ preOps (r_named tights Set.\\ opWords " true not ")
      )
    : staticLevels
    )
    where
    foldInfix :: OpName -> SubAssoc -> [RuleOperator Exp] -> [RuleOperator Exp]
    foldInfix op assoc xs = let op' = Set.singleton op in case assoc of
        A_left  -> leftOps op'  ++ xs
        A_right -> rightOps op' ++ xs
        A_non   -> nonOps op'   ++ xs
        A_chain -> chainOps op' ++ xs
        A_list  -> listOps op'  ++ xs
        _       -> leftOps op'  ++ xs   -- Default to left-assoc
        -- _ -> error $ "Impossible: " ++ show op ++ " has no assoc?"

termLevel, methLevel, incrLevel, expoLevel, symbLevel, multLevel, addiLevel, replLevel, concLevel, junaLevel, junoLevel :: [RuleOperator Exp]
termLevel = circumOps (Set.singleton (MkOpName (cast "\\( )")))
methLevel = methOps (opWords " . .+ .? .* .+ .() .[] .{} .<<>> .= ")
incrLevel = postOps incrOpsPost ++ preOps incrOpsPre
expoLevel = rightOps (opWords " ** ")
symbLevel = preSyn (Set.singleton (MkOpName (cast "|"))) ++ preOps symbPreops
multLevel = leftOps (opWords " * / % +& +< +> ~& ~< ~> ?& ")
addiLevel = leftOps (opWords " + - +| +^ ~| ~^ ?| ?^ ")
replLevel = leftOps (opWords " x xx ")
concLevel = leftOps (opWords " ~ ")
junaLevel = listOps (opWords " & ")
junoLevel = listOps (opWords " ^ | ")

symbPreops :: Set OpName
symbPreops = opWords " = ! + - ~ ? +^ ~^ ?^ \\ ^"

incrOpsPre :: Set OpName
incrOpsPre = opWords " ++ -- "

incrOpsPost :: Set OpName
incrOpsPost = opWords " ++ -- i "

-- The lower levels of immutable ops.  This will be replaced once we have
-- user-defineable precedences.
staticLevels :: [[RuleOperator Exp]]
staticLevels =
    [ nonSyn   (opWords " but does ")                            -- Traits
      ++ nonOps (opWords " leg cmp <=> .. ^.. ..^ ^..^ ff ^ff ff^ ^ff^ fff ^fff fff^ ^fff^ ")  -- Non-chaining Binary
    , chainOps (opWords " != == < <= > >= eqv eq ne lt le gt ge =:= === ")
      ++ matchOps (opWords " ~~ =~ ")
    , leftOps  (opWords "&&")                                    -- Tight And
    , leftOps  (opWords " || ^^ // ")                            -- Tight Or
    , [ternOp "??" "!!" "if"]                                   -- Ternary
    -- Assignment
    , (rightOps (opWords " => ") ++) .                           -- Pair constructor
      (DependentPostfix listAssignment :) .
      (DependentPostfix immediateBinding :) .
      (rightAssignSyn :) .
      (rightDotAssignSyn :) .
      (rightSyn (opWords " := ") ++) $
      rightSyn infixAssignmentOps
    , preOps (opWords " true not ")                              -- Loose unary
    ]

infixAssignmentOps :: Set OpName
infixAssignmentOps = opWords
    ( " ~= += -= *= /= %= x= Z= X= **= xx= ||= &&= //= ^^= "
    ++ " +<= +>= ~<= ~>= +&= +|= +^= ~&= ~|= ~^= ?|= ?^= |= ^= &= "
    )

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
    forceParens (Sym x y flags init inner) = Sym x y flags init (forceParens inner)
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
        , leftOps  (opWords " and andthen ")                     -- Loose And
        , leftOps  (opWords " or xor orelse ")              -- Loose Or
        ]

data CurrentFunction = MkCurrentFunction
    { f_var     :: !Var
    , f_assoc   :: !SubAssoc
    , f_params  :: !Params
    }
    deriving (Show)

-- Read just the current state (i.e. not actually consuming anything)
currentFunctions :: RuleParser [CurrentFunction]
currentFunctions = do
    env <- getRuleEnv
    let funs = catMaybes $! inlinePerformSTM $! do
        glob <- readMPad $ envGlobal env
        let vars  = padToList (filterPad cur glob)
                    ++ padToList (filterPad cur (envLexical env))
            pkg   = envPackage env
            cur var@MkVar{ v_sigil = SCode, v_longname = name } | name == nullID = inScope pkg var
            cur _ = False
        mapM (uncurry filterFun) vars
    return (length funs `seq` funs)

{-# NOINLINE _RefToFunction #-}
_RefToFunction :: H.HashTable PadEntry (Maybe CurrentFunction)
_RefToFunction = unsafePerformIO (H.new (==) hashPadEntry)

hashPadEntry :: PadEntry -> Int32
hashPadEntry PEConstant{ pe_proto = v }  = I32# (unsafeCoerce# v)
hashPadEntry x                              = I32# (unsafeCoerce# (pe_store x))

-- hashTVar :: TVar VRef -> Int32
-- hashTVar x = I32# (unsafeCoerce# x)

filterFun :: Var -> PadEntry -> STM (Maybe CurrentFunction)
filterFun var entry = var `seq` do
    res <- unsafeIOToSTM (H.lookup _RefToFunction entry)
    case res of
        Just rv -> return rv
        Nothing -> do
            ref <- readPadEntry entry
            filterRef ref
    where
    filterRef :: VRef -> STM (Maybe CurrentFunction)
    filterRef (MkRef (ICode cv))
        | relevantToParsing (code_type cv) (code_assoc cv) = do
            let rv = MkCurrentFunction var (code_assoc cv) (code_params cv)
                res = seq rv (Just rv)
            unsafeIOToSTM (H.insert _RefToFunction entry res)
            return res
    filterRef (MkRef (IScalar sv))
        | Just (VCode cv) <- scalar_const sv
        , relevantToParsing (code_type cv) (code_assoc cv) = do
            let rv = MkCurrentFunction var (code_assoc cv) (code_params cv)
                res = seq rv (Just rv)
            unsafeIOToSTM (H.insert _RefToFunction entry res)
            return res
    filterRef _ = do
        unsafeIOToSTM (H.insert _RefToFunction entry Nothing)
        return Nothing

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
relevantToParsing _          AIrrelevantToParsing   = False
relevantToParsing _          _      = True

-- XXX Very bad hacky kluge just for Parser.Operator
--     Switch to macro export for push(@x, 1) instead!
listPkg :: Pkg
listPkg = cast (mkType "List")

-- read just the current state
currentTightFunctions :: RuleParser TightFunctions
currentTightFunctions = do
    funs    <- currentFunctions
    let finalResult = foldr splitUnary termResult unary
        termResult  = foldr splitTerm initResult maybeTerm
        initResult  = MkTightFunctions emptySet emptySet emptySet emptySet emptyMap infixOps
        (unary, notUnary)   = partition matchUnary funs
        slurpyNames         = namesFrom (filter matchSlurpy notUnary)
        (maybeTerm, notTerm)= partition matchTerm funs
        nonTermNames        = namesFrom notTerm
        infixOps            = Map.fromList
            [ (MkOpName name, assoc)
            | MkCurrentFunction { f_var = MkVar { v_categ = C_infix, v_name = name }, f_assoc = assoc } <- notUnary
            , name /= commaID
            ]
        splitTerm :: CurrentFunction -> TightFunctions -> TightFunctions
        splitTerm (MkCurrentFunction MkVar{ v_categ = cat, v_name = n } _ _)
            res@MkTightFunctions{ r_term = term }
                | n `Set.member` nonTermNames   = res
                | otherwise                     = res{ r_term = Map.insert (MkOpName n) cat term }

        splitUnary :: CurrentFunction -> TightFunctions -> TightFunctions
        splitUnary (MkCurrentFunction MkVar{ v_categ = cat, v_name = n } _ [param])
            res@MkTightFunctions{ r_opt = opt, r_named = named, r_pre = pre, r_post = post }
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
    { r_opt         :: !(Set OpName)
    , r_named       :: !(Set OpName)
    , r_pre         :: !(Set OpName)
    , r_post        :: !(Set OpName)
    , r_term        :: !(Map OpName VarCateg)
    , r_infix       :: !(Map OpName SubAssoc)
    }

emptySet :: Set OpName
emptySet = Set.empty

emptyMap :: Map OpName VarCateg
emptyMap = Map.empty

matchUnary :: CurrentFunction -> Bool
matchUnary MkCurrentFunction
    { f_assoc = ANil, f_params = [MkOldParam
        { paramContext = CxtItem{}, isNamed = False }] } = True
matchUnary _ = False

matchTerm :: CurrentFunction -> Bool
matchTerm MkCurrentFunction{ f_var = MkVar{ v_categ = C_term } } = True
matchTerm MkCurrentFunction{ f_assoc = ANil, f_params = [] } = True
matchTerm _ = False

matchSlurpy :: CurrentFunction -> Bool
matchSlurpy MkCurrentFunction
    { f_params = (_:_:_) } = True
matchSlurpy MkCurrentFunction
    { f_params = [MkOldParam
        { paramContext = CxtSlurpy{}, paramName = MkVar{ v_sigil = sig } }] }
            = sig == SArray || sig == SArrayMulti
matchSlurpy _ = False

circumOps, rightSyn, chainOps, matchOps, nonSyn, listSyn, preSyn, preOps, preSymOps, postOps, optOps, leftOps, rightOps, nonOps, listOps :: Set OpName -> [RuleOperator Exp]
preSyn      = ops  $ makeOp1 Prefix "" Syn
preOps      = (ops $ makeOp1 Prefix "&prefix:" doApp) . addHyperPrefix
preSymOps   = (ops $ makeOp1 Prefix "&prefix:" doAppSym) . addHyperPrefix
postOps     = (ops $ makeOp1 Postfix "&postfix:" doApp) . addHyperPostfix
optOps      = (ops $ makeOp1 OptionalPrefix "&prefix:" doApp) . addHyperPrefix
leftOps     = (ops $ makeOp2 AssocLeft "&infix:" doApp) . addHyperInfix
rightOps    = (ops $ makeOp2 AssocRight "&infix:" doApp) . addHyperInfix
nonOps      = ops  $ makeOp2 AssocNone "&infix:" doApp
listOps     = ops  $ makeOp2 AssocLeft "&infix:" doApp
matchOps    = (ops $ makeOp2Match AssocLeft "&infix:" doApp) . addHyperInfix . addNegation
chainOps    = (ops $ makeOp2 AssocLeft "&infix:" doApp) . addHyperInfix . addNegation
rightSyn    = ops $ makeOp2 AssocRight "" Syn
nonSyn      = ops $ makeOp2 AssocNone "" Syn
listSyn     = ops $ makeOp0 AssocList "" Syn
circumOps   = ops $ makeCircumOp "&circumfix:"
rightAssignSyn :: RuleOperator Exp
rightAssignSyn = makeOp2Assign AssocRight "" Syn
rightDotAssignSyn :: RuleOperator Exp
rightDotAssignSyn = makeOp2DotAssign AssocRight "" Syn

{-# INLINE ops #-}
{-# SPECIALISE ops :: (String -> RuleOperator Exp) -> Set OpName -> [RuleOperator Exp] #-}
{-# SPECIALISE ops :: (String -> RuleParser String) -> Set OpName -> [RuleParser String] #-}
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
        | isWordAny (last name) = choice autoquoters
        | otherwise = conOp fullName
    autoquoters = 
        [ char '(' >> unexpected "(" 
        , string "=>" >> unexpected "=>"
        , conOp fullName
        ]
    fullName
        | isAlpha (head name)
        , "&prefix:" <- sigil
        = ('&':name)
        | otherwise
        = sigil ++ name
    conOp name = return $ \x -> case x of
        Syn "" []   -> con name []
        _           -> con name [x]


makeCircumOp :: String -> String -> RuleOperator Exp
makeCircumOp sigil op = Term . try $
    between (lexeme $ string opener) (string closer) $
        enterBracketLevel ParensBracket $ do
            (invs, args) <- option (Nothing, []) parseNoParenArgList
            possiblyApplyMacro $ App (_Var name) invs args
    where
    name = sigil ++ opener ++ " " ++ closer
    [opener, closer] = words op

-- Just for the "state $foo = 1" rewriting
makeOp2Assign :: Assoc -> String -> (String -> [Exp] -> Exp) -> RuleOperator Exp
makeOp2Assign prec _ con = (`Infix` prec) $ do
    symbol "="
    return $ \invExp argExp -> (con "=" [invExp, argExp])

-- Rewrite "EXP ~~ .meth" into "?(EXP.meth)"
makeOp2Match :: Assoc -> String -> (String -> [Exp] -> Exp) -> String -> RuleOperator Exp
makeOp2Match prec sigil con name = (`Infix` prec) $ do
    symbol name
    when (name == "=~") $ do
        fail "There is no =~ operator in Perl 6 -- did you mean ~~ (match) or ~= (concat-assign)?"
    return $ \x y -> case y of
        Syn syn [Var var, rhs] | var == varTopic ->
            App (_Var "&prefix:?") Nothing [Syn syn [x, rhs]]
        App app (Just (Var var)) args | var == varTopic ->
            App (_Var "&prefix:?") Nothing [App app (Just x) args]
        _ -> con (sigil ++ name) [x,y]

-- Just for the ".=" rewriting
makeOp2DotAssign :: Assoc -> String -> (String -> [Exp] -> Exp) -> RuleOperator Exp
makeOp2DotAssign prec _ con = (`Infix` prec) $ do
    symbol ".="
    insertIntoPosition "." -- "$x .= foo" becomes "$x .= .foo"
    return $ \invExp argExp -> case argExp of
        -- XXX - App meth _ args -> con ".=" [invExp, App meth Nothing args]
        App meth _ args
            | meth == Var varTopic  -> con "=" [invExp, App invExp Nothing args]
            | otherwise             -> con "=" [invExp, App meth (Just invExp) args]
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
    texan x = cast (Buf.concat [__">>", cast x, __"<<"])
    french x = cast (Buf.concat [__"\187", cast x, __"\171"])

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
    french x = cast (cast x +++ __"\171")

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
    french x = cast (cast "\187" +++ cast x)

{-|
Add prefix \

-}
addScanPrefix :: Set OpName -> Set OpName
addScanPrefix xs = xs `Set.union` scanPrefix
    where
    scanPrefix = Set.mapMonotonic scan xs
    scan x = cast (Buf.cons '\\' (cast x))

addNegation :: Set OpName -> Set OpName
addNegation xs = xs `Set.union` Set.mapMonotonic negation xs
    where
    negation x = let buf = cast x in
        if Buf.head buf == '!'
            then x
            else cast (Buf.cons '!' (cast x))

methOps             :: a -> [b]
methOps _ = []

doAppSym :: String -> [Exp] -> Exp
doAppSym name@(_:'p':'r':'e':'f':'i':'x':':':_) args = App (_Var name) Nothing args
doAppSym (sigil:name) args = App (_Var (sigil:("prefix:"++name))) Nothing args
doAppSym _ _ = error "doAppSym: bad name"


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
    rassocOp          = {-# SCC "rassocOp" #-}      choice rassoc <?> ""
    lassocOp          = {-# SCC "lassocOp" #-}      choice lassoc <?> ""
    nassocOp          = {-# SCC "nassocOp" #-}      choice nassoc <?> ""
    prefixOp          = {-# SCC "prefixOp" #-}      choice prefix <?> ""
    postfixOp         = {-# SCC "postfixOp" #-}     choice postfix <?> ""
    optPrefixOp       = {-# SCC "optPrefixOp" #-}   choice optPrefix <?> ""
    listAssocOp       = {-# SCC "listAssocOp" #-}   choice listAssoc <?> ""
    depPostfixOp x    = {-# SCC "depPostfixOp" #-}  choice (map ($ x) depPostfix) <?> ""
    termOp            = {-# SCC "termOp" #-}        choice term <|> (simpleTerm <?> "")

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
        fmap (foldOp posts) $ foldM maybeApplyPrefixMacro x' (map liftEither $ reverse pres)

    maybeApplyPrefixMacro t f = {-# SCC "maybeApplyPrefixMacro" #-} possiblyApplyMacro (f t)

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


refillCache :: RuleState -> (DynParsers -> RuleParser a) -> RuleParser a
refillCache state f = do
    (tights, opsTight)  <- tightOperators
    opsLoose            <- looseOperators
    let tightExprs  = buildExpressionParser opsTight parseTerm
        parseTight  = expRule tightExprs
        parseFull   = expRule (buildExpressionParser opsFull tightExprs)
        parseLit    = expRule (buildExpressionParser opsLoose tightExprs)
        parsePost   = pp "&postfix:" $ incrOpsPost `Set.union` r_post tights
     -- parsePre    = pp "&prefix:"  $ symbPreops `Set.union` r_pre tights
     -- parsePreNam = pp "&"         $ r_named tights `Set.union` r_opt tights
        pp pre ops  = fmap (pre ++) (tryChoice . map string . fromSet $ ops)
        opParsers   = MkDynParsers parseFull parseTight parseLit parseNullary parsePost -- <|> parsePre <|> parsePreNam)
        opsFull     = listCons:listInfix:opsLoose
        parseNullary= try $ do
            var <- (choice . map parseOneTerm . Map.toAscList $ r_term tights) <?> "term"
            notFollowedBy (char '(' <|> (char ':' >> char ':'))
            possiblyApplyMacro $ App (Var var) Nothing []
        parseOneTerm (name :: OpName, categ) = do
            symbol (cast name)
            return MkVar
                { v_name    = cast name
                , v_sigil   = SCode
                , v_twigil  = TNil
                , v_categ   = categ
                , v_package = emptyPkg
                , v_meta    = MNil
                , v_longname= nullID
                }
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
parseExpWithCachedParser :: (DynParsers -> RuleParser a) -> RuleParser a
parseExpWithCachedParser f = do
    state <- getState
    case s_dynParsers state of
        MkDynParsersEmpty   -> refillCache state f
        p                   -> f p


ruleHyperPre :: RuleParser String
ruleHyperPre = ((char '\187' >> return ">>") <|> (string ">>"))

ruleHyperPost :: RuleParser String
ruleHyperPost = ((char '\171' >> return "<<") <|> (string "<<"))

-- XXX - the rulePipeHyper below should be more generic and put all +<< etc to listop level
rulePipeHyper :: RuleParser Var
rulePipeHyper = verbatimRule "" $ do
    -- sig <- (fmap show ruleSigil) <|> string "|"
    char '|'
    ruleHyperPost
    return $ cast "&prefix:|<<"

ruleInfixOp :: RuleParser String
ruleInfixOp = verbatimRule "infix operator" $ do
    -- XXX - Instead of a lookup, add a cached parseInfix here!
    MkTightFunctions{ r_infix = infixOps } <- currentTightFunctions
    choice $ ops (try . string)
        (addScanPrefix (addHyperInfix (Map.keysSet infixOps `Set.union` defaultInfixOps)))

ruleInfixAssignment :: RuleParser String
ruleInfixAssignment = choice $ ops (try . string) infixAssignmentOps

-- XXX !~~ needs to turn into metaop plus ~~
defaultInfixOps :: Set OpName
defaultInfixOps = opWords $ concat
    [ " ** * / % x xx +& +< +> ~& ~< ~> "
    , " + - ~ +| +^ ~| ~^ ?| ?^ , Z X minmax "
    , " & ^ | "
    , " => = "
    , " != == < <= > >= ~~ "
    , " !== !< !<= !> !>= !~~ "
    , " eq ne lt le gt ge =:= === eqv "
    , " !eq !ne !lt !le !gt !ge !=:= !=== !eqv "
    , " && "
    , " || ^^ // "
    , " and or xor orelse andthen "
    , " .[] .{} "
    ]

ruleFoldOp :: RuleParser Var
ruleFoldOp = tryVerbatimRule "reduce metaoperator" $ rulePipeHyper <|> do
    char '['
    name <- ruleInfixOp
    char ']'
--    possiblyHyper <- option "" ruleHyperPost
--
    -- S03: If there is ambiguity between a triangular reduce and an infix operator
    --      beginning with backslash, the infix operator is chosen.
    let var = cast ("&prefix:[" ++ name ++ "]")
        nameID = cast name
    case name of
        ('\\':_)  -> do
            MkTightFunctions{ r_infix = infixOps } <- currentTightFunctions
            return $ if MkOpName nameID `Map.member` infixOps
                then var{ v_name = nameID, v_meta = MFold }
                else var
        _ -> return var

