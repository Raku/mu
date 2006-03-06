{-# OPTIONS_GHC -cpp -fglasgow-exts -funbox-strict-fields -fno-full-laziness -fno-cse #-}
{-# OPTIONS_GHC -#include "../UnicodeC.h" #-}

module Pugs.Parser.Operator where
import Pugs.Internals
import Pugs.AST
import Pugs.Types
import Pugs.Lexer
import Pugs.Rule
import Pugs.Rule.Expr
import qualified Data.Set as Set

import Pugs.Parser.Types

operators :: (?parseExpWithTightOps :: RuleParser Exp) =>
    RuleParser (RuleOperatorTable Exp)
operators = do
    tight <- tightOperators
    loose <- looseOperators
    return $ concat $
        [ tight
        , [ listSyn  " , ", listOps " Y ¥ " ]           -- Comma
        , loose
    --  , [ listSyn  " ; " ]                            -- Terminator
        ]

-- Not yet transcribed ------------------------------------------------

tightOperators :: (?parseExpWithTightOps :: RuleParser Exp) =>
    RuleParser (RuleOperatorTable Exp)
tightOperators = do
  [_, optionary, namedUnary, preUnary, postUnary, infixOps] <- currentTightFunctions
  return $
    [ methOps  " . .+ .? .* .+ .() .[] .{} .<<>> .= "   -- Method postfix
    , postOps  " ++ -- " ++ preOps " ++ -- "            -- Auto-Increment
    , rightOps " ** "                                   -- Exponentiation
    , preSyn "*"                                        -- Symbolic Unary
      ++ preOps (concatMap (\x -> " -" ++ [x]) "rwxoRWXOezsfdlpSbctugkTBMAC")
      ++ preOps " = ! + - ** ~ ? +^ ~^ ?^ \\ ^"
      ++ preSymOps preUnary
      ++ postOps postUnary
    , leftOps  " * / % x xx +& +< +> ~& ~< ~> "         -- Multiplicative
    , leftOps  " + - ~ +| +^ ~| ~^ ?| " -- Additive
      ++ leftOps infixOps                               -- User defined ops
    , listOps  " & "                                    -- Junctive And
    , listOps  " ^ | "                                  -- Junctive Or
    , optOps optionary, preOps namedUnary               -- Named Unary
    , noneSyn  " is but does "                          -- Traits
      ++ noneOps " cmp <=> .. ^.. ..^ ^..^ till ^till till^ "  -- Non-chaining Binary
      ++ postOps "..."                                  -- Infinite range
    , chainOps $
               " != == < <= > >= ~~ !~ " ++
               " eq ne lt le gt ge =:= === "            -- Chained Binary
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

-- not a parser!
litOperators :: (?parseExpWithTightOps :: RuleParser Exp) =>
    RuleParser (RuleOperatorTable Exp)
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
                            | relevantToParsing (code_assoc cv) (code_type cv)
                            -> Just (name', code_assoc cv, code_params cv)
                        MkRef (IScalar sv)
                            | Just (VCode cv) <- scalar_const sv
                            , relevantToParsing (code_assoc cv) (code_type cv)
                            -> Just (name', code_assoc cv, code_params cv)
                        _ -> Nothing
    where
    inScope _ ('L':'i':'s':'t':':':':':name) = Just name
    inScope pkg name | Just (post, pre) <- breakOnGlue "::" (reverse name) =
        if pkg == reverse pre then Just (reverse post) else Nothing
    inScope _ name = Just name
    relevantToParsing "pre" SubPrim      = True
    relevantToParsing _     SubPrim      = False
    relevantToParsing _     SubMethod    = False
    relevantToParsing _     _            = True

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

ops :: (String -> a) -> String -> [a]
ops f = map (f . snd) . sort . map (\x -> (-length x, x)) . nub . words . decodeUTF8


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

doApp :: String -> [Exp] -> Exp
doApp str args = App (Var str) Nothing args

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

methOps             :: a -> [b]
methOps _ = []

doAppSym :: String -> [Exp] -> Exp
doAppSym name@(_:'p':'r':'e':'f':'i':'x':':':_) args = App (Var name) Nothing args
doAppSym (sigil:name) args = App (Var (sigil:("prefix:"++name))) Nothing args
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
