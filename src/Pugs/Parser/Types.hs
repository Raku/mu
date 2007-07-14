{-# OPTIONS_GHC -fglasgow-exts -funbox-strict-fields #-}

module Pugs.Parser.Types (
    RuleParser, RuleState(..), CharClass(..),
    DynParsers(..), ParensOption(..), FormalsOption(..), BracketLevel(..), OuterLevel,
    BlockInfo(..), emptyBlockInfo,

    RuleOperator, RuleOperatorTable,
    getRuleEnv, modifyRuleEnv, putRuleEnv, insertIntoPosition,
    clearDynParsers, enterBracketLevel, charClassOf,
    addBlockPad, popClosureTrait, addClosureTrait,
    -- Alternate Char implementations that keeps track of s_charClass
    satisfy, string, oneOf, noneOf, char, hexDigit, octDigit,
    digit, upper, anyChar, expRule, parserWarn, mkPos,

    Operator(..), Assoc(..),
) where
import Pugs.AST
import Pugs.Rule
import Pugs.Types
import Pugs.Internals
import Pugs.Pretty (pretty)
import Text.ParserCombinators.Parsec.Pos
import Debug.Trace
import qualified Data.Map as Map
import qualified Data.Set as Set

data BlockInfo = MkBlockInfo
    { bi_pad    :: !Pad
    , bi_traits :: !(TraitBlocks -> TraitBlocks)
    , bi_body   :: !Exp
    }

emptyBlockInfo :: BlockInfo
emptyBlockInfo = MkBlockInfo emptyPad id emptyExp


{-# INLINE satisfy #-}
satisfy :: (Char -> Bool) -> RuleParser Char
satisfy f = tokenPrim
    (\c -> show [c]) 
    (\pos c _ -> updatePosChar pos c) 
    -- (Just (\_ c _ state -> state{ s_char = c }))
    (\c -> if f c then Just c else Nothing)

{-# INLINE string #-}
string :: String -> RuleParser String
string s = tokens show updatePosString s
--    `finallyM` modify (\state -> state{ s_char = last s })

{-
_captureNamed :: ID -> RuleParser a -> RuleParser a
_captureNamed newState rule = do
    prev <- gets s_name
    modify $ \state -> state{ s_name = newState }
    rv <- rule
    modify $ \state -> state{ s_name = prev }
    return rv

_capturePositioned :: Int -> RuleParser a -> RuleParser a
_capturePositioned pos rule = do
    prev <- gets s_pos
    modify $ \state -> state{ s_pos = pos }
    rv <- rule
    modify $ \state -> state{ s_pos = prev }
    return rv
-}

charClassOf :: Char -> CharClass
charClassOf c   | isAlphaNum c  = WordClass
                | isSpace c     = SpaceClass
                | '_' <- c      = WordClass
                | otherwise     = SymClass

{-

getCurrCharClass :: RuleParser CharClass
getCurrCharClass = fmap charClassOf (lookAhead anyToken) <|> return SpaceClass

getPrevCharClass :: RuleParser CharClass
getPrevCharClass = do
    p   <- gets s_wsPos
    p'  <- getPosition
    return (if (p == p') then SpaceClass else WordClass)
-}

oneOf, noneOf :: [Char] -> RuleParser Char
oneOf cs    = satisfy (\c -> elem c cs)
noneOf cs   = satisfy (\c -> not (elem c cs))

char :: Char -> RuleParser Char
char c      = satisfy (==c)  <?> show [c]

hexDigit, octDigit, digit, upper :: RuleParser Char
hexDigit    = satisfy (isHexDigit)  <?> "hexadecimal digit"
octDigit    = satisfy (isOctDigit)  <?> "octal digit"

digit       = satisfy (isDigit)     <?> "digit"
upper       = satisfy (isUpper)     <?> "uppercase letter"

{-
whiteSpace  = satisfy (\c -> charClassOf c == SpaceClass)
                                    <?> "whitespace"
-}

{-
perl6WhiteSpace :: RuleParser String
perl6WhiteSpace = do
    cls <- getPrevCharClass 
    let mod = if cls == WordClass then many1 else many
    mod whiteSpace <|> (satisfy (\c -> charClassOf c /= WordClass) >> return "")
-}

anyChar :: RuleParser Char
anyChar     = satisfy (const True)

{-|
Cache holding dynamically-generated parsers for user-defined operators.  This
means we don't have to rebuild them for each token.

The cache is generated inside 'Pugs.Parser.parseOpWith'.
It is cleared each time we do compile-time evaluation with
'Pugs.Parser.Unsafe.unsafeEvalExp', by calling 'clearDynParsers'.

Stored inside 'RuleState', the state component of 'RuleParser'.
-}
data DynParsers = MkDynParsersEmpty | MkDynParsers
    { dynParseOp       :: !(RuleParser Exp)
    , dynParseTightOp  :: !(RuleParser Exp)
    , dynParseLitOp    :: !(RuleParser Exp)
    , dynParseNullary  :: !(RuleParser Exp)
    , dynParsePrePost  :: !(RuleParser String)
    }

type OuterLevel = Int

{-|
State object that gets passed around during the parsing process.
-}
data RuleState = MkState
    { s_env           :: Env
    , s_parseProgram  :: (Env -> FilePath -> String -> Env)
    , s_dynParsers    :: DynParsers         -- ^ Cache for dynamically-generated
                                            --     parsers
    , s_bracketLevel  :: !BracketLevel      -- ^ The kind of "bracket" we are in
                                            --     part and has to suppress {..} literals
--  , s_char          :: Char               -- ^ What the previous character contains
--  , s_name          :: !ID                -- ^ Capture name
--  , s_pos           :: !Int               -- ^ Capture position
    , s_wsLine        :: !Line              -- ^ Last whitespace position
    , s_wsColumn      :: !Column            -- ^ Last whitespace position
--  , s_blockPads     :: Map Scope Pad      -- ^ Hoisted pad for this block
    , s_knownVars     :: !(Map Var MPad)        -- ^ Map from variables to its associated scope
    , s_outerVars     :: !(Map MPad (Set Var))  -- ^ Map from scopes to vars that must not be declared in it
--  , s_freeVars      :: !(Set (Var, LexPads))  -- ^ Set of free vars and the mpadlist to check with
    , s_protoPad      :: !Pad                   -- ^ Pad that's part of all scopes; used in param init
    , s_closureTraits :: [TraitBlocks -> TraitBlocks]
                                       -- ^ Closure traits: head is this block, tail is all outer blocks
    }

data BracketLevel
    = ConditionalBracket    -- if ... {}
    | StatementBracket      -- ... ; ...
    | ParensBracket         -- (...)
    | QuoteAdverbBracket    -- q...
    deriving (Show, Eq)

{-|
A parser that operates on @Char@s, and maintains state in a 'RuleState'.
-}
type RuleParser = GenParser Char RuleState

data CharClass = WordClass | SpaceClass | SymClass
    deriving (Show, Eq)

data ParensOption = ParensMandatory | ParensOptional
    deriving (Show, Eq)

data FormalsOption = FormalsSimple | FormalsComplex
    deriving (Show, Eq)

instance MonadSTM RuleParser where
    liftSTM x = return $! unsafePerformSTM x

instance MonadReader Env RuleParser where
    ask = getRuleEnv
    local f action = do
        env     <- getRuleEnv
        putRuleEnv (f env)
        rv      <- action
        env'    <- getRuleEnv
        putRuleEnv env'
            { envPackage = envPackage env
            , envLexical = envLexical env
            }
        return rv

instance MonadState RuleState RuleParser where
    get = getState
    put = setState

type RuleOperator a = Operator Char RuleState a
type RuleOperatorTable a = OperatorTable Char RuleState a

-----------------------------------------------------------
-- Assoc and OperatorTable
-----------------------------------------------------------
data Assoc                = AssocNone
                          | AssocLeft
                          | AssocRight
                          | AssocList
                          | AssocChain
                          deriving (Show)

data Operator t st a      = Infix { op_infix :: (GenParser t st (a -> a -> a)), op_assoc :: Assoc }
                          | Prefix (GenParser t st (a -> a))
                          | Postfix (GenParser t st (a -> a))
                          | InfixList { op_infixList :: (GenParser t st ([a] -> a)), op_assoc ::  Assoc }
                          | OptionalPrefix (GenParser t st (a -> a))
                          | DependentPostfix (a -> GenParser t st a)
                          | Term (GenParser t st a)

type OperatorTable t st a = [[Operator t st a]]

{-|
Retrieve the 'Pugs.AST.Internals.Env' from the current state of the parser.
-}
enterBracketLevel :: BracketLevel -> RuleParser a -> RuleParser a
enterBracketLevel bracket rule = do
    prev <- gets s_bracketLevel
    modify $ \state -> state{ s_bracketLevel = bracket }
    rv <- rule
    modify $ \state -> state{ s_bracketLevel = prev }
    return rv

{-|
Retrieve the 'Pugs.AST.Internals.Env' from the current state of the parser.
-}
getRuleEnv :: RuleParser Env
getRuleEnv = gets s_env

{-|
Update the 'Pugs.AST.Internals.Env' in the parser's state by applying a transformation function.
-}
modifyRuleEnv :: (Env -> Env) -> RuleParser ()
modifyRuleEnv f = modify $ \state -> state{ s_env = f (s_env state) }

{-|
Update the 's_blockPads' in the parser's state by applying a transformation function.
-}
addBlockPad :: Pad -> RuleParser ()
addBlockPad pad = do
    -- To add a Pad to the COMPILING block, we do two things:
    -- First, we check that our pad does not contain shadowed OUTER symbols.
    -- XXX TODO: it should be fine for two identical padEntry to shadow each other,
    --     as is the case with { our multi f () {}; { &f(); our multi f ($x) {} } }.
    state <- get
    let myVars          = padKeys pad
        dupVars         = case Map.lookup compPad (s_outerVars state) of
            Just vars   -> myVars `Set.intersection` vars
            _           -> Set.empty
        Just compPad    = envCompPad (s_env state)

    -- traceM ("Checking: " ++ show (myVars, s_outerVars state))
    unless (Set.null dupVars) $ do
        fail $ "Redeclaration of "
            ++ unwords (map show (Set.elems dupVars))
            ++ " conflicts with earlier OUTER references in the same scope"

    -- Then we merge the Pad into COMPILING, and add those vars into s_knownVars.
    ()  <- stm $ appendMPad compPad pad

    let myKnownVars = Map.fromDistinctAscList [ (var, compPad) | var <- Set.toAscList myVars ]
    put state{ s_knownVars = s_knownVars state `Map.union` myKnownVars }

popClosureTrait :: RuleParser ()
popClosureTrait = do
    modify $ \state -> state
        { s_closureTraits = case s_closureTraits state of
            []      -> [id]
            [_]     -> [id]
            (_:fs)  -> fs
        }

addClosureTrait :: String -> VCode -> RuleParser ()
addClosureTrait name code = do
    let names = words " ENTER LEAVE KEEP UNDO FIRST NEXT LAST PRE POST CATCH CONTROL "
    when (not $ name `elem` names) $
        fail ("Invalid closure trait: " ++ name) 
    modify $ \state -> state
        { s_closureTraits = case s_closureTraits state of
            []      -> [addTrait]
            (f:fs)  -> ((addTrait . f) : fs)
        }
    where
    trait = code{ subName = cast name }
    addTrait block = case name of 
        "CONTROL"   -> block{ subControlBlocks = trait:subControlBlocks block }
        "CATCH"     -> block{ subCatchBlocks = trait:subCatchBlocks block }
        "KEEP"      -> block
            { subKeepBlocks     = trait:subKeepBlocks block
            , subLeaveBlocks    = trait:subLeaveBlocks block
            }
        "UNDO"      -> block
            { subUndoBlocks     = trait:subUndoBlocks block
            , subLeaveBlocks    = trait:subLeaveBlocks block
            }
        "ENTER"     -> block{ subEnterBlocks = subEnterBlocks block ++ [trait] }
        "LEAVE"     -> block{ subLeaveBlocks = trait:subLeaveBlocks block }
        "NEXT"      -> block{ subNextBlocks = trait:subNextBlocks block }
        "LAST"      -> block{ subLastBlocks = trait:subLastBlocks block }
        "PRE"       -> block{ subPreBlocks = subPreBlocks block ++ [trait] }
        "POST"      -> block{ subPostBlocks = trait:subPostBlocks block }
        "FIRST"     -> block{ subFirstBlocks = subFirstBlocks block ++ [trait] }
        _           -> trace ("Wrong closure trait name: "++name) block

{-|
Replace the 'Pugs.AST.Internals.Env' in the parser's state with a new one.
-}
putRuleEnv :: Env -> RuleParser ()
putRuleEnv = modifyRuleEnv . const

{-|
Clear the parser's cache of dynamically-generated parsers for user-defined
operators.

These will be re-generated by 'Pugs.Parser.parseOpWith' when needed.
-}
clearDynParsers :: RuleParser ()
clearDynParsers = modify $ \state -> state{ s_dynParsers = MkDynParsersEmpty }

parserWarn :: (Typeable a, Show a) => String -> a -> RuleParser ()
parserWarn str val = do
    currPos <- getPosition
    traceM (pretty (VError (VStr $ str ++ showVal) [mkPos currPos currPos]))
    where
    showVal = case show val of
        "()" -> ""
        txt  -> ":\n    " ++ txt


{-|
Create a Pugs 'Pugs.AST.Pos' (for storing in the AST) from two Parsec
@SourcePos@ positions, being the start and end respectively of the current
region.
-}
mkPos :: SourcePos -- ^ Starting position of the region
      -> SourcePos -- ^ Ending position of the region
      -> Pos
mkPos pos1 pos2 = MkPos
    { posName         = __(sourceName pos1)
    , posBeginLine    = sourceLine pos1
    , posBeginColumn  = sourceColumn pos1
    , posEndLine      = sourceLine pos2
    , posEndColumn    = sourceColumn pos2
    }

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
Modify the input stream by inserting a 'String' as the next thing to parse.
-}
insertIntoPosition :: String -> RuleParser ()
insertIntoPosition str = do
    currPos <- getPosition
    input   <- getInput 
    setInput (str ++ input)
    setPosition (setSourceColumn currPos (sourceColumn currPos - length str))
