{-# OPTIONS_GHC -fglasgow-exts -funbox-strict-fields #-}

module Pugs.Parser.Types (
    RuleParser, RuleState(..), CharClass(..),
    DynParsers(..), ParensOption(..), FormalsOption(..), BracketLevel(..),
    RuleOperator, RuleOperatorTable,
    getRuleEnv, modifyRuleEnv, putRuleEnv, insertIntoPosition,
    clearDynParsers, enterBracketLevel, getCurrCharClass, getPrevCharClass, charClassOf,
    addBlockPad,
    -- Alternate Char implementations that keeps track of ruleCharClass
    satisfy, string, oneOf, noneOf, char, hexDigit, octDigit,
    digit, upper, anyChar, perl6WhiteSpace,

    Operator(..), Assoc(..),
) where
import Pugs.AST
import Pugs.Rule
import Pugs.Internals
import Text.ParserCombinators.Parsec.Pos
import qualified Data.Map as Map

satisfy :: (Char -> Bool) -> RuleParser Char
satisfy f = do
    rv <- tokenPrim (\c -> show [c]) 
                    (\pos c _ -> updatePosChar pos c) 
                    (\c -> if f c then Just c else Nothing)
    modify $ \state -> state{ ruleChar = rv }
    return rv

string :: String -> RuleParser String
string s = do
    rv <- tokens show updatePosString s
    modify $ \state -> state{ ruleChar = last s }
    return rv

_captureNamed :: String -> RuleParser a -> RuleParser a
_captureNamed newState rule = do
    prev <- gets ruleName
    modify $ \state -> state{ ruleName = newState }
    rv <- rule
    modify $ \state -> state{ ruleName = prev }
    return rv

_capturePositioned :: Int -> RuleParser a -> RuleParser a
_capturePositioned pos rule = do
    prev <- gets rulePos
    modify $ \state -> state{ rulePos = pos }
    rv <- rule
    modify $ \state -> state{ rulePos = prev }
    return rv

charClassOf :: Char -> CharClass
charClassOf c   | isAlphaNum c  = WordClass
                | isSpace c     = SpaceClass
                | '_' <- c      = WordClass
                | otherwise     = SymClass

getCurrCharClass :: RuleParser CharClass
getCurrCharClass = fmap charClassOf (lookAhead anyToken) <|> return SpaceClass

getPrevCharClass :: RuleParser CharClass
getPrevCharClass = do
    c <- gets ruleChar
    return $ charClassOf c

oneOf, noneOf :: [Char] -> RuleParser Char
oneOf cs    = satisfy (\c -> elem c cs)
noneOf cs   = satisfy (\c -> not (elem c cs))

char :: Char -> RuleParser Char
char c      = satisfy (==c)  <?> show [c]

hexDigit, octDigit, digit, upper, whiteSpace :: RuleParser Char
hexDigit    = satisfy (isHexDigit)  <?> "hexadecimal digit"
octDigit    = satisfy (isOctDigit)  <?> "octal digit"

digit       = satisfy (isDigit)     <?> "digit"
upper       = satisfy (isUpper)     <?> "uppercase letter"

whiteSpace  = satisfy (\c -> charClassOf c == SpaceClass)
                                    <?> "whitespace"

perl6WhiteSpace :: RuleParser String
perl6WhiteSpace = do cls <- getPrevCharClass 
		     let mod = if cls == WordClass then many1 else many
		     do mod whiteSpace
		        <|>
		        (satisfy (\c -> charClassOf c /= WordClass) >> return "")

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
    }

{-|
State object that gets passed around during the parsing process.
-}
data RuleState = MkRuleState
    { ruleEnv           :: !Env
    , ruleParseProgram  :: !(Env -> FilePath -> String -> Env)
    , ruleDynParsers    :: !DynParsers -- ^ Cache for dynamically-generated
                                       --     parsers
    , ruleBracketLevel  :: !BracketLevel
                                       -- ^ The kind of "bracket" we are in
                                       --     part and has to suppress {..} literals
    , ruleChar          :: !Char       -- ^ What the previous character contains
    , ruleName          :: !String     -- ^ Capture name
    , rulePos           :: !Int        -- ^ Capture position
    , ruleBlockPads     :: !(Map Scope Pad)
                                       -- ^ Hoisted pad for this block
    }

data BracketLevel
    = ConditionalBracket    -- if ... {}
    | StatementBracket      -- ... ; ...
    | ParensBracket         -- (...)
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

type OperatorTable t st a = [[Operator t st a]]

{-|
Retrieve the 'Pugs.AST.Internals.Env' from the current state of the parser.
-}
enterBracketLevel :: BracketLevel -> RuleParser a -> RuleParser a
enterBracketLevel bracket rule = do
    prev <- gets ruleBracketLevel
    modify $ \state -> state{ ruleBracketLevel = bracket }
    rv <- rule
    modify $ \state -> state{ ruleBracketLevel = prev }
    return rv

{-|
Retrieve the 'Pugs.AST.Internals.Env' from the current state of the parser.
-}
getRuleEnv :: RuleParser Env
getRuleEnv = gets ruleEnv

{-|
Update the 'Pugs.AST.Internals.Env' in the parser's state by applying a transformation function.
-}
modifyRuleEnv :: (Env -> Env) -> RuleParser ()
modifyRuleEnv f = modify $ \state -> state{ ruleEnv = f (ruleEnv state) }

{-|
Update the 'ruleBlockPostProcessor' in the parser's state by applying a transformation function.
-}
addBlockPad :: Scope -> Pad -> RuleParser ()
addBlockPad scope pad = modify $ \state ->
    state{ ruleBlockPads = Map.insertWith unionPads scope pad (ruleBlockPads state) }

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
clearDynParsers = modify $ \state -> state{ ruleDynParsers = MkDynParsersEmpty }

{-|
Modify the input stream by inserting a single 'Char' as the next thing to parse.
-}
insertIntoPosition :: Char -> RuleParser ()
insertIntoPosition ch = do
    currPos <- getPosition
    input <- getInput 
    setInput (ch:input)
    setPosition (setSourceColumn currPos (sourceColumn currPos - 1))
