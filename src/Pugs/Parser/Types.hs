{-# OPTIONS_GHC -fglasgow-exts -funbox-strict-fields #-}

module Pugs.Parser.Types (
    RuleParser, RuleState(..), CharClass(..),
    DynParsers(..), ParensOption(..),
    RuleOperator, RuleOperatorTable,
    getRuleEnv, modifyRuleEnv, putRuleEnv,
    clearDynParsers, withRuleConditional, getPrevCharClass, charClassOf,

    -- Alternate Char implementations that keeps track of ruleCharClass
    satisfy, string, oneOf, noneOf, char, hexDigit, octDigit,
    digit, upper, anyChar,
) where
import Pugs.AST
import Pugs.Rule
import Pugs.Rule.Expr
import Pugs.Internals
import Text.ParserCombinators.Parsec.Pos

satisfy :: (Char -> Bool) -> RuleParser Char
satisfy f = do
    rv <- tokenPrim (\c -> show [c]) 
                    (\pos c _ -> updatePosChar pos c) 
                    (\c -> if f c then Just c else Nothing)
    modify $ \state -> state{ ruleChar = rv }
    return rv

string s = do
    rv <- tokens show updatePosString s
    modify $ \state -> state{ ruleChar = last s }
    return rv

captureNamed :: String -> RuleParser a -> RuleParser a
captureNamed newState rule = do
    prev <- gets ruleName
    modify $ \state -> state{ ruleName = newState }
    rv <- rule
    modify $ \state -> state{ ruleName = prev }
    return rv

capturePositioned :: Int -> RuleParser a -> RuleParser a
capturePositioned pos rule = do
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

getPrevCharClass :: RuleParser CharClass
getPrevCharClass = do
    c <- gets ruleChar
    return $ charClassOf c

oneOf, noneOf :: [Char] -> RuleParser Char
oneOf cs    = satisfy (\c -> elem c cs)
noneOf cs   = satisfy (\c -> not (elem c cs))

char :: Char -> RuleParser Char
char c      = satisfy (==c)  <?> show [c]

hexDigit    = satisfy (isHexDigit)  <?> "hexadecimal digit"
octDigit    = satisfy (isOctDigit)  <?> "octal digit"

digit       = satisfy (isDigit)     <?> "digit"
upper       = satisfy (isUpper)     <?> "uppercase letter"

whiteSpace  = satisfy (\c -> charClassOf c == SpaceClass)
                                    <?> "whitespace"

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
    }

{-|
State object that gets passed around during the parsing process.
-}
data RuleState = MkRuleState
    { ruleEnv           :: !Env
    , ruleDynParsers    :: !DynParsers -- ^ Cache for dynamically-generated
                                       --     parsers
    , ruleInConditional :: !Bool       -- ^ Whether we are in an conditional
                                       --     part and has to suppress {..} literals
    , ruleChar          :: !Char       -- ^ What the previous character contains
    , ruleName          :: !String     -- ^ Capture name
    , rulePos           :: !Int        -- ^ Capture position
    }

{-|
A parser that operates on @Char@s, and maintains state in a 'RuleState'.
-}
type RuleParser a = GenParser Char RuleState a

data CharClass = WordClass | SpaceClass | SymClass
    deriving (Show, Eq)

data ParensOption = ParensMandatory | ParensOptional
    deriving (Show, Eq)

instance MonadState RuleState (GenParser Char RuleState) where
    get = getState
    put = setState

type RuleOperator a = Operator Char RuleState a
type RuleOperatorTable a = OperatorTable Char RuleState a

{-|
Retrieve the 'Pugs.AST.Internals.Env' from the current state of the parser.
-}
withRuleConditional :: Bool -> RuleParser a -> RuleParser a
withRuleConditional newState rule = do
    prev <- gets ruleInConditional
    modify $ \state -> state{ ruleInConditional = newState }
    rv <- rule
    modify $ \state -> state{ ruleInConditional = prev }
    return rv

{-|
Retrieve the 'Pugs.AST.Internals.Env' from the current state of the parser.
-}
getRuleEnv :: RuleParser Env
getRuleEnv = gets ruleEnv

{-|
Update the 'Pugs.AST.Internals.Env' in the parser's state by applying a transformation function.
-}
modifyRuleEnv :: (MonadState RuleState m) => (Env -> Env) -> m ()
modifyRuleEnv f = modify $ \state -> state{ ruleEnv = f (ruleEnv state) }

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
