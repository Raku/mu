{-# OPTIONS_GHC -fglasgow-exts -funbox-strict-fields #-}

module Pugs.Parser.Types (
    RuleParser, RuleState(..), CharClass(..),
    DynParsers(..), ParensOption(..), FormalsOption(..), BracketLevel(..),
    RuleOperator, RuleOperatorTable,
    getRuleEnv, modifyRuleEnv, putRuleEnv, insertIntoPosition,
    clearDynParsers, enterBracketLevel, getCurrCharClass, getPrevCharClass, charClassOf,
    addBlockPad, addClosureTrait, addOuterVar,
    -- Alternate Char implementations that keeps track of s_charClass
    satisfy, string, oneOf, noneOf, char, hexDigit, octDigit,
    digit, upper, anyChar, perl6WhiteSpace,

    Operator(..), Assoc(..),
) where
import Pugs.AST
import Pugs.Rule
import Pugs.Types
import Pugs.Internals
import Text.ParserCombinators.Parsec.Pos
import Debug.Trace
import qualified Data.Map as Map
import qualified Data.Set as Set

satisfy :: (Char -> Bool) -> RuleParser Char
satisfy f = tokenPrimEx
    (\c -> show [c]) 
    (\pos c _ -> updatePosChar pos c) 
    (Just (\_ c _ state -> state{ s_char = c }))
    (\c -> if f c then Just c else Nothing)

string :: String -> RuleParser String
string s = do
    tokens show updatePosString s
    let lastCh = last s
    modify (\state -> state{ s_char = lastCh })
    return (lastCh `seq` s)

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

charClassOf :: Char -> CharClass
charClassOf c   | isAlphaNum c  = WordClass
                | isSpace c     = SpaceClass
                | '_' <- c      = WordClass
                | otherwise     = SymClass

getCurrCharClass :: RuleParser CharClass
getCurrCharClass = fmap charClassOf (lookAhead anyToken) <|> return SpaceClass

getPrevCharClass :: RuleParser CharClass
getPrevCharClass = do
    c <- gets s_char
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
    , dynParsePrePost  :: !(RuleParser String)
    }

{-|
State object that gets passed around during the parsing process.
-}
data RuleState = MkState
    { s_env           :: Env
    , s_parseProgram  :: (Env -> FilePath -> String -> Env)
    , s_dynParsers    :: DynParsers     -- ^ Cache for dynamically-generated
                                        --     parsers
    , s_bracketLevel  :: !BracketLevel  -- ^ The kind of "bracket" we are in
                                        --     part and has to suppress {..} literals
    , s_char          :: !Char          -- ^ What the previous character contains
    , s_name          :: !ID            -- ^ Capture name
    , s_pos           :: !Int           -- ^ Capture position
    , s_blockPads     :: Map Scope Pad  -- ^ Hoisted pad for this block
    , s_outerVars     :: Set Var        -- ^ OUTER symbols we remembers
                                       
    , s_closureTraits :: [Exp->Maybe Exp] 
                                       -- ^ Closure traits for this block 
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
addBlockPad :: Scope -> Pad -> RuleParser ()
addBlockPad scope pad = do
    -- First we check that our pad does not contain shadows OUTER symbols.
    state <- get
    let dupSyms = padKeys pad `Set.intersection` s_outerVars state
    unless (Set.null dupSyms) $ do
        unexpected $ "redeclaration of "
            ++ unwords (map show (Set.elems dupSyms))
            ++ " conflicts with earlier OUTER references in the same scope"
    put state{ s_blockPads = Map.insertWith unionPads scope pad (s_blockPads state) }

addClosureTrait :: String -> VCode -> RuleParser ()
addClosureTrait name trait = do
  let names = words " ENTER LEAVE KEEP UNDO FIRST NEXT LAST PRE POST CATCH CONTROL "
  when (not $ name `elem` names) $
       fail ("Invalid closure trait: " ++ name) 
  modify $ \state -> state{s_closureTraits = addTrait : s_closureTraits state }
    where
      addTrait (Val (VCode block)) = 
          Just . Val . VCode $ case name of 
            "CONTROL" -> block{ subControlBlocks = trait:subControlBlocks block }
            "CATCH" -> block{ subCatchBlocks = trait:subCatchBlocks block }
            "KEEP" -> block{ subKeepBlocks = trait:subKeepBlocks block }
            "UNDO" -> block{ subUndoBlocks = trait:subUndoBlocks block }
            "ENTER" -> block{ subEnterBlocks = trait:subEnterBlocks block }
            "LEAVE" -> block{ subLeaveBlocks = trait:subLeaveBlocks block }
            "NEXT" -> block{ subNextBlocks = trait:subNextBlocks block }
            "LAST" -> block{ subLastBlocks = trait:subLastBlocks block }
            "PRE" -> trace "PRE case" block{ subPreBlocks = trait:subPreBlocks block }
            "POST" -> block{ subPostBlocks = trait:subPostBlocks block }
            "FIRST" -> block{ subFirstBlocks = trait:subFirstBlocks block }
            _ -> trace ("Wrong name "++name) block
      addTrait (Ann f x)= liftM (Ann f) $ addTrait x -- XXX Might be done better...
      addTrait (Stmts x Noop) = addTrait x
      addTrait (Stmts Noop x) = addTrait x
      addTrait (Syn typ [x]) = case typ of 
                                   "sub" -> liftM ((Syn typ) . (:[])) $ addTrait x
                                   "block" -> liftM ((Syn typ) . (:[])) $ addTrait x
                                   _ ->       Nothing
      addTrait _ = Nothing
{-|
Update the 's_outerVars' in the parser's state by applying a transformation function.
-}
addOuterVar :: Var -> RuleParser ()
addOuterVar var = modify $ \state ->
    state{ s_outerVars = Set.insert var (s_outerVars state) }

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

{-|
Modify the input stream by inserting a single 'Char' as the next thing to parse.
-}
insertIntoPosition :: Char -> RuleParser ()
insertIntoPosition ch = do
    currPos <- getPosition
    input <- getInput 
    setInput (ch:input)
    setPosition (setSourceColumn currPos (sourceColumn currPos - 1))
