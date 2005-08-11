{-# OPTIONS_GHC -fglasgow-exts -funbox-strict-fields #-}

module Pugs.Parser.Types (
    RuleParser, RuleState(..),
    DynParsers(..), ParensOption(..),
    RuleOperator, RuleOperatorTable,
    getRuleEnv, modifyRuleEnv, putRuleEnv,
    clearDynParsers,
) where
import Pugs.AST
import Pugs.Rule
import Pugs.Rule.Expr
import Pugs.Internals

data DynParsers = MkDynParsersEmpty | MkDynParsers
    { dynParseOp       :: !(RuleParser Exp)
    , dynParseTightOp  :: !(RuleParser Exp)
    , dynParseLitOp    :: !(RuleParser Exp)
    }

data RuleState = MkRuleState
    { ruleEnv           :: !Env
    , ruleDynParsers    :: !DynParsers
    }

type RuleParser a = GenParser Char RuleState a
data ParensOption = ParensMandatory | ParensOptional
    deriving (Show, Eq)

instance MonadState RuleState (GenParser Char RuleState) where
    get = getState
    put = setState

type RuleOperator a = Operator Char RuleState a
type RuleOperatorTable a = OperatorTable Char RuleState a

getRuleEnv :: RuleParser Env
getRuleEnv = gets ruleEnv

modifyRuleEnv :: (MonadState RuleState m) => (Env -> Env) -> m ()
modifyRuleEnv f = modify $ \state -> state{ ruleEnv = f (ruleEnv state) }

putRuleEnv :: Env -> RuleParser ()
putRuleEnv = modifyRuleEnv . const

clearDynParsers :: RuleParser ()
clearDynParsers = modify $ \state -> state{ ruleDynParsers = MkDynParsersEmpty }
