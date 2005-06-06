module Pugs.Parser.Types (
    RuleParser, RuleState(..),
    OpParsers(..), ParensOption(..),
    RuleOperator, RuleOperatorTable,

    getRuleEnv, setRuleEnv,
    clearOpParsers,
) where
import Pugs.AST
import Pugs.Rule
import Pugs.Rule.Expr

data OpParsers = MkOpParsersEmpty | MkOpParsers
    { ruleParseOp       :: !(RuleParser Exp)
    , ruleParseTightOp  :: !(RuleParser Exp)
    , ruleParseLitOp    :: !(RuleParser Exp)
    }

data RuleState = MkRuleState
    { ruleEnv           :: !(Env)
    , ruleOpParsers     :: !(OpParsers)
    }

type RuleParser a = GenParser Char RuleState a
data ParensOption = ParensMandatory | ParensOptional
    deriving (Show, Eq)

type RuleOperator a = Operator Char RuleState a
type RuleOperatorTable a = OperatorTable Char RuleState a

getRuleEnv :: RuleParser Env
getRuleEnv = fmap ruleEnv getState

setRuleEnv :: Env -> RuleParser ()
setRuleEnv env = do
    state <- getState
    setState state{ ruleEnv = env }

clearOpParsers :: RuleParser ()
clearOpParsers = do
    state <- getState
    setState state{ ruleOpParsers = MkOpParsersEmpty }
