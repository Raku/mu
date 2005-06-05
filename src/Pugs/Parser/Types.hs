module Pugs.Parser.Types (
    RuleParser, RuleState(..),
    ParensOption(..),
    RuleOperator, RuleOperatorTable,

    getRuleEnv,
    setRuleEnv,
) where
import Pugs.AST
import Pugs.Rule
import Pugs.Rule.Expr

newtype RuleState = MkRuleState { ruleEnv :: Env }

type RuleParser a = GenParser Char RuleState a
data ParensOption = ParensMandatory | ParensOptional
    deriving (Show, Eq)

type RuleOperator a = Operator Char RuleState a
type RuleOperatorTable a = OperatorTable Char RuleState a

getRuleEnv :: RuleParser Env
getRuleEnv = fmap ruleEnv getState

setRuleEnv :: Env -> RuleParser ()
setRuleEnv env = setState (MkRuleState env)

