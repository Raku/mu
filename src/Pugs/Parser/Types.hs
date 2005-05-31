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

newtype RuleState = RuleState Env

type RuleParser a = GenParser Char RuleState a
data ParensOption = ParensMandatory | ParensOptional
    deriving (Show, Eq)

type RuleOperator a = Operator Char RuleState a
type RuleOperatorTable a = OperatorTable Char RuleState a

getRuleEnv :: RuleParser Env
getRuleEnv = do { (RuleState env) <- getState; return env }

setRuleEnv :: Env -> RuleParser ()
setRuleEnv env = setState (RuleState env)

