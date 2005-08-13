-- stub module to add your own rules.
module UserRules(userRules) where

import RuleUtils(RuleDef) -- gives some examples 

import UserRuleBinary
import UserRuleXml
import UserRulesGeneric
import UserRuleGhcBinary
import qualified RulePerl5
import qualified RuleUtility 
import qualified RuleFunctorM
import qualified RuleMonoid


-- add your rules to this list
userRules :: [RuleDef]
userRules = userRulesXml ++ userRulesBinary ++ userRulesGeneric ++ userRulesGhcBinary  ++ RuleUtility.rules ++ RuleFunctorM.rules ++ RuleMonoid.rules ++ RulePerl5.rules

