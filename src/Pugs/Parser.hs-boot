module Pugs.Parser where
import Pugs.AST
import Pugs.Parser.Types
import Pugs.Types

parseTerm :: RuleParser Exp
parseNoParenParamList :: RuleParser (Maybe Exp, [Exp])

ruleExpression :: RuleParser Exp
ruleSubName :: RuleParser String
ruleArraySubscript :: RuleParser (Exp -> Exp)
ruleHashSubscript :: RuleParser (Exp -> Exp)
ruleCodeSubscript :: RuleParser (Exp -> Exp)
ruleInvocationParens :: RuleParser (Exp -> Exp)
verbatimVarNameString :: RuleParser String
ruleVerbatimBlock :: RuleParser Exp
ruleBlockLiteral :: RuleParser Exp
ruleDoBlock :: RuleParser Exp
ruleNamedMethodCall :: RuleParser (Maybe Char, String)
ruleSigil :: RuleParser VarSigil

regularVarName :: RuleParser String
regularVarNameForSigil :: VarSigil -> RuleParser String
