module Pugs.Parser where
import Pugs.AST
import Pugs.Parser.Types
import Pugs.Types

parseTerm :: RuleParser Exp
parseNoParenArgList :: RuleParser (Maybe Exp, [Exp])

ruleExpression :: RuleParser Exp
ruleSubName :: RuleParser String
ruleArraySubscript :: RuleParser (Exp -> Exp)
ruleHashSubscript :: RuleParser (Exp -> Exp)
ruleCodeSubscript :: RuleParser (Exp -> Exp)
ruleInvocationParens :: RuleParser (Exp -> Exp)
verbatimVarNameString :: RuleParser String
retInterpolatedBlock :: BlockInfo -> RuleParser Exp
ruleVerbatimBlock :: RuleParser BlockInfo
ruleBlockLiteral :: RuleParser Exp
ruleDoBlock :: RuleParser Exp
ruleNamedMethodCall :: RuleParser (Maybe Char, String)
ruleSigil :: RuleParser VarSigil

regularVarName :: RuleParser String
regularVarNameForSigil :: VarSigil -> RuleParser String
