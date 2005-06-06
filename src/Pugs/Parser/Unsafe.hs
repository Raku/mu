-- possibly this needs to be beside the AST or Eval, not the Parser
module Pugs.Parser.Unsafe (
    unsafeEvalLexDiff,
    unsafeEvalEnv,
    unsafeEvalExp,
) where
import Pugs.Internals
import Pugs.AST
import Pugs.Pretty
import Pugs.Parser.Types

unsafeEvalLexDiff :: Exp -> RuleParser Pad
unsafeEvalLexDiff exp = do
    env  <- getRuleEnv
    putRuleEnv env{ envLexical = mkPad [] }
    env' <- unsafeEvalEnv exp
    putRuleEnv env'{ envLexical = envLexical env' `unionPads` envLexical env }
    return $ envLexical env'

-- XXX: Should these fail instead of error?
unsafeEvalEnv :: Exp -> RuleParser Env
unsafeEvalEnv exp = do
    -- pos <- getPosition
    env <- getRuleEnv
    val <- unsafeEvalExp $ mergeStmts exp (Syn "env" [])
    case val of
        Val (VControl (ControlEnv env')) ->
            return env'{ envDebug = envDebug env }
        _  -> error $ pretty val

unsafeEvalExp :: Exp -> RuleParser Exp
unsafeEvalExp exp = do
    clearDynParsers
    env <- getRuleEnv
    let val = unsafePerformIO $ do
        runEvalIO (env{ envDebug = Nothing }) $ do
            evl <- asks envEval
            evl exp
    case val of
        VError _ _  -> error $ pretty (val :: Val)
        _           -> return $ Val val
