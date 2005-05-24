-- Before you would import Pugs.Parser, now you import Pugs.Parser.Program.
module Pugs.Parser.Program (
    runRule,
    ruleProgram,
) where
import Pugs.Internals
import Pugs.AST
import Pugs.Lexer
import Pugs.Rule
import Pugs.Rule.Error

import Pugs.Parser
import Pugs.Parser.Unsafe

runRule :: Env -> (Env -> a) -> RuleParser Env -> FilePath -> String -> a
runRule env f p name str = f $ case ( runParser p env name str ) of
    Left err    -> env { envBody = Val $ VError msg [mkPos pos pos] }
        where
        pos = errorPos err
        msg = showErr err
    Right env'  -> env'

showErr :: ParseError -> String
showErr err =
      showErrorMessages "or" "unknown parse error"
                        "expecting" "unexpected" "end of input"
                       (errorMessages err)

-- Lexical units --------------------------------------------------

ruleProgram :: RuleParser Env
ruleProgram = rule "program" $ do
    env <- getState
    statements <- ruleBlockBody
    -- error $ show statements
    eof
    -- S04: CHECK {...}*      at compile time, ALAP
    --  $_() for @?CHECK
    rv <- unsafeEvalExp $ Syn "for"
	[ Var "@?CHECK"
	, Syn "sub"
	    [ Val . VCode $ mkSub
		{ subBody   = App (Var "$_") [] []
		, subParams = [defaultScalarParam]
		}
	    ]
	]
    -- If there was a exit() in a CHECK block, we've to exit.
    possiblyExit rv
    env' <- getState
    return $ env'
        { envBody       = mergeStmts emptyExp statements
        , envStash      = ""
        , envPackage    = envPackage env
        }

