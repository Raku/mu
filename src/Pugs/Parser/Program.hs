-- Before you would import Pugs.Parser, now you import Pugs.Parser.Program.
module Pugs.Parser.Program (
    parseProgram,
) where
import Pugs.Internals
import Pugs.AST

import Pugs.Parser
import Pugs.Rule
import Pugs.Rule.Error

parseProgram :: Env -> FilePath -> String -> Env
parseProgram = flip runRule ruleProgram

makeState :: Env -> RuleState
makeState env = MkRuleState env MkDynParsersEmpty

runRule :: Env -> RuleParser Env -> FilePath -> String -> Env
runRule env p name str =
    case ( runParser p (makeState env) name str ) of
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
    env <- getRuleEnv
    statements <- ruleBlockBody
    -- error $ show statements
    eof
    -- S04: CHECK {...}*      at compile time, ALAP
    --  $_() for @*CHECK
    rv <- unsafeEvalExp $ Syn "for"
        [ Var "@*CHECK"
        , Syn "sub"
            [ Val . VCode $ mkSub
                { subBody   = App (Var "$_") Nothing []
                , subParams = [defaultScalarParam]
                }
            ]
        ]
    -- If there was a exit() in a CHECK block, we have to exit.
    possiblyExit rv
    env' <- getRuleEnv
    return $ env'
        { envBody       = mergeStmts emptyExp statements
        , envPackage    = envPackage env
        }

