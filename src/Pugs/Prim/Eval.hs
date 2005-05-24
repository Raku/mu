module Pugs.Prim.Eval (
    -- used by Pugs.Prim
    op1EvalHaskell,
    opEval,
    opRequire,
    -- used by Pugs.Eval -- needs factored somewhere bettwen
    retEvalResult, 
) where
import Pugs.AST
import Pugs.Parser.Program
import Pugs.Embed
import Pugs.Internals

opRequire :: Bool -> Val -> Eval Val
opRequire dumpEnv v = do
    file    <- fromVal v
    incs    <- fromVal =<< readVar "@*INC"
    requireInc incs file (errMsg file incs)
    where
    errMsg file incs = "Can't locate " ++ file ++ " in @INC (@INC contains: " ++ unwords incs ++ ")."
    requireInc [] _ msg = fail msg
    requireInc (p:ps) file msg = do
        let pathName = p ++ "/" ++ file
        ok <- liftIO $ doesFileExist pathName
        if (not ok)
            then requireInc ps file msg
            else do
                str <- liftIO $ readFile pathName
                opEval (Just dumpEnv) pathName (decodeUTF8 str)

op1EvalHaskell :: Val -> Eval Val
op1EvalHaskell cv = do
    str     <- fromVal cv
    val     <- resetT $ evalHaskell str
    retEvalResult False val

opEval :: Maybe Bool -> String -> String -> Eval Val
opEval flag name str = do
    env <- ask
    let env' = runRule env id ruleProgram name str
        trans | flag == Just True = (`mergeStmts` Syn "env" [])
              | otherwise         = id
    val <- resetT $ local (const env') $ do
        evl <- asks envEval
        evl (trans $ envBody env')
    retEvalResult (maybe False id flag) val

retEvalResult :: Bool -> Val -> Eval Val
retEvalResult external val = do
    glob <- askGlobal
    errSV <- findSymRef "$!" glob
    case val of
        VError str _ | not external  -> do
            writeRef errSV (VStr str)
            retEmpty
        _ -> do
            writeRef errSV VUndef
            return val

