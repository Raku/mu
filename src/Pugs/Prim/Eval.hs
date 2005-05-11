module Pugs.Prim.Eval (
    -- used by Pugs.Prim
    op1EvalHaskell,
    opEval,
    -- used by Pugs.Eval -- needs factored somewhere bettwen
    retEvalResult, 
) where
import Control.Monad.Reader
import Pugs.AST
import Pugs.Parser
import Pugs.Embed

op1EvalHaskell :: Val -> Eval Val
op1EvalHaskell cv = do
    str     <- fromVal cv :: Eval String
    ret     <- liftIO (evalHaskell str)
    glob    <- askGlobal
    errSV   <- findSymRef "$!" glob
    case ret of
        Right str -> do
            writeRef errSV VUndef
            return $ VStr str
        Left  err -> do
            writeRef errSV (VStr err)
            retEmpty

opEval :: Bool -> String -> String -> Eval Val
opEval fatal name str = do
    env <- ask
    let env' = runRule env id ruleProgram name str
    val <- resetT $ local (const env') $ do
        evl <- asks envEval
        evl (envBody env')
    retEvalResult fatal val

retEvalResult :: Bool -> Val -> Eval Val
retEvalResult fatal val = do
    glob <- askGlobal
    errSV <- findSymRef "$!" glob
    case val of
        VError str _ | not fatal  -> do
            writeRef errSV (VStr str)
            retEmpty
        _ -> do
            writeRef errSV VUndef
            return val

