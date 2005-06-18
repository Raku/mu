module Pugs.Prim.Eval (
    -- used by Pugs.Prim
    op1EvalHaskell,
    opEval, opEvalfile,
    opRequire,
    EvalError(..), EvalResult(..), EvalStyle(..),
    -- used by Pugs.Eval -- needs factored somewhere bettwen
    retEvalResult, 
) where
import Pugs.AST
import Pugs.Parser.Program
import Pugs.Embed
import Pugs.Internals
import Pugs.Pretty

data EvalError = EvalErrorFatal
               | EvalErrorUndef
               deriving Eq
data EvalResult = EvalResultLastValue
                | EvalResultModule
                | EvalResultEnv
                deriving Eq
data EvalStyle = MkEvalStyle
               { evalError  :: EvalError
               , evalResult :: EvalResult
               }

opRequire :: Bool -> Val -> Eval Val
opRequire dumpEnv v = do
    file    <- fromVal v
    incs    <- fromVal =<< readVar "@*INC"
    requireInc incs file (errMsg file incs)
    where
    style = MkEvalStyle
        { evalError  = EvalErrorFatal
        , evalResult = (if dumpEnv == True then EvalResultEnv
                                           else EvalResultLastValue)}
    errMsg file incs = "Can't locate " ++ file ++ " in @*INC (@*INC contains: " ++ unwords incs ++ ")."
    requireInc [] _ msg = fail msg
    requireInc (p:ps) file msg = do
        let pathName = p ++ "/" ++ file
        ok <- liftIO $ doesFileExist pathName
        if (not ok)
            then requireInc ps file msg
            else do
                -- XXX security issue?  only if you _can_ create a filename,
                -- but _cant_ control its contents.  Yes?
                opEval style "<internal>" ("%*INC{q{"
                                           ++ (decodeUTF8 file)
                                           ++ "}} = q{"
                                           ++ (decodeUTF8 pathName) ++ "};")
                str <- liftIO $ readFile pathName
                opEval style pathName (decodeUTF8 str)

opEvalfile :: String -> Eval Val
opEvalfile filename = do
    ok <- liftIO $ doesFileExist filename
    if (not ok)
        then fail $ "Can't locate " ++ filename ++ "."
        else do
            contents <- liftIO $ readFile filename
            opEval style filename $ decodeUTF8 contents
    where
    style = MkEvalStyle{ evalError=EvalErrorUndef
                       , evalResult=EvalResultLastValue}

op1EvalHaskell :: Val -> Eval Val
op1EvalHaskell cv = do
    str     <- fromVal cv
    val     <- resetT $ evalHaskell str
    retEvalResult style val
    where
    style = MkEvalStyle{ evalError=EvalErrorUndef
                       , evalResult=EvalResultLastValue}

opEval :: EvalStyle -> FilePath -> String -> Eval Val
opEval style path str = do
    env <- ask
    let env' = parseProgram env path str
        trans = case evalResult style of
            EvalResultEnv -> (`mergeStmts` Syn "env" [])
            _             -> id
    val <- resetT $ local (const env') $ do
        evl <- asks envEval
        evl (trans $ envBody env')
    retEvalResult style val

retEvalResult :: EvalStyle -> Val -> Eval Val
retEvalResult style val = do
    glob <- askGlobal
    errSV <- findSymRef "$!" glob
    case val of
        err@(VError str _) -> do
            writeRef errSV (VStr str)
            when (evalError style == EvalErrorFatal) $ do
                liftIO $ fail $ pretty err
            retEmpty
        _ -> do
            writeRef errSV VUndef
            return val

