module Pugs.Prim.Eval (
    -- used by Pugs.Prim
    op1EvalHaskell,
    opEval, opEvalFile,
    opRequire, requireInc,
    EvalError(..), EvalResult(..), EvalStyle(..),
    -- used by Pugs.Eval -- needs factored somewhere bettwen
    retEvalResult, getLibs,
) where
import Pugs.AST
import Pugs.Parser.Program
import Pugs.Embed
import Pugs.Monads
import Pugs.Internals
import Pugs.Pretty
import Pugs.Config
import Pugs.Run.Args
import Pugs.Prim.Keyed

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
    file        <- fromVal v
    incs        <- fromVal =<< readVar "@*INC"
    glob        <- askGlobal
    seen        <- findSymRef "%*INC" glob
    loaded      <- existsFromRef seen v
    pathName    <- requireInc incs file (errMsg file incs)
    if loaded then opEval style pathName "" else do
        -- %*INC{file} = pathname
        evalExp $ Syn "="
            [ Syn "{}"
                [ Var "%*INC", Val . VStr $ decodeUTF8 file ]
                , Val . VStr $ decodeUTF8 pathName
            ]
        str         <- liftIO $ readFile pathName
        opEval style pathName (decodeUTF8 str)
    where
    style = MkEvalStyle
        { evalError  = EvalErrorFatal
        , evalResult = (if dumpEnv == True then EvalResultEnv
                                           else EvalResultLastValue)}
    errMsg file incs = "Can't locate " ++ file ++ " in @*INC (@*INC contains: " ++ unwords incs ++ ")."

requireInc :: (MonadIO m) => [FilePath] -> FilePath -> String -> m String 
requireInc [] _ msg = fail msg
requireInc (p:ps) file msg = do
    let pathName = p ++ "/" ++ file
    ok <- liftIO $ doesFileExist pathName
    if (not ok)
        then requireInc ps file msg
        else return pathName

opEvalFile :: String -> Eval Val
opEvalFile filename = do
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
opEval style path str = enterCaller $ do
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

{-|
Combine @%*ENV\<PERL6LIB\>@, -I, 'Pugs.Config.config' values and \".\" into the
@\@*INC@ list for 'Main.printConfigInfo'. If @%*ENV\<PERL6LIB\>@ is not set,
@%*ENV\<PERLLIB\>@ is used instead.
-}
getLibs :: IO [String]
getLibs = do
    args    <- getArgs
    p6lib   <- (getEnv "PERL6LIB") >>= (return . (fromMaybe ""))
    plib    <- (getEnv "PERLLIB")  >>= (return . (fromMaybe ""))
    let lib = if (p6lib == "") then plib else p6lib
    return $ filter (not . null) (libs lib $ canonicalArgs args)
    where
    -- broken, need real parser
    inclibs ("-I":dir:rest) = [dir] ++ inclibs(rest)
    inclibs (_:rest)        = inclibs(rest)
    inclibs ([])            = []
    libs p6lib args = (inclibs args)
              ++ (split (getConfig "path_sep") p6lib)
              ++ [ getConfig "archlib"
                 , getConfig "privlib"
                 , getConfig "sitearch"
                 , getConfig "sitelib"
                 ]
              ++ [ "." ]

