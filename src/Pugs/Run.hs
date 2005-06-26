{-# OPTIONS_GHC -fglasgow-exts -cpp #-}

{-|
    Runtime engine.

>   The mountain throne once more is freed!
>   O! Wandering folk, the summons heed!
>   Come haste! Come haste! Across the waste!
>   The king of friend and kin has need...
-}

module Pugs.Run (
    runWithArgs,
    prepareEnv, runEnv,
    runAST, runComp,
    getLibs,
) where
import Pugs.Run.Args
import Pugs.Run.Perl5 ()
import Pugs.Internals
import Pugs.Config
import Pugs.Context
import Pugs.AST
import Pugs.Types
import Pugs.Eval
import Pugs.Prim
import Pugs.Prim.Eval
import Pugs.Embed
import Pugs.Prelude
import qualified Data.Map as Map

{-|
Run 'Main.run' with command line args. 

See 'Main.main' and 'Pugs.Run.Args.canonicalArgs'
-}
runWithArgs :: ([String] -> IO t) -> IO t
runWithArgs f = do
    args <- getArgs
    f $ canonicalArgs args

runEvalMain :: Env -> Eval Val -> IO Val
runEvalMain env eval = withSocketsDo $ do
    val     <- runEvalIO env eval
    -- freePerl5 my_perl
    return val

runEnv :: Env -> IO Val
runEnv env = runEvalMain env $ evaluateMain (envBody env)

-- | Run for 'Pugs.Compile.Pugs' backend
runAST :: Pad -> Exp -> IO Val
runAST glob ast = do
    hSetBuffering stdout NoBuffering
    name    <- getProgName
    args    <- getArgs
    env     <- prepareEnv name args
    globRef <- liftSTM $ do
        glob' <- readTVar $ envGlobal env
        newTVar (glob `unionPads` glob')
    runEnv env{ envBody = ast, envGlobal = globRef, envDebug = Nothing }

-- | Run for 'Pugs.Compile.Haskell' backend
runComp :: Eval Val -> IO Val
runComp comp = do
    hSetBuffering stdout NoBuffering
    name <- getProgName
    args <- getArgs
    env  <- prepareEnv name args
    runEvalMain env{ envDebug = Nothing } comp

-- | Initialize globals and install primitives in an 'Env'
prepareEnv :: VStr -> [VStr] -> IO Env
prepareEnv name args = do
    let confHV = Map.map VStr config
    exec    <- getArg0
    libs    <- getLibs
    pid     <- getProcessID
    pidSV   <- newScalar (VInt $ toInteger pid)
    uid     <- getRealUserID
    uidSV   <- newScalar (VInt $ toInteger uid)
    euid    <- getEffectiveUserID
    euidSV  <- newScalar (VInt $ toInteger euid)
    gid     <- getRealGroupID
    gidSV   <- newScalar (VInt $ toInteger gid)
    egid    <- getEffectiveGroupID
    egidSV  <- newScalar (VInt $ toInteger egid)
    execSV  <- newScalar (VStr exec)
    progSV  <- newScalar (VStr name)
    checkAV <- newArray []
    initAV  <- newArray []
    endAV   <- newArray []
    matchAV <- newScalar (VMatch mkMatchFail)
    incAV   <- newArray (map VStr libs)
    argsAV  <- newArray (map VStr args)
    inGV    <- newHandle stdin
    outGV   <- newHandle stdout
    errGV   <- newHandle stderr
    argsGV  <- newScalar undef
    errSV   <- newScalar (VStr "")
    defSV   <- newScalar undef
    pkgSV   <- newScalar (VStr "main")
    autoSV  <- newScalar undef
    classes <- initClassObjects [] initTree
#if defined(PUGS_HAVE_HSPLUGINS)
    hspluginsSV <- newScalar (VInt 1)
#else
    hspluginsSV <- newScalar (VInt 0)
#endif
    let subExit = \x -> case x of
            [x] -> op1Exit x     -- needs refactoring (out of Prim)
            _   -> op1Exit undef
    env <- emptyEnv name $
        [ genSym "@*ARGS"       $ hideInSafemode $ MkRef argsAV
        , genSym "@*INC"        $ hideInSafemode $ MkRef incAV
        , genSym "$*PUGS_HAS_HSPLUGINS" $ hideInSafemode $ MkRef hspluginsSV
        , genSym "$*EXECUTABLE_NAME"    $ hideInSafemode $ MkRef execSV
        , genSym "$*PROGRAM_NAME"       $ hideInSafemode $ MkRef progSV
        , genSym "$*PID"        $ hideInSafemode $ MkRef pidSV
        -- XXX these four need a proper `set' magic
        , genSym "$*UID"        $ hideInSafemode $ MkRef uidSV
        , genSym "$*EUID"       $ hideInSafemode $ MkRef euidSV
        , genSym "$*GID"        $ hideInSafemode $ MkRef gidSV
        , genSym "$*EGID"       $ hideInSafemode $ MkRef egidSV
        , genSym "@?CHECK"      $ MkRef checkAV
        , genSym "@?INIT"       $ MkRef initAV
        , genSym "@*END"        $ MkRef endAV
        , genSym "$*IN"         $ hideInSafemode $ MkRef inGV
        , genSym "$*OUT"        $ hideInSafemode $ MkRef outGV
        , genSym "$*ERR"        $ hideInSafemode $ MkRef errGV
        , genSym "$*ARGS"       $ hideInSafemode $ MkRef argsGV
        , genSym "$!"           $ MkRef errSV
        , genSym "$/"           $ MkRef matchAV
        , genSym "%*ENV"        $ hideInSafemode $ hashRef MkHashEnv
        , genSym "$*CWD"        $ hideInSafemode $ scalarRef MkScalarCwd
        -- XXX What would this even do?
        -- , genSym "%=POD"        (Val . VHash $ emptyHV)
        , genSym "@=POD"        $ MkRef $ constArray []
        , genSym "$=POD"        $ MkRef $ constScalar (VStr "")
        -- To answer the question "what revision does evalbot run on?"
        , genSym "$?PUGS_VERSION" $ MkRef $ constScalar (VStr $ getConfig "pugs_version")
        , genSym "$*OS"         $ hideInSafemode $ MkRef $ constScalar (VStr $ getConfig "osname")
        , genSym "&?BLOCK_EXIT" $ codeRef $ mkPrim
            { subName = "&?BLOCK_EXIT"
            , subBody = Prim subExit
            }
        , genSym "%?CONFIG" $ hideInSafemode $ hashRef confHV
        , genSym "$*_" $ MkRef defSV
        , genSym "$*AUTOLOAD" $ MkRef autoSV
        , genSym "$*PACKAGE" $ MkRef pkgSV
        ] ++ classes
    unless safeMode $ do
        initPerl5 "" (Just . VControl $ ControlEnv env{ envDebug = Nothing })
        return ()
    initPrelude env
    return env
    where
    hideInSafemode x = if safeMode then MkRef $ constScalar undef else x

{-# NOINLINE initPrelude #-}
initPrelude :: Env -> IO ()
initPrelude env = do
    if bypass then return () else do
        -- Display the progress of loading the Prelude, but only in interactive
        -- mode (similar to GHCi):
        -- "Loading Prelude... done."
        let dispProgress = (posName . envPos $ env) == "<interactive>"
        when dispProgress $ putStr "Loading Prelude... "
        runEvalIO env{envDebug = Nothing} $ opEval style "<prelude>" preludeStr
        when dispProgress $ putStrLn "done."
    where
    style = MkEvalStyle{evalResult=EvalResultModule
                       ,evalError =EvalErrorFatal}
    bypass = case (unsafePerformIO $ getEnv "PUGS_BYPASS_PRELUDE") of
        Nothing     -> False
        Just ""     -> False
        Just "0"    -> False
        _           -> True

initClassObjects :: [Type] -> ClassTree -> IO [STM (Pad -> Pad)]
initClassObjects parent (Node typ children) = do
    obj     <- createObject (mkType "Class") $
        [ ("name",   castV $ showType typ)
        , ("traits", castV $ map showType parent)
        ]
    objSV   <- newScalar (VObject obj)
    rest    <- mapM (initClassObjects [typ]) children
    let sym = genSym (':':'*':showType typ) $ MkRef objSV
    return (sym:concat rest)

