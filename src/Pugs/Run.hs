{-# OPTIONS_GHC -fglasgow-exts -cpp #-}

{-|
    Runtime engine.

>   The mountain throne once more is freed!
>   O! Wandering folk, the summons heed!
>   Come haste! Come haste! Across the waste!
>   The king of friend and kin has need...
-}

module Pugs.Run where
import Pugs.Run.Args
import Pugs.Run.Perl5 ()
import Pugs.Internals
import Pugs.Config
import Pugs.AST
import Pugs.Types
import Pugs.Eval
import Pugs.Prim
import Pugs.Embed
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
    autoSV  <- newScalar undef
#if defined(PUGS_HAVE_HSPLUGINS)
    hspluginsSV <- newScalar (VInt 1)
#else
    hspluginsSV <- newScalar (VInt 0)
#endif
    let subExit = \x -> case x of
            [x] -> op1Exit x     -- needs refactoring (out of Prim)
            _   -> op1Exit undef
    env <- emptyEnv name $
        [ genSym "@*ARGS"       $ MkRef argsAV
        , genSym "@*INC"        $ MkRef incAV
        , genSym "$*PUGS_HAS_HSPLUGINS" $ MkRef hspluginsSV
        , genSym "$*EXECUTABLE_NAME"    $ MkRef execSV
        , genSym "$*PROGRAM_NAME"       $ MkRef progSV
        , genSym "$*PID"        $ MkRef pidSV
        -- XXX these four need a proper `set' magic
        , genSym "$*UID"        $ MkRef uidSV
        , genSym "$*EUID"       $ MkRef euidSV
        , genSym "$*GID"        $ MkRef gidSV
        , genSym "$*EGID"       $ MkRef egidSV
        , genSym "@?CHECK"      $ MkRef checkAV
        , genSym "@?INIT"       $ MkRef initAV
        , genSym "@*END"        $ MkRef endAV
        , genSym "$*IN"         $ MkRef inGV
        , genSym "$*OUT"        $ MkRef outGV
        , genSym "$*ERR"        $ MkRef errGV
        , genSym "$*ARGS"       $ MkRef argsGV
        , genSym "$!"           $ MkRef errSV
        , genSym "$/"           $ MkRef matchAV
        , genSym "%*ENV"        $ hashRef MkHashEnv
        , genSym "$*CWD"        $ scalarRef MkScalarCwd
        -- XXX What would this even do?
        -- , genSym "%=POD"        (Val . VHash $ emptyHV)
        , genSym "@=POD"        $ MkRef $ constArray []
        , genSym "$=POD"        $ MkRef $ constScalar (VStr "")
        , genSym "$*OS"         $ MkRef $ constScalar (VStr $ getConfig "osname")
        , genSym "&?BLOCK_EXIT" $ codeRef $ mkPrim
            { subName = "&?BLOCK_EXIT"
            , subBody = Prim subExit
            }
        , genSym "%?CONFIG" $ hashRef confHV
        , genSym "$*_" $ MkRef defSV
        , genSym "$*AUTOLOAD" $ MkRef autoSV
        ]
    initPerl5 "" (Just . VControl $ ControlEnv env)
    return env

{-|
Combine @%*ENV\<PERL6LIB\>@, -I, 'Pugs.Config.config' values and \".\" into
the @\@*INC@ list for 'Main.printConfigInfo'
-}
getLibs :: IO [String]
getLibs = do
    args    <- getArgs
    p6lib   <- (getEnv "PERL6LIB") >>= (return . (fromMaybe ""))
    return $ filter (not . null) (libs p6lib $ canonicalArgs args)
    where
    -- broken, need real parser
    inclibs ("-I":dir:rest) = [dir] ++ inclibs(rest)
    inclibs (_:rest)        = inclibs(rest)
    inclibs ([])            = []

    libs p6lib args =  (inclibs args)
              ++ (split (getConfig "path_sep") p6lib)
              ++ [ getConfig "archlib"
                 , getConfig "privlib"
                 , getConfig "sitearch"
                 , getConfig "sitelib"
                 ]
              ++ [ "." ]

