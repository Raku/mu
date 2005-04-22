{-# OPTIONS_GHC -fglasgow-exts -cpp #-}

{-
    Runtime engine.

    The mountain throne once more is freed!
    O! Wandering folk, the summons heed!
    Come haste! Come haste! Across the waste!
    The king of friend and kin has need...
-}

module Pugs.Run where
import Pugs.Run.Args
import Pugs.Internals
import Pugs.Config
import Pugs.AST
import Pugs.Types
import Pugs.Eval
import Pugs.Prim
import qualified Data.Map as Map

runWithArgs f = do
    args <- getArgs
    f $ canonicalArgs args

runEval :: Env -> Eval Val -> IO Val
runEval env eval = withSocketsDo $ do
    my_perl <- initPerl5 ""
    val <- (`runReaderT` env) $ do
        (`runContT` return) $
            resetT eval
    freePerl5 my_perl
    return val

runEnv :: Env -> IO Val
runEnv env = runEval env $ evaluateMain (envBody env)

runAST :: Exp -> IO Val
runAST ast = do
    hSetBuffering stdout NoBuffering
    name <- getProgName
    args <- getArgs
    env  <- prepareEnv name args
    runEnv env{ envBody = ast, envDebug = Nothing }

runComp :: Eval Val -> IO Val
runComp comp = do
    hSetBuffering stdout NoBuffering
    name <- getProgName
    args <- getArgs
    env  <- prepareEnv name args
    runEval env{ envDebug = Nothing } comp

prepareEnv :: VStr -> [VStr] -> IO Env
prepareEnv name args = do
    let confHV = [ (k, VStr v) | (k, v) <- Map.toList config ]
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
    modSV   <- newScalar (VStr "main")
    endAV   <- newArray []
    matchAV <- newArray []
    incAV   <- newArray (map VStr libs)
    argsAV  <- newArray (map VStr args)
    inGV    <- newHandle stdin
    outGV   <- newHandle stdout
    errGV   <- newHandle stderr
    argsGV  <- newScalar undef
    errSV   <- newScalar (VStr "")
    defSV   <- newScalar undef
#if defined(PUGS_HAVE_HSPLUGINS)
    hspluginsSV <- newScalar (VInt 1)
#else
    hspluginsSV <- newScalar (VInt 0)
#endif
    let subExit = \x -> case x of
            [x] -> op1 "exit" x
            _   -> op1 "exit" undef
    emptyEnv
        [ MkSym "@*ARGS"       $ MkRef argsAV
        , MkSym "@*INC"        $ MkRef incAV
        , MkSym "$*PUGS_HAS_HSPLUGINS" $ MkRef hspluginsSV
        , MkSym "$*EXECUTABLE_NAME"    $ MkRef execSV
        , MkSym "$*PROGRAM_NAME"       $ MkRef progSV
        , MkSym "$*PID"        $ MkRef pidSV
        -- XXX these four need a proper `set' magic
        , MkSym "$*UID"        $ MkRef uidSV
        , MkSym "$*EUID"       $ MkRef euidSV
        , MkSym "$*GID"        $ MkRef gidSV
        , MkSym "$*EGID"       $ MkRef egidSV
        , MkSym "@*END"        $ MkRef endAV
        , MkSym "$*IN"         $ MkRef inGV
        , MkSym "$*OUT"        $ MkRef outGV
        , MkSym "$*ERR"        $ MkRef errGV
        , MkSym "$*ARGS"       $ MkRef argsGV
        , MkSym "$!"           $ MkRef errSV
        , MkSym "$/"           $ MkRef matchAV
        , MkSym "%*ENV"        $ hashRef (undefined :: IHashEnv)
        , MkSym "$*CWD"        $ scalarRef (undefined :: IScalarCwd)
        -- XXX What would this even do?
        -- , MkSym "%=POD"        (Val . VHash $ emptyHV)
        , MkSym "@=POD"        $ MkRef $ constArray []
        , MkSym "$=POD"        $ MkRef $ constScalar (VStr "")
        , MkSym "$?OS"         $ MkRef $ constScalar (VStr $ getConfig "osname")
        , MkSym "$*OS"         $ MkRef $ constScalar (VStr $ getConfig "osname")
        , MkSym "$?MODULE"     $ MkRef modSV
        , MkSym "&?BLOCK_EXIT" $ codeRef $ mkPrim
            { subName = "&?BLOCK_EXIT"
            , subFun = Prim subExit
            }
        , MkSym "%?CONFIG" $ hashRef confHV
        , MkSym "$_" $ MkRef defSV
        , MkSym "$?FILE" $ MkRef progSV
        ]



getLibs :: IO [String]
getLibs = do
    args    <- getArgs
    p6lib   <- tryIO "" (getEnv "PERL6LIB")
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

