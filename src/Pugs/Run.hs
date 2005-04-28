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

runEnv :: Env -> IO Val
runEnv env = runEval env $ evaluateMain (envBody env)

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

runComp :: Eval Val -> IO Val
runComp comp = do
    hSetBuffering stdout NoBuffering
    name <- getProgName
    args <- getArgs
    env  <- prepareEnv name args
    runEval env{ envDebug = Nothing } comp

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
    emptyEnv $
        posSyms (SourcePos name 1 1) ++
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
        , genSym "@*END"        $ MkRef endAV
        , genSym "$*IN"         $ MkRef inGV
        , genSym "$*OUT"        $ MkRef outGV
        , genSym "$*ERR"        $ MkRef errGV
        , genSym "$*ARGS"       $ MkRef argsGV
        , genSym "$!"           $ MkRef errSV
        , genSym "$/"           $ MkRef matchAV
        , genSym "%*ENV"        $ hashRef (undefined :: IHashEnv)
        , genSym "$*CWD"        $ scalarRef (undefined :: IScalarCwd)
        -- XXX What would this even do?
        -- , genSym "%=POD"        (Val . VHash $ emptyHV)
        , genSym "@=POD"        $ MkRef $ constArray []
        , genSym "$=POD"        $ MkRef $ constScalar (VStr "")
        , genSym "$?OS"         $ MkRef $ constScalar (VStr $ getConfig "osname")
        , genSym "$*OS"         $ MkRef $ constScalar (VStr $ getConfig "osname")
        , genSym "$?MODULE"     $ MkRef modSV
        , genSym "&?BLOCK_EXIT" $ codeRef $ mkPrim
            { subName = "&?BLOCK_EXIT"
            , subBody = Prim subExit
            }
        , genSym "%?CONFIG" $ hashRef confHV
        , genSym "$*_" $ MkRef defSV
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

