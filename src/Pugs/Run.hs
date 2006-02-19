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
    -- mutable global storage
    _BypassPreludePC, _GlobalFinalizer,
) where
import Pugs.Run.Args
import Pugs.Run.Perl5 ()
import Pugs.Internals
import Pugs.Config
import Pugs.AST
import Pugs.Types
import Pugs.Eval
import Pugs.Prim
import Pugs.Embed
import Data.IORef
import System.FilePath
import qualified Data.Map as Map

#ifndef HADDOCK
-- XXX - This needs to be factored out into a separate module
#include "PreludePC.hs"
#endif

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
    incHV   <- newHash Map.empty
    argsAV  <- newArray (map VStr args)
    inGV    <- newHandle stdin
    outGV   <- newHandle stdout
    errGV   <- newHandle stderr
    argsGV  <- newScalar undef
    errSV   <- newScalar (VStr "")
    defSV   <- newScalar undef
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
        , genSym "%*INC"        $ hideInSafemode $ MkRef incHV
        , genSym "$*PUGS_HAS_HSPLUGINS" $ hideInSafemode $ MkRef hspluginsSV
        , genSym "$*EXECUTABLE_NAME"    $ hideInSafemode $ MkRef execSV
        , genSym "$*PROGRAM_NAME"       $ hideInSafemode $ MkRef progSV
        , genSym "$*PID"        $ hideInSafemode $ MkRef pidSV
        -- XXX these four need a proper `set' magic
        , genSym "$*UID"        $ hideInSafemode $ MkRef uidSV
        , genSym "$*EUID"       $ hideInSafemode $ MkRef euidSV
        , genSym "$*GID"        $ hideInSafemode $ MkRef gidSV
        , genSym "$*EGID"       $ hideInSafemode $ MkRef egidSV
        , genSym "@*CHECK"      $ MkRef checkAV
        , genSym "@*INIT"       $ MkRef initAV
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
        -- If you change the name or contents of $?PUGS_BACKEND, be sure
        -- to update all t/ and perl5/{PIL2JS,PIL-Run} as well.
        , genSym "$?PUGS_BACKEND" $ MkRef $ constScalar (VStr "BACKEND_PUGS")
        , genSym "$*OS"         $ hideInSafemode $ MkRef $ constScalar (VStr $ getConfig "osname")
        , genSym "&?BLOCK_EXIT" $ codeRef $ mkPrim
            { subName = "&?BLOCK_EXIT"
            , subBody = Prim subExit
            }
        , genSym "%?CONFIG" $ hideInSafemode $ hashRef confHV
        , genSym "$*_" $ MkRef defSV
        , genSym "$*AUTOLOAD" $ MkRef autoSV
        ] ++ classes
    -- defSVcell <- (genSym "$_" . MkRef) =<< newScalar undef
    let env' = env
    {-
            { envLexical  = defSVcell (envLexical env)
            , envImplicit = Map.singleton "$_" ()
            }
    -}
    unless safeMode $ do
        initPerl5 "" (Just . VControl $ ControlEnv env'{ envDebug = Nothing })
        return ()
    initPreludePC env'             -- null in first pass
    where
    hideInSafemode x = if safeMode then MkRef $ constScalar undef else x

initClassObjects :: [Type] -> ClassTree -> IO [STM PadMutator]
initClassObjects parent (Node typ children) = do
    obj     <- createObject (mkType "Class") $
        [ ("name",   castV $ showType typ)
        , ("traits", castV $ map showType parent)
        ]
    objSV   <- newScalar (VObject obj)
    rest    <- mapM (initClassObjects [typ]) children
    let sym = genSym (':':'*':showType typ) $ MkRef objSV
    return (sym:concat rest)

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
                 , foldl1 joinFileName [getConfig "privlib", "auto", "pugs", "perl6", "lib"]
                 , foldl1 joinFileName [getConfig "sitelib", "auto", "pugs", "perl6", "lib"]
                 ]
              ++ [ "." ]
