{-# OPTIONS_GHC -fglasgow-exts -fno-full-laziness -fno-cse -cpp -fallow-overlapping-instances #-}


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
    _GlobalFinalizer,
) where
import Pugs.Run.Args
import Pugs.Run.Perl5 ()
import Pugs.Internals
import Pugs.Config
import Pugs.AST
import Pugs.Types
import Pugs.Eval
import Pugs.Prim.Eval
import Pugs.Embed
import Pugs.Prelude 
import qualified Data.Map as Map
import qualified Data.ByteString as Str
import DrIFT.YAML
import Data.Yaml.Syck
--import Data.Generics.Schemes
import System.IO
import qualified System.FilePath as FilePath (combine)
import Data.Binary (decode)
import qualified Data.ByteString.Lazy as L


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
    io performGC
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
    globRef <- stm $ do
        glob' <- readMPad $ envGlobal env
        newMPad (glob `unionPads` glob')
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
    failSV  <- newScalar (VBool False)
    egidSV  <- newScalar (VInt $ toInteger egid)
    execSV  <- newScalar (VStr exec)
    progSV  <- newScalar (VStr name)
    checkAV <- newArray []
    initAV  <- newArray []
    endAV   <- newArray []
    takeAV  <- newArray []
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
    classes <- initClassObjects (MkObjectId $ -1) [] initTree
    strictSV<- newScalar $ VBool (name /= "-e")
    baset   <- getCurrentTime
#if defined(PUGS_HAVE_HSPLUGINS)
    hspluginsSV <- newScalar (VInt 1)
#else
    hspluginsSV <- newScalar (VInt 0)
#endif
    let gen = genSym . cast
    env <- emptyEnv name $
        [ gen "@*ARGS"       $ hideInSafemode $ MkRef argsAV
        , gen "@*INC"        $ hideInSafemode $ MkRef incAV
        , gen "%*INC"        $ hideInSafemode $ MkRef incHV
        , gen "$*PUGS_HAS_HSPLUGINS" $ hideInSafemode $ MkRef hspluginsSV
        , gen "$*EXECUTABLE_NAME"    $ hideInSafemode $ MkRef execSV
        , gen "$*PROGRAM_NAME"       $ hideInSafemode $ MkRef progSV
        , gen "$*PID"        $ hideInSafemode $ MkRef pidSV
        -- XXX these four need a proper `set' magic
        , gen "$*UID"        $ hideInSafemode $ MkRef uidSV
        , gen "$*EUID"       $ hideInSafemode $ MkRef euidSV
        , gen "$*GID"        $ hideInSafemode $ MkRef gidSV
        , gen "$*EGID"       $ hideInSafemode $ MkRef egidSV
        , gen "$*FAIL_SHOULD_DIE"$ hideInSafemode $ MkRef failSV
        , gen "@*CHECK"      $ MkRef checkAV
        , gen "@*INIT"       $ MkRef initAV
        , gen "@*END"        $ MkRef endAV
        , gen "$*TAKE"       $ MkRef takeAV
        , gen "$*IN"         $ hideInSafemode $ MkRef inGV
        , gen "$*OUT"        $ hideInSafemode $ MkRef outGV
        , gen "$*ERR"        $ hideInSafemode $ MkRef errGV
        , gen "$*ARGS"       $ hideInSafemode $ MkRef argsGV
        , gen "$!"           $ MkRef errSV
        , gen "$/"           $ MkRef matchAV
        , gen "%*ENV"        $ hideInSafemode $ hashRef MkHashEnv
        , gen "$*CWD"        $ hideInSafemode $ scalarRef MkScalarCwd
        -- XXX What would this even do?
        -- , gen "%=POD"        (Val . VHash $ emptyHV)
        , gen "@=POD"        $ MkRef $ constArray []
        , gen "$=POD"        $ MkRef $ constScalar (VStr "")
        -- To answer the question "what revision does evalbot run on?"
        , gen "$?PUGS_VERSION" $ MkRef $ constScalar (VStr $ getConfig "pugs_version")
        , gen "$*PUGS_VERSION" $ MkRef $ constScalar (VStr $ getConfig "pugs_version")
        -- If you change the name or contents of $?PUGS_BACKEND, be sure
        -- to update all t/ and perl5/{PIL2JS,PIL-Run} as well.
        , gen "$?PUGS_BACKEND" $ MkRef $ constScalar (VStr "BACKEND_PUGS")
        , gen "$?COMPILER"   $ MkRef $ constScalar (VStr "Pugs")
        , gen "$?VERSION"    $ MkRef $ constScalar (VStr $ getConfig "pugs_versnum")
        , gen "$*OS"         $ hideInSafemode $ MkRef $ constScalar (VStr $ getConfig "osname")
        , gen "%?CONFIG" $ hideInSafemode $ hashRef confHV
        , gen "$*_" $ MkRef defSV
        , gen "$*AUTOLOAD" $ MkRef autoSV
        , gen "$*STRICT" $ MkRef strictSV
        -- XXX do we want hideInSafemode?
        , gen "$*BASETIME" $ MkRef $ constScalar (VRat $ pugsTimeSpec baset)
        ] ++ classes
    -- defSVcell <- (gen "$_" . MkRef) =<< newScalar undef
    let env' = env
    {-
            { envLexical  = defSVcell (envLexical env)
            , envImplicit = Map.singleton "$_" ()
            }
    -}
    initPerl5 "" (Just env')
    initPreludePC env'             -- null in first pass
    where
    hideInSafemode x = if safeMode then MkRef $ constScalar undef else x

initClassObjects :: ObjectId -> [Type] -> ClassTree -> IO [STM PadMutator]
initClassObjects uniq parent (MkClassTree (Node typ children)) = do
    obj     <- createObjectRaw uniq Nothing (mkType "Class")
        [ ("name",  castV $ showType typ)
        , ("is",    castV $ map showType parent)
        ]
    objSV   <- newScalar (VObject obj)
    rest    <- forM children $
        initClassObjects (MkObjectId . pred $ unObjectId uniq) [typ] . MkClassTree
    let metaSym  = genSym (cast (":*"++name)) (MkRef objSV)
        codeSym  = genMultiSym (cast ("&*term:"++name)) (codeRef typeCode) mempty
        name     = showType typ
        typeBody = Val . VType . mkType $ name
        Syn "sub" [Val (VCode typeCode)] = typeMacro name typeBody
    return (metaSym:codeSym:concat rest)

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
    inclibs ("-I":dir:rest) = (dir:inclibs rest)
    inclibs (_:rest)        = inclibs rest
    inclibs ([])            = []
    libs p6lib args = (inclibs args)
              ++ (split (getConfig "path_sep") p6lib)
              ++ [ getConfig "archlib"
                 , getConfig "privlib"
                 , getConfig "sitearch"
                 , getConfig "sitelib"
                 , foldl1 FilePath.combine [getConfig "privlib", "auto", "pugs", "perl6", "lib"]
                 , foldl1 FilePath.combine [getConfig "sitelib", "auto", "pugs", "perl6", "lib"]
                 ]
              ++ [ "." ]

bypassPreludePC :: IO Bool
bypassPreludePC = do
    compPrelude <- getEnv "PUGS_COMPILE_PRELUDE"
    return $! case compPrelude of
        Just "0"    -> True
        _           -> False

initPreludePC :: Env -> IO Env
initPreludePC env = do
    bypass <- bypassPreludePC
    if bypass then return env else do
        let dispProgress = (posName . envPos $ env) == (__"<interactive>")
        when dispProgress $ putStr "Loading Prelude... "
        catchIO loadPreludePC $ \e -> do
            case e of
                IOException ioe
                    | isUserError ioe, not . null $ ioeGetErrorString ioe
                    -> hPrint stderr ioe
                _ -> hPrint stderr e
            when dispProgress $ do
                hPutStr stderr "Reloading Prelude from source..."
            evalPrelude
        when dispProgress $ putStrLn "done."
        return env
    where
    style = MkEvalStyle
        { evalResult = EvalResultModule
        , evalError  = EvalErrorFatal
        }
    evalPrelude = runEvalIO env{ envDebug = Nothing } $ opEval style "<prelude>" preludeStr
    loadPreludePC = do  -- XXX: this so wants to reuse stuff from op1EvalP6Y
        -- print "Parsing yaml..."
--        incs     <- io $ fmap ("blib6/lib":) getLibs
--        pathName <- io $ requireInc incs "Prelude.pm.bin" ""
        let MkCompUnit _ _ glob ast = decode preludeByteStringLazy
        appendMPad (envGlobal env) glob
        runEnv env{ envBody = ast, envDebug = Nothing }
        --     Right Nothing -> fail ""
