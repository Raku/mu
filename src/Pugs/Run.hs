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
import System.FilePath (joinFileName)


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
    liftIO performGC
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
    failSV  <- newScalar (VBool False)
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
    classes <- initClassObjects (MkObjectId $ -1) [] initTree
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
        ] ++ classes
    -- defSVcell <- (gen "$_" . MkRef) =<< newScalar undef
    let env' = env
    {-
            { envLexical  = defSVcell (envLexical env)
            , envImplicit = Map.singleton "$_" ()
            }
    -}
    initPerl5 "" (Just . VControl $ ControlEnv env'{ envDebug = Nothing })
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
    let metaSym  = genSym (cast (":*"++name)) $ MkRef objSV
        codeSym  = genMultiSym (cast ("&*term:"++name)) $ codeRef typeCode
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
                 , foldl1 joinFileName [getConfig "privlib", "auto", "pugs", "perl6", "lib"]
                 , foldl1 joinFileName [getConfig "sitelib", "auto", "pugs", "perl6", "lib"]
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
        let dispProgress = (posName . envPos $ env) == "<interactive>"
        when dispProgress $ putStr "Loading Prelude... "
        catchIO loadPreludePC $ \e -> do
            case e of
                IOException ioe
                    | isUserError ioe, not . null $ ioeGetErrorString ioe
                    -> hPrint stderr ioe
                _ -> return ()
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
        incs     <- liftIO $ fmap ("blib6/lib":) getLibs
        pathName <- liftIO $ requireInc incs "Prelude.pm.yml" ""
        yml      <- liftIO $ parseYamlBytes =<< Str.readFile pathName
        when (n_elem yml == ENil) $ fail ""
        -- FIXME: this detects an error if a bad version number was found,
        -- but not if no number was found at all. Then again, if that
        -- happens surely the fromYAML below will fail?
        case yml of
            MkNode{ n_elem=ESeq (v:_) }
                | MkNode{ n_elem=EStr vnum } <- v
                , vnum /= (packBuf $ show compUnitVersion) -> do
                    fail $ unlines
                        [ "Incompatible version number for compilation unit"
                        , "Consider removing " ++ pathName ++ " and make it again"
                        ]
            _ -> return ()
        -- print "Parsing done!"
        -- print "Loading yaml..."
        --(glob, ast) <- fromYAML yml
        MkCompUnit _ glob ast <- liftIO $ fromYAML yml
        -- print "Loading done!"
        liftSTM $ modifyTVar (envGlobal env) (`unionPads` glob)
        runEnv env{ envBody = ast, envDebug = Nothing }
        --     Right Nothing -> fail ""
{-# OPTIONS_GHC -fglasgow-exts #-}

{-

  This needs to be redone as a proper Haskell parser,
  which will be one of my next projects. But so far,
  this works.

  The operators are simple prefix operators
  with zero or one argument, except for everything
  that ultimatively goes into @ARGS for the Pugs
  script.

  If you change anything here, make sure all tests under
  t/pugsrun/ still pass. Otherwise you might break building
  for everybody, once you commit.

-}

-- | Command line argument parser for pugs.
module Pugs.Run.Args (
    canonicalArgs,
    gatherArgs,
    unpackOptions,
) where
import Pugs.Internals

{- | 
  Convert command line arguments into canonical form for 
  'Pugs.Run.runWithArgs'.  The switch ordering is defined
  by compareArgs and is currently:

  > (-h -v -V) (-I) (-d) (-w) (-c) (-C) (--external) (-M) (-n -p) (-0 -e other)

  Args -M, -n and -p are converted to -e scripts by desugarDashE.
-}
canonicalArgs :: [String] -> [String]
canonicalArgs x = concatMap procArg
                . concatDashE
                . desugarDashE
                . sortBy compareArgs
                . gatherArgs
                . unpackOptions
                $ x

concatDashE :: [Arg] -> [Arg]
concatDashE (Opt "-e" e:xs) = (Opt "-e" $ concat (intersperse "\n" (e:map optArg es))) : rest
    where
    (es, rest)          = partition isOptE xs
    isOptE (Opt "-e" _) = True
    isOptE _            = False
concatDashE (x:xs) = (x:concatDashE xs)
concatDashE xs = xs

data Arg
    = File !String
    | Switch !Char
    | Opt { _optFlag :: !String, optArg :: !String }
    deriving Show

procArg :: Arg -> [String]
procArg (Opt name arg)  = [name, arg]
procArg (File name)     = [name]
procArg (Switch name)   = ['-':name:[]]

unpackOptions :: [String] -> [String]
unpackOptions []                = []
unpackOptions (("-"):rest)      = ("-":unpackOptions rest)
unpackOptions opts@("--":_)     = opts
unpackOptions (('-':opt):arg:rest)
    | takesArg opt              = unpackOption opt ++ (arg:unpackOptions rest)
unpackOptions (('-':opt):rest)  = unpackOption opt ++ unpackOptions rest
unpackOptions opts@[_]          = opts
unpackOptions (filename:rest)   = filename : "--" : rest

takesArg :: String -> Bool
takesArg xs     | xs `elem` withParam   = True
takesArg (x:xs) | x `elem` composable   = takesArg xs
takesArg _                              = False

unpackOption :: String -> [String]
unpackOption "" = [] -- base case for composing
unpackOption opt
    | Just short <- lookup ('-':opt) longOptions = [short]
    | head opt `elem` composable = ['-', head opt] : unpackOption (tail opt)
    | Just (prefix, param) <- prefixOpt opt = ['-':prefix, param]
    | otherwise = ['-':opt]

-- | List of options with long and sort variants, as tupples of long, short (with the dashes).
longOptions :: [(String, String)]
longOptions = [("--help", "-h"), ("--version", "-v")]

-- | List of options that can have their argument just after, with no space.
composable :: [Char]
composable = "cdlnpw"

-- | List of options that can take arguments
withParam :: [String]
withParam = words "e C B I M V:"

prefixOpt :: [Char] -> Maybe (String, String)
prefixOpt opt = msum $ map (findArg opt) withParam

findArg :: Eq a => [a] -> [a] -> Maybe ([a], [a])
findArg arg prefix = do
    param <- afterPrefix prefix arg
    guard (not (null param))
    return (prefix, param)

{-
  Enforce a canonical order of command line switches.  Currently this is:

  > (-h -v -V) (-I) (-d) (-w) (-c) (-C) (--external) (-M) (-n -p) (-0 -e other)

  This makes pattern matching more convenient

  Backwards incompatible changes:

   *  -p and -n autochomp.

   *  -p uses say() instead of print()
-}

compareArgs :: Arg -> Arg -> Ordering
compareArgs a b = compare (argRank a) (argRank b)

argRank :: Arg -> Int
argRank (Switch 'h')         = -1
argRank (Switch 'v')         = -1
argRank (Opt "-V:" _)        = -1
argRank (Switch 'V')         = -1
argRank (Opt "-I" _)         = 0
argRank (Switch 'd')         = 1
argRank (Switch 'w')         = 2
argRank (Switch 'c')         = 3
argRank (Opt "-C" _)         = 4
argRank (Opt "-B" _)         = 4
argRank (Opt "--external" _) = 5
argRank (Opt "-M" _)         = 98
argRank (Switch 'n')         = 99   -- translated into Perl code (later)
argRank (Switch 'p')         = 99   -- translated into Perl code (later)
argRank (Switch 'l')         = 100  -- translated into Perl code (later)
argRank (Switch '0')         = 100  -- translated into Perl code (later)
argRank (Opt "-e" _)         = 100  -- translated into Perl code
argRank _                    = 100  -- filename or @ARGS or whatever

gatherArgs :: [String] -> [Arg]
gatherArgs [] = []
gatherArgs ("-e":frag:rest)        = [Opt "-e" frag] ++ gatherArgs(rest)
gatherArgs ("--external":mod:rest) = [Opt "--external" mod] ++ gatherArgs(rest)
gatherArgs ("-I":dir:rest)         = [Opt "-I" dir] ++ gatherArgs(rest)
gatherArgs ("-M":mod:rest)         = [Opt "-M" mod] ++ gatherArgs(rest)
gatherArgs ("-C":backend:rest)     = [Opt "-C" backend] ++ gatherArgs(rest)
gatherArgs ("-B":backend:rest)     = [Opt "-B" backend] ++ gatherArgs(rest)
gatherArgs ("-V:":item:rest)       = [Opt "-V:" item] ++ gatherArgs(rest)
gatherArgs (('-':[]):xs)           = [File "-"] ++ gatherArgs(xs)
gatherArgs (("--"):rest)           = [File x | x <- rest]
gatherArgs (('-':x:[]):xs)         = [Switch x] ++ gatherArgs(xs)
gatherArgs (x:xs)                  = [File x] ++ gatherArgs(xs)

{- collect "-e" switches together,
   handle transformation of "-M", "-n"
   and "-p" into "-e" fragments
-}
desugarDashE :: [Arg] -> [Arg]
desugarDashE [] = []
desugarDashE ((Switch 'p'):args) = desugarDashE $
    (Opt "-e" "env $_; while (defined($_ = =<>)) { " : args) ++ [Opt "-e" "; say $_; }"]
desugarDashE ((Switch 'n'):args) = desugarDashE $
    (Opt "-e" "env $_; while (defined($_ = =<>)) { " : args) ++ [Opt "-e" "}"]

-- -E is like -e, but not accessible as a normal parameter and used only
-- internally:
--   "-e foo bar.pl" executes "foo" with @*ARGS[0] eq "bar.pl",
--   "-E foo bar.pl" executes "foo" and then bar.pl.
desugarDashE ((Opt "-M" mod):args)
    | (mod', (_:args)) <- break (== '=') mod
    = useWith $ mod' ++ " '" ++ escape args ++ "'.split(',')"
    | otherwise
    = useWith mod
    where
    useWith mod = desugarDashE ((Opt "-E" (";use " ++ mod ++ ";\n")):args)
    escape [] = []
    escape ('\'':xs) = '\\':'\'':escape xs
    escape ('\\':xs) = '\\':'\\':escape xs
    escape (x:xs) = x:escape xs


-- Preserve the curious Perl5 behaviour:
--   perl -e 'print CGI->VERSION' -MCGI     # works
--   perl print_cgi.pl -MCGI                # fails
desugarDashE (x@(Opt "-e" _):y@(Opt "-E" _):args) = desugarDashE (y:x:args)
desugarDashE ((Opt "-E" a):y@(Opt "-e" _):args) = desugarDashE ((Opt "-e" a):y:args)
desugarDashE (x:xs) = (x:desugarDashE xs)
        --     x  -> fail $ "Error loading precompiled Prelude: " ++ show x
