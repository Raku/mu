{-# OPTIONS_GHC -fglasgow-exts -fallow-overlapping-instances -fffi #-}

{-|
    Public API for the Pugs system.

>   Dance all ye joyful, now dance all together!
>   Soft is the grass, and let foot be like feather!
>   The river is silver, the shadows are fleeting;
>   Merry is May-time, and merry our meeting.

-}

module Pugs (
    module Pugs,
    Command(..),
    banner,
    liftSTM,
    printCommandLineHelp,
    intro,
    initializeShell,
    getCommand,
    pretty,
    printInteractiveHelp,
) where
import Pugs.AST
import Pugs.CodeGen
import Pugs.Config
import Pugs.Embed
import Pugs.Eval
import Pugs.External
import Pugs.Help
import Pugs.Internals
import Pugs.Monads
import Pugs.Parser.Program
import Pugs.Pretty
import Pugs.Run
import Pugs.Shell
import Pugs.Types
import Data.IORef
import qualified Data.Map as Map
import qualified System.FilePath as FilePath (combine, splitFileName)
import Control.Timeout

{-|
The entry point of Pugs. Uses 'Pugs.Run.runWithArgs' to normalise the command-line
arguments and pass them to 'run'.
-}
pugsMain :: IO ()
pugsMain = do
    let ?debugInfo = Nothing
    timeout <- getEnv "PUGS_TIMEOUT"
    case timeout of
        Just str | [(t, _)] <- reads str -> do
            addTimeout t (hPutStrLn stderr "*** TIMEOUT" >> _exit 1)
            return ()
        _ -> return ()
    mainWith run

foreign import ccall unsafe _exit :: Int -> IO ()

defaultProgramName :: String
defaultProgramName = "<interactive>"

runFile :: String -> IO ()
runFile file = do
    withArgs [file] pugsMain

run :: [String] -> IO ()
run xs = let ?debugInfo = Nothing in run' xs

-- see also Run/Args.hs
run' :: (?debugInfo :: DebugInfo) => [String] -> IO ()
run' ("-d":rest)                 = do
    info <- newDebugInfo
    let ?debugInfo = info
    run' rest
run' ("-l":rest)                 = run' rest
run' ("-w":rest)                 = run' rest
run' ("-I":_:rest)               = run' rest

-- XXX should raise an error here:
-- run ("-I":[])                     = do
--                                    print "Empty -I"

run' ("-h":_)                  = printCommandLineHelp
run' ("-V":_)                  = printConfigInfo []
run' ("-V:":item:_)            = printConfigInfo [item]
run' ("-v":_)                  = banner

-- turn :file: and "-e":frag into a common subroutine/token
run' ("-c":"-e":prog:_)          = doCheck "-e" prog
run' ("-c":file:_)               = readFile file >>= doCheck file

-- -CPIL1.Perl5 outputs PIL formatted as Perl 5.
run' ("-C":backend:args) | (== map toLower backend) `any` ["js","perl5","js-perl5"] = do
    exec <- getArg0
    doHelperRun backend ("--compile-only":("--pugs="++exec):args)
run' ("-C":backend:"-e":prog:_)           = doCompileDump backend "-e" prog
run' ("-C":backend:file:_)                = readFile file >>= doCompileDump backend file

run' ("-B":backend:_) | (== map toLower backend) `any` ["js","perl5","js-perl5","redsix"] = do
    exec <- getArg0
    args <- getArgs
    doHelperRun backend (("--pugs="++exec):args)
run' ("-B":backend:"-e":prog:_)           = doCompileRun backend "-e" prog
run' ("-B":backend:file:_)                = readFile file >>= doCompileRun backend file

run' ("--external":mod:"-e":prog:_)       = doExternal mod "-e" prog
run' ("--external":mod:file:_)            = readFile file >>= doExternal mod file

run' ("-e":prog:args)                     = do doRun "-e" args prog
-- -E is like -e, but not accessible as a normal parameter and used only
-- internally:
--   "-e foo bar.pl" executes "foo" with @*ARGS[0] eq "bar.pl",
--   "-E foo bar.pl" executes "foo" and then bar.pl.
-- XXX - Wrong -- Need to preserve environment across -E runs
run' ("-E":prog:rest)            = run' ("-e":prog:[]) >> run' rest
run' ("-":args)                  = do doRun "-" args =<< readStdin
run' (file:args)                 = readFile file >>= doRun file args
run' []                          = do
    isTTY <- hIsTerminalDevice stdin
    if isTTY
        then do banner >> intro >> repLoop
        else run' ["-"]

readStdin :: IO String
readStdin = do
    eof     <- isEOF
    if eof then return [] else do
    ch      <- getChar
    rest    <- readStdin
    return (ch:rest)

repLoop :: IO ()
repLoop = initializeShell $ do
    tvEnv <- io . newTVarIO . noEnvDebug =<< io (tabulaRasa defaultProgramName)
    fix $ \loop -> do
        command <- getCommand
        let parseEnv f prog = do
                env <- stm (readTVar tvEnv)
                doParse env f defaultProgramName prog
            resetEnv = do
                env <- fmap noEnvDebug (tabulaRasa defaultProgramName)
                stm (writeTVar tvEnv env)
        if command == CmdQuit then io $ putStrLn "Leaving pugs." else do
            io $ case command of
                CmdLoad fn        -> doLoad tvEnv fn
                CmdRun opts prog  -> doRunSingle tvEnv opts prog
                CmdParse prog     -> parseEnv pretty prog
                CmdParseRaw prog  -> parseEnv show prog
                CmdHelp           -> printInteractiveHelp
                CmdReset          -> resetEnv
                _                 -> return ()
            loop

mainWith :: ([String] -> IO a) -> IO ()
mainWith run = do
    hSetBuffering stdout LineBuffering
--    when (isJust _DoCompile) $ do
--        writeIORef (fromJust _DoCompile) doCompile
    runWithArgs run
    globalFinalize

-- convenience functions for GHCi
eval :: String -> IO ()
eval prog = do
    args <- getArgs
    runProgramWith id (putStrLn . encodeUTF8 . pretty) defaultProgramName args (encodeUTF8 prog)

parse :: String -> IO ()
parse prog = do
    env <- tabulaRasa defaultProgramName
    doParse env (encodeUTF8 . pretty) "-" (encodeUTF8 prog)

dump :: String -> IO ()
dump = (doParseWith $ \env _ -> print $ envBody env) "-"

globalFinalize :: IO ()
globalFinalize = join $ readIORef _GlobalFinalizer

dumpGlob :: String -> IO ()
dumpGlob = (doParseWith $ \env _ -> do
    glob <- stm . readMPad $ envGlobal env
    print $ filterUserDefinedPad glob) "-"

{-|
Create a \'blank\' 'Env' for our program to execute in. Of course,
'prepareEnv' actually declares quite a few symbols in the environment,
e.g. \'\@\*ARGS\', \'\$\*PID\', \'\$\*ERR\' etc.

('Tabula rasa' is Latin for 'a blank slate'.)
-}
tabulaRasa :: String -> IO Env
tabulaRasa name = prepareEnv name []

doCheck :: FilePath -> String -> IO ()
doCheck = doParseWith $ \_ name -> do
    putStrLn $ name ++ " syntax OK"

doExternal :: String -> FilePath -> String -> IO ()
doExternal mod = doParseWith $ \env _ -> do
    str <- externalize mod $ envBody env
    putStrLn str

doCompile :: String -> FilePath -> String -> IO String
doCompile backend = doParseWith $ \env file -> do
    globRef <- stm $ do
        glob <- readMPad $ envGlobal env
        newMPad $ filterUserDefinedPad glob
    codeGen backend file env{ envGlobal = globRef }

initCompile :: IO ()
initCompile = do
    compPrelude <- getEnv "PUGS_COMPILE_PRELUDE"
    let bypass = case compPrelude of
            Nothing     -> True
            Just ""     -> True
            Just "0"    -> True
            _           -> False
    setEnv "PUGS_COMPILE_PRELUDE" (if bypass then "0" else "") True

doCompileDump :: String -> FilePath -> String -> IO ()
doCompileDump backend file prog = do
    initCompile
    str <- doCompile backend' file prog
    putStr str
    where
    backend' = capitalizeWord backend
    capitalizeWord []     = []
    capitalizeWord (c:cs) = toUpper c:(map toLower cs)

doCompileRun :: String -> FilePath -> String -> IO ()
doCompileRun backend file prog = do
    initCompile
    str <- doCompile backend' file prog
    evalEmbedded backend' str
    where
    backend' = capitalizeWord backend
    capitalizeWord []     = []
    capitalizeWord (c:cs) = toUpper c:(map toLower cs)

doHelperRun :: String -> [String] -> IO ()
doHelperRun backend args =
    case map toLower backend of
        "js"    -> if (args' == [])
                   then (doExecuteHelper "jspugs.pl"  args)
                   else (doExecuteHelper "runjs.pl"   args)
        "perl5" ->       doExecuteHelper "v6.pm" args
        "js-perl5" -> doExecuteHelper "runjs.pl" (jsPerl5Args ++ args)
        "redsix" -> doExecuteHelper "redsix" args
        _       ->       fail ("unknown backend: " ++ backend)
    where
    args' = f args
    jsPerl5Args = words "--run=jspm --perl5"
    f [] = []
    f (bjs:rest)      | "-BJS" `isPrefixOf` map toUpper bjs = f rest
    f ("-B":js:rest)  | "JS" `isPrefixOf` map toUpper  js = f rest
    f (pugspath:rest) | "--pugs=" `isPrefixOf` pugspath = f rest
    f (x:xs) = x:f xs

doExecuteHelper :: FilePath -> [String] -> IO ()
doExecuteHelper helper args = do
    let searchPaths = concatMap (\x -> map (x++) suffixes) [["."], ["..", ".."], [getConfig "sourcedir"], [getConfig "sourcedir", "blib6", "pugs"], [getConfig "privlib", "auto", "pugs"], [getConfig "sitelib", "auto", "pugs"]]
    mbin <- runMaybeT (findHelper searchPaths)
    case mbin of
        Just binary -> do
            let (p, _) = FilePath.splitFileName binary
            exitWith =<< executeFile' perl5 True (("-I" ++ p):binary:args) Nothing
        _ -> fail ("Couldn't find helper program " ++ helper ++ " (searched in " ++ show (map (foldl1 FilePath.combine) searchPaths) ++ ")")
    where
    suffixes =
        [ []
        , ["perl5", "PIL2JS"]                -- sourcedir/perl5/PIL2JS/jspugs.pl
        , ["perl5", "lib"]                   -- pugslibdir/perl5/lib/jspugs.pl
        , ["misc", "pX", "Common", "redsix"] -- sourcedir/misc/pX/Common/redsix/redsix
        ]
    perl5 = getConfig "perl5_path"
    findHelper :: [[FilePath]] -> MaybeT IO FilePath
    findHelper []     = fail "Can't find anything"
    findHelper (x:xs) = maybeFindFile file
                `mplus` maybeFindFile (file ++ getConfig "exe_ext")
                `mplus` findHelper xs
        where 
        file = foldl1 FilePath.combine (x ++ [helper])
    maybeFindFile :: FilePath -> MaybeT IO FilePath
    maybeFindFile pathname = do
        dir <- liftIO $ getDirectoryContents path `catchIO` (const $ return [])
        guard (filename `elem` dir)
        return pathname
        where
        (path, filename) = FilePath.splitFileName pathname

doParseWith :: (Env -> FilePath -> IO a) -> FilePath -> String -> IO a
doParseWith f name prog = do
    env <- tabulaRasa name
    f' $ parseProgram env{ envDebug = Nothing } name prog
    where
    f' env | Val err@(VError _ _) <- envBody env = do
        hPutStrLn stderr $ pretty err
        globalFinalize
        exitFailure
    f' env = f env name

doParse :: Env -> (Exp -> String) -> FilePath -> String -> IO ()
doParse env prettyFunc name prog = do
    case envBody $ parseProgram env name prog of
        (Val err@(VError _ _)) -> putStrLn $ pretty err
        exp -> putStrLn $ prettyFunc exp

doLoad :: TVar Env -> String -> IO ()
doLoad env fn = do
    runImperatively env (evaluate exp)
    return ()
    where
    exp = App (_Var "&require") Nothing [Val $ VStr fn]

doRunSingle :: TVar Env -> RunOptions -> String -> IO ()
doRunSingle menv opts prog = (`catchIO` handler) $ do
    exp     <- makeProper =<< parse
    if exp == Noop then return () else do
    env     <- theEnv
    rv      <- runImperatively env (evaluate exp)
    result  <- case rv of
        VControl (ControlContinuation env' val _) -> do
            stm $ writeTVar menv env'
            return val
        _ -> return rv
    printer env result
    where
    parse = do
        env <- stm $ readTVar menv
        return $ envBody $ parseProgram env defaultProgramName $
          (dropTrailingSemi prog)
    dropTrailingSemi = reverse .
                       (\x -> ';' : (dropWhile (`elem` " \t\r\n;") x)) .
                       reverse
    hasTrailingSemi = case f prog of ';':_ -> True; _ -> False
        where f = dropWhile (`elem` " \t\r\n\f") . reverse
    theEnv = do
        ref <- if runOptSeparately opts
                then (io . newTVarIO) =<< tabulaRasa defaultProgramName
                else return menv
        debug <- if runOptDebug opts
                then newDebugInfo
                else return Nothing
        stm $ modifyTVar ref $ \e -> e{ envDebug = debug }
        return ref
    printer' = if runOptShowPretty opts then putStrLn . pretty else print
    printer env = \val -> do
      final <- runImperatively env (fromVal' val)
      if hasTrailingSemi
         then case final of (VError _ _) -> printer' final ; _ -> return ()
         else printer' final
    makeProper exp = case exp of
        Val err@(VError (VStr msg) _)
            | runOptShowPretty opts
            , any (== "Unexpected end of input") (lines msg) -> do
            cont <- readline "....> "
            case cont of
                Just line   -> do
                    doRunSingle menv opts (prog ++ ('\n':line))
                    return Noop
                _           -> fail $ pretty err
        Val err@VError{} -> fail $ pretty err
        _ | runOptSeparately opts -> return exp
        App (Syn "block" [Val (VCode cv)]) invs args -> return $
            App (Syn "block" [Val (VCode cv{ subBody = makeDumpEnv (subBody cv) })]) invs args
        _ -> return $ makeDumpEnv exp

    -- XXX Generalize this into structural folding
    makeDumpEnv Noop              = Syn "continuation" []
    makeDumpEnv (Stmts x Noop)    = Stmts (Ann (Cxt cxtItemAny) x) (Syn "continuation" [])
    makeDumpEnv (Stmts x exp)     = Stmts x   $ makeDumpEnv exp
    makeDumpEnv (Ann ann exp)     = Ann ann   $ makeDumpEnv exp
    makeDumpEnv (Sym x y z w exp) = Sym x y z w $ makeDumpEnv exp
    makeDumpEnv exp               = Stmts (Ann (Cxt cxtItemAny) exp) (Syn "continuation" [])

    handler (IOException ioe) | isUserError ioe = do
        putStrLn "Internal error while running expression:"
        putStrLn $ ioeGetErrorString ioe
    handler err = do
        putStrLn "Internal error while running expression:"
        putStrLn $ show err

runImperatively :: TVar Env -> Eval Val -> IO Val
runImperatively menv eval = do
    env <- stm $ readTVar menv
    runEvalIO env $ do
        val <- eval
        newEnv <- ask
        stm $ writeTVar menv newEnv
        return val

doRun :: (?debugInfo :: DebugInfo) => String -> [String] -> String -> IO ()
doRun = do
    runProgramWith (\e -> e{ envDebug = ?debugInfo }) end
    where
    end err@(VError _ _)  = do
        hPutStrLn stderr $ encodeUTF8 $ pretty err
        globalFinalize
        exitFailure
    end (VControl (ControlExit exit)) = do
        globalFinalize
        exitWith exit
    end _ = return ()

noEnvDebug :: Env -> Env
noEnvDebug e = e{ envDebug = Nothing }

runProgramWith ::
    (Env -> Env) -> (Val -> IO a) -> VStr -> [VStr] -> String -> IO a
runProgramWith fenv f name args prog = do
    env <- prepareEnv name args
    -- Cache the compilation tree right here.
    -- We only really care about envGlobal and envBody here.
    val <- runEnv $ parseProgram (fenv env) name prog
    f val

createConfigLine :: String -> String
createConfigLine item = "\t" ++ item ++ ": " ++ (Map.findWithDefault "UNKNOWN" item config)

printConfigInfo :: [String] -> IO ()
printConfigInfo [] = do
    libs <- getLibs
    putStrLn $ unlines $
        ["This is " ++ version ++ " built for " ++ getConfig "archname"
        ,""
        ,"Summary of pugs configuration:" ]
        ++ map (\x -> createConfigLine x) (map (fst) (Map.toList config))
        ++ [ "" ]
        ++ [ "@*INC:" ] ++ libs

printConfigInfo (item:_) = do
        putStrLn $ createConfigLine item

compPIR :: String -> IO ()
compPIR prog = do
    pir <- doCompile "PIR" "-" prog
    putStr $ (subMain ++ (last $ split subMain pir))
    where
    subMain = ".sub main"

runPIR :: String -> IO ()
runPIR prog = do
    pir <- doCompile "PIR" "-" prog
    writeFile "a.pir" pir
    fail "evalParrotFile is bitrotten."
    -- evalParrotFile "a.pir"

{-
withInlinedIncludes :: String -> IO String
withInlinedIncludes prog = do
    libs <- getLibs
    expandInc libs prog
    where
    expandInc :: [FilePath] -> String -> IO String
    expandInc incs str = case breakOnGlue "\nuse " ('\n':str) of
        Nothing -> case breakOnGlue "\nrequire " ('\n':str) of
            Nothing -> return str
            Just (pre, post) -> do
                let (mod, (_:rest)) = span (/= ';') (dropWhile isSpace post)
                mod'    <- includeInc incs mod
                rest'   <- expandInc incs rest
                return $ pre ++ mod' ++ rest'
        Just (pre, post) -> do
            let (mod, (_:rest)) = span isAlphaNum (dropWhile isSpace post)
            mod'    <- includeInc incs mod
            rest'   <- expandInc incs rest
            return $ pre ++ "\n{" ++ mod' ++ "\n}\n" ++ rest'
    includeInc :: [FilePath] -> String -> IO String
    includeInc _ ('v':_) = return []
    includeInc incs name = do
        let name' = concat (intersperse "/" names) ++ ".pm"
            names = split "::" name
        pathName    <- requireInc incs name' (errMsg name incs)
        readFile pathName
    errMsg fn incs = "Can't locate " ++ fn ++ " in @*INC (@*INC contains: " ++ unwords incs ++ ")."
-}
