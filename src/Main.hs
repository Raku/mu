{-# OPTIONS -fglasgow-exts #-}

{-
    The Main REPL loop.

    A ship then new they built for him
    Of mithril and of elven-glass
    With shining prow; no shaven oar
    Nor sail she bore on silver mast;
    The Silmaril as lantern light
    And banner bright with living flame
    To gleam thereon by Elbereth
    Herself was set, who thither came...
-}

module Main where
import Internals
import Config 
import Run
import AST
import Eval
import External
import Shell
import Parser
import Help
import Pretty
import Compile

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering 
    args <- getArgs
    run $ concatMap procArg args
    where
    procArg ('-':'e':prog@(_:_)) = ["-e", prog]
    procArg ('-':'d':rest@(_:_)) = ["-d", ('-':rest)]
    procArg x = [x]

run :: [String] -> IO ()
run ("-l":rest)                 = run rest
run ("-d":rest)                 = run rest
run ("-w":rest)                 = run rest
run (('-':'I':_):rest)            = run rest
run (('-':'l':xs):rest)         = run (('-':xs):rest)
run (('-':'w':xs):rest)         = run (('-':xs):rest)
run (('-':'d':xs):rest)         = run (('-':xs):rest)
run (('-':'e':prog@(_:_)):args) = doRun "-e" args prog
run ("-e":prog:args)            = doRun "-e" args prog
run ("-h":_)                    = printCommandLineHelp
run ("--help":_)                = printCommandLineHelp
run ("-V":_)                    = printConfigInfo
run ("-v":_)                    = banner
run ("--version":_)             = banner
run ("-c":"-e":prog:_)          = doCheck "-e" prog
run ("-ce":prog:_)              = doCheck "-e" prog
run ("-c":file:_)               = readFile file >>= doCheck file
run (('-':'C':backend):"-e":prog:_) = doCompile backend "-e" prog
run (('-':'C':backend):file:_)      = readFile file >>= doCompile backend file
run ("--external":mod:"-e":prog:_)    = doExternal mod "-e" prog
run ("--external":mod:file:_)         = readFile file >>= doExternal mod file
run ("-":_)                     = do
    prog <- getContents
    doRun "-" [] prog
run (file:args)                 = readFile file >>= doRun file args
run []                          = do
    isTTY <- hIsTerminalDevice stdin
    if isTTY
        then do banner >> intro >> repLoop
        else run ["-"]

-- convenience functions for GHCi
eval = runProgramWith id (putStrLn . pretty) "<interactive>" []
parse = doParse "-"
dump = (doParseWith $ \exp _ -> print exp) "-"
comp = (doParseWith $ \exp _ -> putStrLn =<< compile "Haskell" exp) "-"

repLoop :: IO ()
repLoop = do
    env <- tabulaRasa >>= newIORef
    modifyIORef env $ \e -> e{ envDebug = Nothing }
    fix $ \loop -> do
        command <- getCommand
        case command of
            CmdQuit           -> putStrLn "Leaving pugs."
            CmdLoad fn        -> doLoad env fn >> loop
            CmdRun opts prog  -> doRunSingle env opts prog >> loop
            CmdParse prog     -> doParse "<interactive>" prog >> loop
            CmdHelp           -> printInteractiveHelp >> loop
            CmdReset          -> tabulaRasa >>= writeIORef env >> loop

tabulaRasa = prepareEnv "<interactive>" []

doCheck = doParseWith $ \_ name -> do
    putStrLn $ name ++ " syntax OK"

doExternal mod = doParseWith $ \exp _ -> do
    str <- externalize mod exp
    putStrLn str

doCompile backend = doParseWith $ \exp _ -> do
    str <- compile backend exp
    writeFile "dump.ast" str

doParseWith f name prog = do
    env <- emptyEnv []
    runRule env (f' . envBody) ruleProgram name $ decodeUTF8 prog
    where
    f' (Val err@(VError _ _)) = do
        hPutStrLn stderr $ pretty err
        exitFailure
    f' exp = f exp name


doParse name prog = do
    env <- emptyEnv []
    case runRule env envBody ruleProgram name (decodeUTF8 prog) of
        (Val err@(VError _ _)) -> putStrLn $ pretty err
        exp -> putStrLn $ pretty exp

doLoad :: IORef Env -> String -> IO ()
doLoad env fn = do
    runImperatively env (evaluate exp)
    return ()
    where
    exp = App "&require" [] [Val $ VStr fn]

doRunSingle :: IORef Env -> RunOptions -> String -> IO ()
doRunSingle menv opts prog = (`catch` handler) $ do
    exp <- parse >>= makeProper
    env <- theEnv
    result <- runImperatively env (evaluate exp)
    printer env result
    where
    parse = do
        parseEnv <- emptyEnv []
        runRule parseEnv (return . envBody) ruleProgram "<interactive>" (decodeUTF8 prog)
    theEnv = do
        ref <- if runOptSeparately opts
                then tabulaRasa >>= newIORef
                else return menv
        debug <- if runOptDebug opts
                then liftM Just (newIORef emptyFM)
                else return Nothing
        modifyIORef ref $ \e -> e{ envDebug = debug }
        return ref
    printer env = if runOptShowPretty opts
        then \val -> do
            final <- runImperatively env (fromVal' val)
            putStrLn $ pretty final
        else print
    makeProper exp = case exp of
        Val err@(VError _ _) -> fail $ pretty err
        Statements stmts@((_,pos):_) | not (runOptSeparately opts) -> do
            let withDump = stmts ++ [(Syn "dump" [], pos)]
            return $ Statements withDump
        _ | not (runOptSeparately opts) -> fail "Expected statements"
        _ -> return exp
    handler err = if not (isUserError err) then ioError err else do
        putStrLn "Internal error while running expression:"
        putStrLn $ ioeGetErrorString err

runImperatively :: IORef Env -> Eval Val -> IO Val
runImperatively menv eval = do
    env <- readIORef menv
    runEval env $ do
        val <- eval
        newEnv <- ask
        liftIO $ writeIORef menv newEnv
        return val

doRun :: String -> [String] -> String -> IO ()
doRun = do
    runProgramWith (\e -> e{ envDebug = Nothing }) end
    where
    end (VError str exp)  = do
        hPutStrLn stderr str
        hPutStrLn stderr (show exp)
        exitFailure
    end _               = return ()

runFile :: String -> IO ()
runFile file = do
    withArgs [file] main

runProgramWith :: 
    (Env -> Env) -> (Val -> IO ()) -> VStr -> [VStr] -> String -> IO ()
runProgramWith fenv f name args prog = do
    env <- prepareEnv name args
    val <- runEnv $ runRule (fenv env) id ruleProgram name $ decodeUTF8 prog
    f val

printConfigInfo :: IO ()
printConfigInfo = do
    environ <- getEnvironment
    libs <- getLibs environ
    putStrLn $ unlines $
        ["This is " ++ version ++ " built for" ++ getConfig "archname"
        ,""
        ,"Summary of pugs configuration:"
        ,""
        , unlines $ map (\x -> "\t" ++ fst x ++ ": " ++ snd x) (fmToList config)
        --,"archlib: " ++ lookupFM config "archlib"
        --,"privlib: " ++ lookupFM config "privlib"
        --,"sitearch: " ++ lookupFM config "sitearch"
        --,"sitelib: " ++ lookupFM config "sitelib"
        ,""
        ] ++
        [ "@*INC:" ] ++ libs

