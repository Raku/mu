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
import AST
import Eval
import Shell
import Parser
import Help
import Pretty
import Posix

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
run ("-C":"-e":prog:_)          = doDump "-e" prog
run ("-Ce":prog:_)              = doDump "-e" prog
run ("-C":file:_)               = readFile file >>= doDump file
run ("-":_)                     = do
    prog <- getContents
    doRun "-" [] prog
run (file:args)                 = readFile file >>= doRun file args
run []                          = do
    isTTY <- hIsTerminalDevice stdin
    if isTTY
        then do banner >> intro >> repLoop
        else run ["-"]

-- convenience function for GHCi
eval = doDebug []
parse = doParse "-"

repLoop :: IO ()
repLoop = do
    env <- tabulaRasa >>= newIORef
    fix $ \loop -> do
        command <- getCommand
        case command of
            CmdQuit       -> putStrLn "Leaving pugs."
            CmdLoad fn    -> load fn
            CmdRun prog   -> doRunSingle env prog >> loop
            CmdDebug prog -> doDebug [] prog >> loop
            CmdParse prog -> doParse "<interactive>" prog >> loop
            CmdHelp       -> printInteractiveHelp >> loop
            CmdReset      -> tabulaRasa >>= writeIORef env >> loop
            _             -> internalError "unimplemented command"
    where
    tabulaRasa = do
        env <- prepareEnv "<interactive>" []
        return env{ envDebug = Nothing }

load _ = return ()

doCheck = doParseWith $ \_ name -> do
    putStrLn $ name ++ " syntax OK"

doDump = doParseWith $ \exp _ -> do
    fh <- openFile "dump.ast" WriteMode
    hPutStrLn fh $ show exp
    hClose fh

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
    runRule env (putStrLn . pretty) ruleProgram name $ decodeUTF8 prog

doDebug :: [String] -> String -> IO ()
doDebug = runProgramWith id (putStrLn . pretty) "<interactive>"

doRunSingle :: IORef Env -> String -> IO ()
doRunSingle menv prog = doParseWith runner "<interactive>" prog
    where
    runner (Statements stmts@((_,pos):_)) _ = do
        env <- readIORef menv
        val <- (`runReaderT` env) $ (`runContT` return) $ resetT $ do
            val <- evaluate $ Statements (stmts ++ [(Syn "dump" [], pos)])
            newEnv <- ask
            liftIO $ writeIORef menv newEnv
            return val
        putStrLn $ pretty val
    runner _ _ = do
        putStrLn "Cannot handle things that are not statements. try `?' parhaps."

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

runEnv env = do
    (`runReaderT` env) $ do
        (`runContT` return) $ resetT $ do
            evaluateMain (envBody env)

runAST ast = do
    hSetBuffering stdout NoBuffering 
    name <- getProgName
    args <- getArgs
    env  <- prepareEnv name args
    runEnv env{ envBody = ast, envDebug = Nothing }

prepareEnv name args = do
    environ <- getEnvironment
    let envFM = listToFM $ [ (VStr k, VStr v) | (k, v) <- environ ]
    libs    <- getLibs environ
    progSV  <- newMVal $ VStr name
    endAV   <- newMVal $ VList []
    incAV   <- newMVal $ VList (map VStr libs)
    argsAV  <- newMVal $ VList (map VStr args)
    inGV    <- newMVal $ VHandle stdin
    outGV   <- newMVal $ VHandle stdout
    errGV   <- newMVal $ VHandle stderr
    errSV   <- newMVal $ VStr ""
    emptyEnv
        [ Symbol SGlobal "@*ARGS"       $ Val argsAV
        , Symbol SGlobal "@*INC"        $ Val incAV
        , Symbol SGlobal "$*PROGNAME"   $ Val progSV
        , Symbol SGlobal "@*END"        $ Val endAV
        , Symbol SGlobal "$*IN"         $ Val inGV
        , Symbol SGlobal "$*OUT"        $ Val outGV
        , Symbol SGlobal "$*ERR"        $ Val errGV
        , Symbol SGlobal "$!"           $ Val errSV
        , Symbol SGlobal "%*ENV" (Val . VHash . MkHash $ envFM)
        -- XXX What would this even do?
        -- , Symbol SGlobal "%=POD"        (Val . VHash . MkHash $ emptyFM) 
        , Symbol SGlobal "@=POD"        (Val . VArray . MkArray $ [])
        , Symbol SGlobal "$=POD"        (Val . VStr $ "")
        , Symbol SGlobal "$?OS"         (Val . VStr $ config_osname)
        ]

getLibs :: [(String, String)] -> IO [String]
getLibs environ = return $ filter (not . null) libs
    where
    envlibs nm = maybe [] (split config_path_sep) $ nm `lookup` environ
    libs =  envlibs "PERL6LIB"
         ++ [ config_archlib
            , config_privlib
            , config_sitearch
            , config_sitelib
            ]
         ++ [ "." ]

printConfigInfo :: IO ()
printConfigInfo = do
    environ <- getEnvironment
    libs <- getLibs environ
    putStrLn $ unlines $
        ["Summary of pugs configuration:"
        ,""
        ,"archlib: " ++ config_archlib
        ,"privlib: " ++ config_privlib
        ,"sitearch: " ++ config_sitearch
        ,"sitelib: " ++ config_sitelib
        ,""
        ] ++
        [ "@*INC:" ] ++ libs
