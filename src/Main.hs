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

-- convenience functions for GHCi
eval = doDebug []
parse = doParse "-"

repLoop :: IO ()
repLoop = do
    env <- tabulaRasa >>= newIORef
    fix $ \loop -> do
        command <- getCommand
        case command of
            CmdQuit       -> putStrLn "Leaving pugs."
            CmdLoad fn    -> doLoad env fn >> loop
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
    case runRule env envBody ruleProgram name (decodeUTF8 prog) of
        (Val err@(VError _ _)) -> putStrLn $ pretty err
        exp -> putStrLn $ pretty exp

doDebug :: [String] -> String -> IO ()
doDebug = runProgramWith id (putStrLn . pretty) "<interactive>"

doLoad :: IORef Env -> String -> IO ()
doLoad env fn = runImperatively env exp where
    exp = App "&require" [] [Val $ VStr fn]

doRunSingle :: IORef Env -> String -> IO ()
doRunSingle menv prog = do
    env <- emptyEnv []
    let exp = runRule env envBody ruleProgram "<interactive>" $ decodeUTF8 prog
    case exp of
        (Statements stmts@((_,pos):_)) -> runImperatively menv $ Statements (stmts ++ [(Syn "dump" [], pos)])
        (Val err@(VError _ _)) -> putStrLn $ pretty err
        _ -> do
            putStrLn "Expected statements, got something else."
            putStrLn "This is a bug in Pugs, please report it."

runImperatively :: IORef Env -> Exp -> IO ()
runImperatively menv exp = do
    env <- readIORef menv
    val <- (`runReaderT` env) $ (`runContT` return) $ resetT $ do
        val <- evaluate exp
        newEnv <- ask
        liftIO $ writeIORef menv newEnv
        return val
    putStrLn $ pretty val

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
        [ SymVal SGlobal "@*ARGS"       argsAV
        , SymVal SGlobal "@*INC"        incAV
        , SymVal SGlobal "$*PROGNAME"   progSV
        , SymVal SGlobal "@*END"        endAV
        , SymVal SGlobal "$*IN"         inGV
        , SymVal SGlobal "$*OUT"        outGV
        , SymVal SGlobal "$*ERR"        errGV
        , SymVal SGlobal "$!"           errSV
        , SymVal SGlobal "%*ENV" (VHash . MkHash $ envFM)
        -- XXX What would this even do?
        -- , SymVal SGlobal "%=POD"        (Val . VHash . MkHash $ emptyFM) 
        , SymVal SGlobal "@=POD"        (VArray . MkArray $ [])
        , SymVal SGlobal "$=POD"        (VStr "")
        , SymVal SGlobal "$?OS"         (VStr config_osname)
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
