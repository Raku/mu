{-# Oi,PTIONS -fglasgow-exts #-}

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
import Prim
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

runEval :: Env -> Eval Val -> IO Val
runEval env eval = do
    (`runReaderT` env) $ do
        (`runContT` return) $
            resetT eval

runEnv env = runEval env $ evaluateMain (envBody env)

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
    matchAV <- newMVal $ VList []
    incAV   <- newMVal $ VList (map VStr libs)
    argsAV  <- newMVal $ VList (map VStr args)
    inGV    <- newMVal $ VHandle stdin
    outGV   <- newMVal $ VHandle stdout
    errGV   <- newMVal $ VHandle stderr
    errSV   <- newMVal $ VStr ""
    let subExit = \x -> case x of
            [x] -> op1 "exit" x
            _   -> op1 "exit" VUndef
    emptyEnv
        [ SymVal SGlobal "@*ARGS"       argsAV
        , SymVal SGlobal "@*INC"        incAV
        , SymVal SGlobal "$*PROGNAME"   progSV
        , SymVal SGlobal "@*END"        endAV
        , SymVal SGlobal "$*IN"         inGV
        , SymVal SGlobal "$*OUT"        outGV
        , SymVal SGlobal "$*ERR"        errGV
        , SymVal SGlobal "$!"           errSV
        , SymVal SGlobal "$/"           matchAV
        , SymVal SGlobal "%*ENV" (VHash . MkHash $ envFM)
        -- XXX What would this even do?
        -- , SymVal SGlobal "%=POD"        (Val . VHash . MkHash $ emptyFM) 
        , SymVal SGlobal "@=POD"        (VArray . MkArray $ [])
        , SymVal SGlobal "$=POD"        (VStr "")
        , SymVal SGlobal "$?OS"         (VStr config_osname)
        , SymVal SGlobal "$?_BLOCK_EXIT" $ VSub $ Sub
            { isMulti = False
            , subName = "$?_BLOCK_EXIT"
            , subType = SubPrim
            , subPad = []
            , subAssoc = "pre"
            , subParams = []
            , subBindings = []
            , subReturns = "Void"
            , subFun = Prim subExit
            }
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

runComp comp = do
    hSetBuffering stdout NoBuffering 
    name <- getProgName
    args <- getArgs
    env  <- prepareEnv name args
    runEval env{ envDebug = Nothing } comp
