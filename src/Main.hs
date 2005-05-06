{-# OPTIONS_GHC -fglasgow-exts #-}

{-|
    The Main REPL loop.

>   A ship then new they built for him
>   Of mithril and of elven-glass
>   With shining prow; no shaven oar
>   Nor sail she bore on silver mast;
>   The Silmaril as lantern light
>   And banner bright with living flame
>   To gleam thereon by Elbereth
>   Herself was set, who thither came...

-}

module Main where
import Pugs.Internals
import Pugs.Config
import Pugs.Run
import Pugs.AST
import Pugs.Types
import Pugs.Eval
import Pugs.External
import Pugs.Shell
import Pugs.Parser
import Pugs.Help
import Pugs.Pretty
import Pugs.Compile
import qualified Data.Map as Map

-- |Pugs' entry point. Uses 'Pugs.Run.runWithArgs' to normalise the command-line 
-- arguments and pass them to 'run'.
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    runWithArgs run

warn :: Show a => a -> IO ()
warn x = do
            hPrint stderr $ show x

-- see also ArgParse.hs
run :: [String] -> IO ()
run (("-d"):rest)                 = run rest

{- -l does not appear here anymore
   as it will have been replaced by an -e snippet further
   above .
-- run (("-l"):rest)                 = run rest
-}

run (("-w"):rest)                 = run rest
run (("-I"):_:rest)               = run rest

-- XXX should raise an error here:
-- run ("-I":[])                     = do
--                                    print "Empty -I"

run ("-h":_)                    = printCommandLineHelp
run (("-V"):_)                  = printConfigInfo []
run (("-V:"):item:_)            = printConfigInfo [item]
run ("-v":_)                    = banner

-- turn :file: and "-e":frag into a common subroutine/token
run ("-c":"-e":prog:_)          = doCheck "-e" prog
run ("-c":file:_)               = readFile file >>= doCheck file

run ("-C":backend:"-e":prog:_)           = doCompileDump backend "-e" prog
run ("-C":backend:file:_)                = readFile file >>= doCompileDump backend file

run ("-B":backend:"-e":prog:_)           = doCompileRun backend "-e" prog
run ("-B":backend:file:_)                = readFile file >>= doCompileRun backend file

run ("--external":mod:"-e":prog:_)    = doExternal mod "-e" prog
run ("--external":mod:file:_)         = readFile file >>= doExternal mod file

run (("-e"):prog:args)          = do doRun "-e" args prog
run ("-":args)                  = do
                                    prog <- getContents
                                    doRun "-" args prog
run (file:args)                 = readFile file >>= doRun file args
run []                          = do
    isTTY <- hIsTerminalDevice stdin
    if isTTY
        then do banner >> intro >> repLoop
        else run ["-"]

-- convenience functions for GHCi
eval :: String -> IO ()
eval prog = do
    args <- getArgs
    runProgramWith id (putStrLn . pretty) "<interactive>" args prog

parse :: String -> IO ()
parse = doParse "-"

dump :: String -> IO ()
dump = (doParseWith $ \env _ -> print $ envBody env) "-"

dumpGlob :: String -> IO ()
dumpGlob = (doParseWith $ \env _ -> do
    glob <- liftSTM $ readTVar $ envGlobal env
    print $ userDefined glob) "-"

userDefined :: Pad -> Pad
userDefined (MkPad pad) = MkPad $ Map.filterWithKey doFilter pad
    where
    doFilter (_:'*':_) _ = False
    doFilter _ _         = True

repLoop :: IO ()
repLoop = do
    initializeShell
    env <- liftSTM . newTVar . (\e -> e{ envDebug = Nothing }) =<< tabulaRasa
    fix $ \loop -> do
        command <- getCommand
        case command of
            CmdQuit           -> putStrLn "Leaving pugs."
            CmdLoad fn        -> doLoad env fn >> loop
            CmdRun opts prog  -> doRunSingle env opts prog >> loop
            CmdParse prog     -> doParse "<interactive>" prog >> loop
            CmdHelp           -> printInteractiveHelp >> loop
            CmdReset          -> tabulaRasa >>= (liftSTM . writeTVar env) >> loop

tabulaRasa :: IO Env
tabulaRasa = prepareEnv "<interactive>" []

doCheck :: FilePath -> String -> IO ()
doCheck = doParseWith $ \_ name -> do
    putStrLn $ name ++ " syntax OK"

doExternal :: String -> FilePath -> String -> IO ()
doExternal mod = doParseWith $ \env _ -> do
    str <- externalize mod $ envBody env
    putStrLn str

doCompile :: [Char] -> FilePath -> String -> IO String
doCompile backend = doParseWith $ \env _ -> do
    globRef <- liftSTM $ do
        glob <- readTVar $ envGlobal env
        newTVar $ userDefined glob
    compile backend env{ envGlobal = globRef }

doCompileDump :: [Char] -> FilePath -> String -> IO ()
doCompileDump backend file prog = do
    str <- doCompile backend file prog
    writeFile "dump.ast" str

doCompileRun :: [Char] -> FilePath -> String -> IO ()
doCompileRun backend file prog = do
    str <- doCompile backend file prog
    evalEmbedded backend str

doParseWith :: (Env -> FilePath -> IO a) -> FilePath -> String -> IO a
doParseWith f name prog = do
    env <- emptyEnv name []
    runRule env f' ruleProgram name $ decodeUTF8 prog
    where
    f' env | Val err@(VError _ _) <- envBody env = do
        hPutStrLn stderr $ pretty err
        exitFailure
    f' env = f env name


doParse :: FilePath -> String -> IO ()
doParse name prog = do
    env <- emptyEnv name []
    case runRule env envBody ruleProgram name (decodeUTF8 prog) of
        (Val err@(VError _ _)) -> putStrLn $ pretty err
        exp -> putStrLn $ pretty exp

doLoad :: TVar Env -> String -> IO ()
doLoad env fn = do
    runImperatively env (evaluate exp)
    return ()
    where
    exp = App "&require" [] [Val $ VStr fn]

doRunSingle :: TVar Env -> RunOptions -> String -> IO ()
doRunSingle menv opts prog = (`catch` handler) $ do
    exp     <- makeProper =<< parse
    env     <- theEnv
    rv      <- runImperatively env (evaluate exp)
    result  <- case rv of
        VControl (ControlEnv env') -> do
            ref <- liftSTM $ findSymRef "$*_" =<< readTVar (envGlobal env')
            val <- runEvalIO env' $ readRef ref
            liftSTM $ writeTVar menv env'
            return val
        _ -> return rv
    printer env result
    where
    parse = do
        env <- liftSTM $ readTVar menv
        runRule env (return . envBody) ruleProgram "<interactive>" (decodeUTF8 prog)
    theEnv = do
        ref <- if runOptSeparately opts
                then (liftSTM . newTVar) =<< tabulaRasa
                else return menv
        debug <- if runOptDebug opts
                then fmap Just (liftSTM $ newTVar Map.empty)
                else return Nothing
        liftSTM $ modifyTVar ref $ \e -> e{ envDebug = debug }
        return ref
    printer env = if runOptShowPretty opts
        then \val -> do
            final <- runImperatively env (fromVal' val)
            putStrLn $ pretty final
        else print
    makeProper exp = case exp of
        Val err@(VError _ _) -> fail $ pretty err
        _ | runOptSeparately opts -> return exp
        _ -> return $ makeDumpEnv exp
    -- XXX Generalize this into structural folding
    makeDumpEnv (Stmts x exp)   = Stmts x   $ makeDumpEnv exp
    makeDumpEnv (Cxt x exp)     = Cxt x     $ makeDumpEnv exp
    makeDumpEnv (Pad x y exp)   = Pad x y   $ makeDumpEnv exp
    makeDumpEnv (Sym x y exp)   = Sym x y   $ makeDumpEnv exp
    makeDumpEnv (Pos x exp)     = Pos x     $ makeDumpEnv exp
    makeDumpEnv (Parens exp)    = Parens    $ makeDumpEnv exp
    makeDumpEnv exp = Stmts exp (Syn "env" [])
    handler err = if not (isUserError err) then ioError err else do
        putStrLn "Internal error while running expression:"
        putStrLn $ ioeGetErrorString err

runImperatively :: TVar Env -> Eval Val -> IO Val
runImperatively menv eval = do
    env <- liftSTM $ readTVar menv
    runEvalIO env $ do
        val <- eval
        newEnv <- ask
        liftSTM $ writeTVar menv newEnv
        return val

doRun :: String -> [String] -> String -> IO ()
doRun = do
    runProgramWith (\e -> e{ envDebug = Nothing }) end
    where
    end err@(VError _ _)  = do
        hPutStrLn stderr (pretty err)
        exitFailure
    end (VControl (ControlExit exit)) = exitWith exit
    end _ = return ()

runFile :: String -> IO ()
runFile file = do
    withArgs [file] main

runProgramWith ::
    (Env -> Env) -> (Val -> IO ()) -> VStr -> [VStr] -> String -> IO ()
runProgramWith fenv f name args prog = do
    env <- prepareEnv name args
    val <- runEnv $ runRule (fenv env) id ruleProgram name $ decodeUTF8 prog
    f val

createConfigLine :: String -> String
createConfigLine item = "\t" ++ item ++ ": " ++ (Map.findWithDefault "UNKNOWN" item config)

printConfigInfo :: [String] -> IO ()
printConfigInfo [] = do
    libs <- getLibs
    putStrLn $ unlines $
        ["This is " ++ version ++ " built for " ++ getConfig "archname"
        ,""
        ,"Summary of pugs configuration:"
        ,"" ]
        ++ map (\x -> createConfigLine x) (map (fst) (Map.toList config))
        ++ [ "" ]
        ++ [ "@*INC:" ] ++ libs

printConfigInfo (item:_) = do
	putStrLn $ createConfigLine item
