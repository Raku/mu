{-# OPTIONS_GHC -fglasgow-exts #-}

{-|
    The Main REPL loop.

>   眾人為彼造新舟
>   鑄以祕銀精靈璃
>   船首閃耀何需槳
>   銀桅未有風帆繫
>
>   無雙寶鑽作燈炬
>   旗幟輝煌展生焰
>   映照燃星雅碧綠
>   神祇乘梭下九天...

-}

module Main (module Main, withArgs) where
import Pugs.Internals
import Pugs.Config
import Pugs.Run
import Pugs.AST
import Pugs.Types
import Pugs.Eval
import Pugs.External
import Pugs.Shell
import Pugs.Parser.Program
import Pugs.Help
import Pugs.Pretty
import Pugs.CodeGen
import Pugs.Embed
import Pugs.Prim.Eval (requireInc)
import qualified Data.Map as Map
import Data.IORef

{-|
Pugs' entry point. Uses 'Pugs.Run.runWithArgs' to normalise the command-line
arguments and pass them to 'run'.
-}
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    when (isJust _DoCompile) $ do
        writeIORef (fromJust _DoCompile) doCompile
    runWithArgs run

warn :: Show a => a -> IO ()
warn x = do
            hPrint stderr $ show x

-- see also Run/Args.hs
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
run ("-C":backend:file:_)                = slurpFile file >>= doCompileDump backend file

run ("-B":backend:"-e":prog:_)           = doCompileRun backend "-e" prog
run ("-B":backend:file:_)                = slurpFile file >>= doCompileRun backend file

run ("--external":mod:"-e":prog:_)    = doExternal mod "-e" prog
run ("--external":mod:file:_)         = readFile file >>= doExternal mod file

run (("-e"):prog:args)          = do doRun "-e" args prog
-- -E is like -e, but not accessible as a normal parameter and used only
-- internally:
--   "-e foo bar.p6" executes "foo" with @*ARGS[0] eq "bar.p6",
--   "-E foo bar.p6" executes "foo" and then bar.p6.
-- XXX - Wrong -- Need to preserve environment across -E runs
run (("-E"):prog:rest)          = run ("-e":prog:[]) >> run rest
run ("-":args)                  = do doRun "-" args =<< readStdin
run (file:args)                 = readFile file >>= doRun file args
run []                          = do
    isTTY <- hIsTerminalDevice stdin
    if isTTY
        then do banner >> intro >> repLoop
        else run ["-"]

readStdin :: IO String
readStdin = do
    eof     <- isEOF
    if eof then return [] else do
    ch      <- getChar
    rest    <- readStdin
    return (ch:rest)

-- convenience functions for GHCi
eval :: String -> IO ()
eval prog = do
    args <- getArgs
    runProgramWith id (putStrLn . pretty) "<interactive>" args prog

parse :: String -> IO ()
parse = doParse pretty "-"

dump :: String -> IO ()
dump = (doParseWith $ \env _ -> print $ envBody env) "-"

dumpGlob :: String -> IO ()
dumpGlob = (doParseWith $ \env _ -> do
    glob <- liftSTM $ readTVar $ envGlobal env
    print $ userDefined glob) "-"

userDefined :: Pad -> Pad
userDefined (MkPad pad) = MkPad $ Map.filterWithKey doFilter pad
    where
    doFilter key _ = not (key `elem` reserved)
    reserved = words $
        "@*ARGS @*INC %*INC $*PUGS_HAS_HSPLUGINS $*EXECUTABLE_NAME " ++
        "$*PROGRAM_NAME $*PID $*UID $*EUID $*GID $*EGID @*CHECK @*INIT $*IN " ++
        "$*OUT $*ERR $*ARGS $!  $/ %*ENV $*CWD @=POD $=POD $?PUGS_VERSION " ++
        "$*OS &?BLOCK_EXIT %?CONFIG $*_ $*AUTOLOAD $*PACKAGE"

repLoop :: IO ()
repLoop = do
    initializeShell
    env <- liftSTM . newTVar . (\e -> e{ envDebug = Nothing }) =<< tabulaRasa "<interactive>"
    fix $ \loop -> do
        command <- getCommand
        case command of
            CmdQuit           -> putStrLn "Leaving pugs."
            CmdLoad fn        -> doLoad env fn >> loop
            CmdRun opts prog  -> doRunSingle env opts prog >> loop
            CmdParse prog     -> doParse pretty "<interactive>" prog >> loop
            CmdParseRaw prog  -> doParse show   "<interactive>" prog >> loop
            CmdHelp           -> printInteractiveHelp >> loop
            CmdReset          -> tabulaRasa "<interactive>" >>= (liftSTM . writeTVar env) >> loop

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
doCompile backend = doParseWith $ \env _ -> do
    globRef <- liftSTM $ do
        glob <- readTVar $ envGlobal env
        newTVar $ userDefined glob
    translate backend env{ envGlobal = globRef }

doCompileDump :: String -> FilePath -> String -> IO ()
doCompileDump backend file prog = do
    str <- doCompile backend' file prog
    putStr str
    where
    backend' = capitalizeWord backend
    capitalizeWord []     = []
    capitalizeWord (c:cs) = toUpper c:(map toLower cs)

doCompileRun :: String -> FilePath -> String -> IO ()
doCompileRun backend file prog = do
    str <- doCompile backend' file prog
    evalEmbedded backend' str
    where
    backend' = capitalizeWord backend
    capitalizeWord []     = []
    capitalizeWord (c:cs) = toUpper c:(map toLower cs)

doParseWith :: (Env -> FilePath -> IO a) -> FilePath -> String -> IO a
doParseWith f name prog = do
    env <- tabulaRasa name
    f' $ parseProgram env name $ decodeUTF8 prog
    where
    f' env | Val err@(VError _ _) <- envBody env = do
        hPutStrLn stderr $ pretty err
        exitFailure
    f' env = f env name

doParse :: (Exp -> String) -> FilePath -> String -> IO ()
doParse prettyFunc name prog = do
    env <- tabulaRasa name
    case envBody $ parseProgram env name (decodeUTF8 prog) of
        (Val err@(VError _ _)) -> putStrLn $ pretty err
        exp -> putStrLn $ prettyFunc exp

doLoad :: TVar Env -> String -> IO ()
doLoad env fn = do
    runImperatively env (evaluate exp)
    return ()
    where
    exp = App (Var "&require") Nothing [Val $ VStr fn]

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
        return $ envBody $ parseProgram env "<interactive>" $
	  (decodeUTF8 prog)
    theEnv = do
        ref <- if runOptSeparately opts
                then (liftSTM . newTVar) =<< tabulaRasa "<interactive>"
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
        hPutStrLn stderr $ encodeUTF8 $ pretty err
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
    val <- runEnv $ parseProgram (fenv env) name $ decodeUTF8 prog
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
    evalParrotFile "a.pir"

slurpFile :: FilePath -> IO String
slurpFile file = do
    prog <- readFile file
    libs <- getLibs
    file <- expandInc libs prog
    -- writeFile "ZZZ" file
    return file
    where
    expandInc :: [FilePath] -> String -> IO String
    expandInc incs str = case breakOnGlue "\nuse " str of
        Nothing -> case breakOnGlue "\nrequire " str of
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
            return $ pre ++ mod' ++ rest'
    includeInc :: [FilePath] -> String -> IO String
    includeInc _ ('v':_) = return []
    includeInc incs name = do
        let name' = concat (intersperse "/" names) ++ ".pm"
            names = split "::" name
        pathName    <- requireInc incs name' (errMsg name incs)
        readFile pathName
    errMsg file incs = "Can't locate " ++ file ++ " in @*INC (@*INC contains: " ++ unwords incs ++ ")."

