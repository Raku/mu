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
run (('-':'e':prog@(_:_)):args) = doRun "-" args prog
run ("-e":prog:args)            = doRun "-" args prog
run ("-h":_)                    = printHelp
run ("-v":_)                    = banner
run ("--version":_)             = banner
run ("-":args)                  = do
    prog <- getContents
    doRun "-" [] prog
run (file:args)                 = readFile file >>= doRun file args
run []                          = do
    isTTY <- hIsTerminalDevice stdin
    if isTTY
        then do banner >> intro >> repLoop
        else run ["-"]

repLoop :: IO ()
repLoop
   = do command <- getCommand
        case command of
           CmdQuit      -> putStrLn "Leaving pugs."
           CmdLoad fn   -> load fn
           CmdEval prog -> doEval [] prog >> repLoop
           CmdParse prog-> doParse prog >> repLoop
           CmdHelp      -> printHelp >> repLoop

load fn = return ()

parse = doParse
eval prog = doEval [] prog

doParse prog = do
    env <- emptyEnv []
    runRule env (putStrLn . pretty) ruleProgram "<interactive>" prog

doEval :: [String] -> String -> IO ()
doEval = do
    runProgramWith id (putStrLn . pretty) "<interactive>"

doRun :: String -> [String] -> String -> IO ()
doRun = do
    runProgramWith (\e -> e{ envDebug = Nothing }) end
    where
    end v@(VError str exp)  = do
        hPutStrLn stderr str
        hPutStrLn stderr (show exp)
        exitFailure
    end _               = return ()

runFile :: String -> IO ()
runFile file = do
    withArgs [file] main

runProgramWith :: (Env -> Env) -> (Val -> IO ()) -> VStr -> [VStr] -> String -> IO ()
runProgramWith fenv f name args prog = do
    environ <- getEnvironment
    let envFM = listToFM $ [ (VStr k, VStr v) | (k, v) <- environ ]
        p6lib = maybeToList $ lookup "PERL6LIB" environ
        p5lib = map fixLib $ catMaybes [lookup "PERL5LIB" environ, lookup "PERLLIB" environ]
    progSV  <- newMVal $ VStr name
    endAV   <- newMVal $ VList []
    incAV   <- newMVal $ VList (map VStr $ p6lib ++ p5lib ++ incs)
    argsAV  <- newMVal $ VList (map VStr args)
    inGV    <- newMVal $ VHandle stdin
    outGV   <- newMVal $ VHandle stdout
    errGV   <- newMVal $ VHandle stderr
    env <- emptyEnv
        [ Symbol SGlobal "@*ARGS"       $ Val argsAV
        , Symbol SGlobal "@*INC"        $ Val incAV
        , Symbol SGlobal "$*PROGNAME"   $ Val progSV
        , Symbol SGlobal "@*END"        $ Val endAV
        , Symbol SGlobal "$*IN"         $ Val inGV
        , Symbol SGlobal "$*OUT"        $ Val outGV
        , Symbol SGlobal "$*ERR"        $ Val errGV
        , Symbol SGlobal "%*ENV" (Val . VHash . MkHash $ envFM)
        ]
--    str <- return "" -- getContents
    let env' = runRule (fenv env) id ruleProgram name prog
    val <- (`runReaderT` env') $ do
        (`runContT` return) $ resetT $ do
            evaluateMain (envBody env')
    f val
    where
    fixLib = ((++) "/Perl6/lib")
    incs = ["./lib/Perl6/lib", "../lib/Perl6/lib", "."]

{-
main = do
    -- (optsIO, rest, errs) <- return . getOpt Permute options $ procArgs args

options :: [OptDescr (Opts -> Opts)]
options =
    [ reqArg "e" ["eval"]           "command"       "Command-line program"
        (\s o -> o { encodings          = split "," s })
    , noArg  "d" ["debug"]                          "Turn on debugging"
        (\s o -> o { inputFile          = s })
    , noArg  "h" ["help"]                           "Show help"
        (\o   -> o { showHelp           = usage "" })
    ]
-}

