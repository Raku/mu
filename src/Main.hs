{-# OPTIONS_GHC -fglasgow-exts #-}

{-|
    The Main REPL loop.

>   Un nuevo barco para él construyeron
>   de sándalo. y de vidrio élfico,
>   de proa brillante; ningún remo desnudo,
>   ninguna vela en el mástil de plata:
>   el Silmaril como linterna
>   y en la bandera un fuego vivo
>   puesto allí mismo por Elbereth,
>   y otorgándole alas inmortales...

-}

module Main (module Main, module Pugs) where
import Pugs
import Pugs.Internals

{-|
The entry point of Pugs. Uses 'Pugs.Run.runWithArgs' to normalise the command-line
arguments and pass them to 'run'.
-}
main :: IO ()
main = mainWith run

runFile :: String -> IO ()
runFile file = do
    withArgs [file] main

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

-- -CPerl5 outputs PIL formatted as Perl 5, PIL-Run is not involved.
-- Should we rename -CPerl5, -CJSON etc. to -CPIL.Perl5, -CPIL.JSON etc.?
run ("-C":backend:args) | map toUpper backend == "JS" = do
    exec <- getArg0
    doHelperRun "JS" ("--compile-only":("--pugs="++exec):args)
run ("-C":backend:"-e":prog:_)           = doCompileDump backend "-e" prog
run ("-C":backend:file:_)                = slurpFile file >>= doCompileDump backend file

run ("-B":backend:_) | (== map toLower backend) `any` ["js","perl5"] = do
    exec <- getArg0
    args <- getArgs
    doHelperRun backend (("--pugs="++exec):args)
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
