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

main :: IO ()
main = getArgs >>= run

run :: [String] -> IO ()
run (('-':'e':prog@(_:_)):args) = doRun "-" args prog
run ("-e":prog:args)            = doRun "-" args prog
run ("-h":_)                    = printHelp
run (file:args)                 = readFile file >>= doRun file args
run []                          = do
    hSetBuffering stdout NoBuffering 
    isTTY <- hIsTerminalDevice stdin
    if isTTY
        then banner >> repLoop
        else do
            prog <- getContents
            doRun "-" [] prog

repLoop :: IO ()
repLoop
   = do command <- getCommand
        case command of
           CmdQuit      -> putStrLn "Leaving pugs."
           CmdLoad fn   -> load fn
           CmdEval prog -> doEval [] prog >> repLoop
           CmdParse prog-> doParse prog >> repLoop
           CmdHelp     -> printHelp >> repLoop

load fn = return ()

parse = doParse
eval prog = doEval [] prog

doParse prog = do
    env <- emptyEnv
    runRule env (putStrLn . pretty) ruleProgram prog

doEval :: [String] -> String -> IO ()
doEval = runProgramWith (putStrLn . pretty) "<interactive>"

doRun :: String -> [String] -> String -> IO ()
doRun = runProgramWith (putStr . concatMap vCast . vCast)

runProgramWith :: (Val -> IO ()) -> String -> [String] -> String -> IO ()
runProgramWith f name args prog = do
    env <- emptyEnv
    let env' = runRule (prepare env) id ruleProgram prog
    val <- (`runReaderT` env') $ do
        (`runContT` return) $ do
            evaluate (envBody env')
    f val
    where
    prepare e = e{ envGlobal =
        [ Symbol SGlobal "@*ARGS" (Val $ VList $ map VStr args)
        , Symbol SGlobal "$*PROGNAME" (Val $ VStr name)
        ] ++ envGlobal e }

