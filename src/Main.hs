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

main = getArgs >>= run

run (('-':'e':str@(_:_)):args) = doRun str args
run ("-e":str:args) = doRun str args
run ("-h":_)        = printHelp
run (file:args)     = readFile file >>= (`doRun` args)
run []              = do
    hSetBuffering stdout NoBuffering 
    isTTY <- hIsTerminalDevice stdin
    if isTTY
        then banner >> repLoop
        else do
            str <- getContents
            doRun str []

repLoop :: IO ()
repLoop
   = do command <- getCommand
        case command of
           CmdQuit     -> putStrLn "Leaving pugs."
           CmdLoad fn  -> load fn
           CmdEval str -> doEval str [] >> repLoop
           CmdParse str-> doParse str >> repLoop
           CmdHelp     -> printHelp >> repLoop

load fn = do
    return ()

doParse = parse
parse str = do
    env <- emptyEnv
    runRule env (putStrLn . pretty) ruleProgram str

eval str = doEval str []

doEval str args = undefined
doRun str args = undefined

{- XXX -
doEval str args = do
    runRule emptyEnv (putStrLn . pretty . evaluate emptyEnv) ruleProgram str
-}

{- XXX -
doRun str args = do
    runRule emptyEnv (putStr . concatMap vCast . vCast . evaluate emptyEnv) ruleProgram str
-}
