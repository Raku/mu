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
import IO
import System

import AST
import Eval
import Shell
import Parser
import Help
import Pretty

type State = ()
initState = ()

main = getArgs >>= run

run (('-':'e':str@(_:_)):args) = doRun str args
run ("-e":str:args) = doRun str args
run (file:args)     = readFile file >>= (`doRun` args)
run []              = do
    hSetBuffering stdout NoBuffering 
    banner
    repLoop () 

repLoop :: State -> IO ()
repLoop state 
   = do command <- getCommand
        case command of
           Quit     -> putStrLn "Leaving pugs."
           Load fn  -> load fn
           Eval str -> doEval str [] >> repLoop initState 
           Parse str-> parse str >> repLoop initState 
           Help     -> printHelp >> repLoop state 

load fn = do
    return ()

parse str = runLex print parseOp str

eval str = doEval str []

doEval str args = do
    runLex (putStrLn . pretty . evaluate emptyEnv) parseOp str

doRun str args = do
    runLex (putStrLn . concatMap vCast . vCast . evaluate emptyEnv) parseOp str

