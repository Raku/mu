{-# OPTIONS -fglasgow-exts -cpp #-}

{-
    Interactive shell.

    There is an inn, a merry old inn,
    beneath an old grey hill,
    And there they brew a beer so brown
    That the Man in the Moon himself came down
    one night to drink his fill...
-}

module Shell where
import Internals
import AST

#define READLINE 1
#include "config.h"

#ifdef READLINE
import qualified System.Console.Readline as Readline
#endif

data Command
   = Load FilePath
   | Quit
   | Browse
   | Parse String 
   | Eval String 
   | Type Exp 
   | Help

-- read some input from the user
-- parse the input and return the corresponding command
getCommand :: IO Command
getCommand = do
    input <- readline "pugs> " 
    doCommand input

doCommand Nothing = return Quit
doCommand (Just line)
    | all isSpace line  = getCommand
    | (s, _) <- break (== '#') line
    , all isSpace s     = getCommand
    | otherwise         = do
        addHistory line
        return $ parseCommandLine line

parseCommandLine :: String -> Command 
parseCommandLine ('?':str)      = Eval str
parseCommandLine ('.':str)      = Parse str
parseCommandLine (':':'q':_)    = Quit
parseCommandLine (':':'h':_)    = Help
-- parseCommandLine (':':'b':_)    = Browse
-- parseCommandLine (':':'l':str)  = Load . unwords . tail $ words str
parseCommandLine str            = Eval str

initializeShell :: IO ()
initializeShell
#ifdef READLINE
   = Readline.initialize
#else
   = return ()
#endif

readline :: String -> IO (Maybe String)
readline prompt
#ifdef READLINE
   = Readline.readline prompt
#else
   = do putStr prompt
        input <- getLine
        return $ Just input
#endif

addHistory :: String -> IO ()
addHistory str
#ifdef READLINE
   = Readline.addHistory str
#else
   = return ()
#endif 
