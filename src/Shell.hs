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

#undef PUGS_HAVE_READLINE
#include "pugs_config.h"

#ifdef PUGS_HAVE_READLINE
import qualified System.Console.Readline as Readline
#endif

data Command
   = CmdLoad FilePath
   | CmdQuit
   | CmdBrowse
   | CmdParse String 
   | CmdEval String 
   | CmdType Exp 
   | CmdHelp

-- read some input from the user
-- parse the input and return the corresponding command
getCommand :: IO Command
getCommand = do
    input <- readline "pugs> " 
    doCommand input

doCommand Nothing = return CmdQuit
doCommand (Just line)
    | all isSpace line  = getCommand
    | (s, _) <- break (== '#') line
    , all isSpace s     = getCommand
    | otherwise         = do
        addHistory line
        return $ parseCommandLine line

parseCommandLine :: String -> Command 
parseCommandLine ('?':str)      = CmdEval str
parseCommandLine ('.':str)      = CmdParse str
parseCommandLine (':':'q':_)    = CmdQuit
parseCommandLine (':':'h':_)    = CmdHelp
-- parseCommandLine (':':'b':_)    = CmdBrowse
-- parseCommandLine (':':'l':str)  = CmdLoad . unwords . tail $ words str
parseCommandLine str            = CmdEval str

initializeShell :: IO ()
initializeShell
#ifdef PUGS_HAVE_READLINE
   = Readline.initialize
#else
   = return ()
#endif

readline :: String -> IO (Maybe String)
readline prompt
#ifdef PUGS_HAVE_READLINE
   = Readline.readline prompt
#else
   = do putStr prompt
        input <- getLine
        return $ Just input
#endif

addHistory :: String -> IO ()
#ifdef PUGS_HAVE_READLINE
addHistory str = Readline.addHistory str
#else
addHistory _ = return ()
#endif 
