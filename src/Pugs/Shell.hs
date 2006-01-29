{-# OPTIONS_GHC -fglasgow-exts -cpp #-}

{-|
    Interactive shell.

>   There is an inn, a merry old inn,
>   beneath an old grey hill,
>   And there they brew a beer so brown
>   That the Man in the Moon himself came down
>   one night to drink his fill...
-}

module Pugs.Shell (
    Command(..),
    RunOptions(..),
    initializeShell,
    getCommand,
    readline,
) where
import Pugs.Internals

#ifdef PUGS_HAVE_READLINE
import qualified System.Console.Readline as Readline
#endif

data Command
   = CmdLoad FilePath
   | CmdQuit
   | CmdParse String
   | CmdParseRaw String
   | CmdRun { runOpt :: RunOptions, runProg :: String }
   | CmdHelp
   | CmdReset

data RunOptions = RunOpts { runOptDebug :: Bool
                          , runOptSeparately :: Bool
                          , runOptShowPretty :: Bool}

-- | read some input from the user
-- parse the input and return the corresponding command
getCommand :: IO Command
getCommand = do
    input <- readline "pugs> " 
    doCommand input

doCommand :: Maybe String -> IO Command
doCommand Nothing = return CmdQuit
doCommand (Just line)
    | all isSpace line  = getCommand
    | (s, _) <- break (== '#') line
    , all isSpace s     = getCommand
    | otherwise         = do
        addHistory line
        return $ parseCommandLine line

parseCommandLine :: String -> Command 
parseCommandLine (':':'e':str)  = CmdRun (RunOpts False True False) str
parseCommandLine (':':'E':str)  = CmdRun (RunOpts True True False) str
parseCommandLine (':':'d':str)  = CmdParse str
parseCommandLine (':':'D':str)  = CmdParseRaw str
parseCommandLine (':':'q':_)    = CmdQuit
parseCommandLine (':':'h':_)    = CmdHelp
parseCommandLine (':':'r':_)    = CmdReset
parseCommandLine (':':'l':str)  = CmdLoad $ unwords (words str)
parseCommandLine str            = CmdRun (RunOpts False False True) str

initializeShell :: IO ()
initializeShell = do
#ifdef PUGS_HAVE_READLINE
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin  NoBuffering
    Readline.initialize
#endif
    return ()

readline :: String -> IO (Maybe String)
readline prompt = do
#ifdef PUGS_HAVE_READLINE
    Readline.setCatchSignals False
    Readline.setCatchSigwinch False
    Readline.readline prompt
#else
    putStr prompt
    input <- getLine
    return $ Just input
#endif

addHistory :: String -> IO ()
#ifdef PUGS_HAVE_READLINE
addHistory str = Readline.addHistory str
#else
addHistory _ = return ()
#endif

