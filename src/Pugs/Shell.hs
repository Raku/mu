{-# OPTIONS_GHC -fglasgow-exts #-}

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
    module System.Console.Haskeline
) where
import Pugs.Internals
import Data.Char (isSpace)
import System.Console.Haskeline
import System.FilePath
import System.Directory(getHomeDirectory)

data Command
    = CmdLoad FilePath
    | CmdQuit
    | CmdParse String
    | CmdParseRaw String
    | CmdRun { runOpt :: RunOptions, runProg :: String }
    | CmdHelp
    | CmdReset
    deriving Eq

data RunOptions = RunOpts { runOptDebug :: Bool
                          , runOptSeparately :: Bool
                          , runOptShowPretty :: Bool}
    deriving Eq

type Input = InputT IO

-- | read some input from the user
-- parse the input and return the corresponding command
getCommand :: Input Command
getCommand = do
    input <- fmap (fmap encodeUTF8) $ getInputLine "pugs> " 
    doCommand input

doCommand :: Maybe String -> Input Command
doCommand Nothing = return CmdQuit
doCommand (Just line)
    | all isSpace line  = getCommand
    | (s, _) <- break (== '#') line
    , all isSpace s     = getCommand
    | otherwise         = do
        -- addHistory line
        return $ parseCommandLine line

parseCommandLine :: String -> Command 
parseCommandLine (':':'e':'r':str)  = CmdRun (RunOpts False True False) str
parseCommandLine (':':'e':str)  = CmdRun (RunOpts False False False) str
parseCommandLine (':':'E':'R':str)  = CmdRun (RunOpts True True False) str
parseCommandLine (':':'E':str)  = CmdRun (RunOpts True False False) str
parseCommandLine (':':'d':str)  = CmdParse str
parseCommandLine (':':'D':str)  = CmdParseRaw str
parseCommandLine (':':'q':_)    = CmdQuit
parseCommandLine (':':'h':_)    = CmdHelp
parseCommandLine (':':'r':_)    = CmdReset
parseCommandLine (':':'l':str)  = CmdLoad $ unwords (words str)
parseCommandLine str            = CmdRun (RunOpts False False True) str

initializeShell :: Input a -> IO a
initializeShell f = (`runInputT` f) =<< pugsSettings

readline :: String -> IO (Maybe String)
readline prompt = (`runInputT` fmap (fmap encodeUTF8) (getInputLine prompt)) =<< pugsSettings

pugsSettings :: IO (Settings IO)
pugsSettings = do
    home <- getHomeDirectory
    return $ defaultSettings { historyFile = Just (home </> ".pugs_history") }

addHistory :: String -> IO ()
addHistory str = return () -- Readline.addHistory str

