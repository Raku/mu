{-# OPTIONS -cpp #-}
#define READLINE 1
#include "config.h"

{-------------------------------------------------------------------------------

        Copyright:              Bernie Pope 2004

        Module:                 Shell

        Description:            The Baskell interpreter command line. 

        Primary Authors:        Bernie Pope

        Notes: 

        Will use GNU readline if it is available. 

-------------------------------------------------------------------------------}

module Shell 
   ( Command (..)
   , getCommand
   , initializeShell
   ) 
   where

import AST
import Char
#ifdef READLINE
import qualified System.Console.Readline as Readline
   ( readline
   , addHistory
   , initialize
   )
#endif

--------------------------------------------------------------------------------

data Command
   = Load FilePath
   | Quit
   | Browse
   | Parse String 
   | Eval String 
   | Type Exp 
   | Help
#ifdef DEBUG
   | ShowAST
   | ShowDepend
#endif

-- read some input from the user
-- parse the input and return the corresponding command
getCommand :: IO Command
getCommand 
   = do input <- readline "pugs> " 
        case input of
           Nothing -> return Quit
           Just line
              -> if all isSpace line
                    then getCommand
                    else do
                        addHistory line
                        return $ parseCommandLine line

parseCommandLine :: String -> Command 
parseCommandLine ('?':str)      = Eval str
parseCommandLine ('.':str)      = Parse str
parseCommandLine (':':'q':_)    = Quit
parseCommandLine (':':'h':_)    = Help
parseCommandLine (':':'b':_)    = Browse
parseCommandLine (':':'l':str)  = Load . unwords . tail $ words str
parseCommandLine str            = Eval str

--------------------------------------------------------------------------------

-- optional support for GNU Readline  

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
