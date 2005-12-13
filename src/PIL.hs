{-# OPTIONS_GHC -fglasgow-exts #-}
{-# OPTIONS_GHC -#include "UnicodeC.h" #-}

{-|
    PIL2 - The New Runcore.

>   And the Tree that was withered shall be renewed,
>   And he shall plant it in the high places,
>   And the City shall be blessed.
>   Sing all ye people!
-}

module PIL where
import PIL.Native
import System (getArgs)
import System.IO
import System.IO.Error
import Control.Monad.Error
import Pugs.Shell

main :: IO ()
main = do
    args <- getArgs
    if null args
        then do
            banner "Welcome to PIL2 REPL, the Pugs Again Shell!"
            banner "Please enter expressions, or :q to exit"
            prompt
        else do
            src <- readFile (head args)
            eval src
    where
    prompt = do
        cmd <- getCommand
        case cmd of
            CmdQuit -> return ()
            CmdRun { runProg = src } -> (`catchError` parseErr) $ do
                exps <- parseNativeLang src 
                banner "Parsed"
                putStrLn =<< prettyM exps
                banner "Evaluated"
                (val, objs) <- evalNativeLang exps
                putStrLn =<< prettyM val
                banner "Object Space"
                dumpObjSpace objs
                prompt
            _ -> do
                banner "Unknown Command"
                prompt
    parseErr err = do
        banner "Error"
        putStrLn $ ioeGetErrorString err
        prompt

parse :: String -> IO ()
parse src = do
    exps <- parseNativeLang src
    putStrLn =<< prettyM exps
    return ()

eval :: String -> IO ()
eval src = do
    exps <- parseNativeLang src 
    (val, objs) <- evalNativeLang exps
    putStrLn =<< prettyM val
    banner "Object Space"
    dumpObjSpace objs

banner :: String -> IO ()
banner x = putStrLn ("\n*** " ++ x ++ " ***")
