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
import Control.Monad.Reader
import Pugs.Shell

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            banner "Welcome to PIL2 REPL, the Pugs Again Shell!"
            banner "Please enter expressions, or :q to exit"
            (`runReaderT` Nothing) prompt
        [file]      -> eval =<< readFile file
        ["-e", src] -> eval src
        ["-p", src] -> parse src
        ["-P", file] -> parse =<< readFile file
        _           -> do
            putStrLn "Usage: ./pil file"
            putStrLn "       ./pil -e source"
    where
    prompt = do
        cmd <- liftIO $ getCommand
        case cmd of
            CmdQuit -> return ()
            CmdRun { runProg = src } -> runProgram src
            CmdLoad file -> do
                src <- liftIO $ readFile file
                runProgram src
            _ -> do
                banner "Unknown Command"
                prompt
    parseErr err = do
            liftIO $ do
                banner "Error"
                putStrLn $ ioeGetErrorString err
            prompt
    runProgram src = (`catchError` parseErr) $ do
        exps <- liftIO $ do
            exps <- parseNativeLang src 
            banner "Parsed"
            putStrLn =<< prettyM exps
            banner "Evaluated"
            return exps
        cur  <- ask
        res' <- liftIO $ do
            res' <- case cur of
                Just res -> resumeNativeLang res exps
                _        -> evalNativeLang exps
            putStrLn =<< prettyM (result_value res')
            banner "Object Space"
            dumpObjSpace (result_objs res')
            return res'
        local (const $ Just res') prompt

parse :: MonadIO m => String -> m ()
parse src = liftIO $ do
    exps <- parseNativeLang src
    putStrLn =<< prettyM exps
    return ()

eval :: MonadIO m => String -> m ()
eval src = liftIO $ do
    exps <- parseNativeLang src 
    res <- evalNativeLang exps
    putStrLn =<< prettyM (result_value res)
    banner "Object Space"
    dumpObjSpace (result_objs res)

banner :: MonadIO m => String -> m ()
banner x = liftIO $ putStrLn ("\n### " ++ x ++ " ###")
