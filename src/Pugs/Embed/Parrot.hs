{-# INCLUDE "Parrot_hsc.h" #-}
{-# LINE 1 "Parrot.hsc" #-}
{-# OPTIONS_GHC -fglasgow-exts -cpp -fno-full-laziness -fno-cse #-}
{-# LINE 2 "Parrot.hsc" #-}

{-# LINE 3 "Parrot.hsc" #-}

module Pugs.Embed.Parrot where
import Data.IORef
import System.Cmd
import System.Process
import System.Directory
import System.IO
import System.IO.Unsafe
import Data.Maybe
import Control.Monad
import Pugs.Compat (getEnv, _PUGS_HAVE_POSIX)
import Pugs.Internals (encodeUTF8)

findExecutable' :: String -> IO (Maybe FilePath)
findExecutable' cmd = do
    dir <- getEnv "PARROT_PATH"
    if isJust dir then (do
        rv  <- findExecutableInDirectory (fromJust dir) cmd
        if isJust rv then return rv else findExecutable'') else do
    findExecutable''
    where
    findExecutable'' = do
        rv  <- findExecutable cmd
        if isJust rv then return rv else do
        cwd <- getCurrentDirectory
        rv  <- findExecutableInDirectory cwd cmd
        if isJust rv then return rv else do
        return Nothing

findExecutableInDirectory :: FilePath -> FilePath -> IO (Maybe FilePath)
findExecutableInDirectory dir cmd = do
    let file | _PUGS_HAVE_POSIX = dir ++ ('/':cmd)
             | otherwise        = dir ++ ('\\':cmd) ++ ".exe"
    ok <- doesFileExist file
    return $ if ok then (Just file) else Nothing

findParrot :: IO FilePath
findParrot = do
    rv <- findExecutable' "parrot"
    case rv of
        Nothing     -> fail "Cannot find the parrot executable in PATH"
        Just cmd    -> return cmd

evalParrotFile :: FilePath -> IO ()
evalParrotFile file = do
    cmd <- findParrot
    -- parrot -j is fatal on systems where jit is not supported,
    -- so we use the next fastest CGP core.
    args <- getEnv "PUGS_PARROT_OPTS"
    let args' | isJust args && fromJust args /= "" = fromJust args
              | otherwise                          = "-f"
    rawSystem cmd [args', file]
    return ()

evalParrot :: String -> IO ()
evalParrot str = do
    tmp         <- getTemporaryDirectory
    (file, fh)  <- openTempFile tmp "pugs.pir"
    hPutStr fh str
    hClose fh
    evalParrotFile file
    removeFile file

evalPGE :: [FilePath] -> String -> String -> [(String, String)] -> IO String
evalPGE paths match rule subrules = do
    (inp, out, err, pid) <- initPGE $ head paths
    (`mapM` subrules) $ \(name, rule) -> do
        let nameStr = escape name
            ruleStr = escape rule
        hPutStrLn inp $ unwords
            ["add_rule", show (length nameStr), show (length ruleStr)]
        hPutStrLn inp nameStr
        hPutStrLn inp ruleStr
    let matchStr = escape match
        ruleStr  = escape rule
    hPutStrLn inp $ unwords
        ["match", show (length matchStr), show (length ruleStr)]
    hPutStrLn inp $ matchStr
    hPutStrLn inp $ ruleStr
    hFlush inp
    rv <- hGetLine out
    case rv of
        ('O':'K':' ':sizeStr) -> do
            size <- readIO sizeStr
            rv   <- sequence (replicate size (hGetChar out))
            ln   <- hGetLine out
            return $ rv ++ ln
        _ -> do
            errMsg  <- hGetContents err
            rv      <- waitForProcess pid
            writeIORef _ParrotInterp Nothing
            let msg | null errMsg = show rv
                    | otherwise   = errMsg
            fail $ "*** Running external 'parrot' failed:\n" ++ msg
    where
    escape = escape . encodeUTF8
    _escape "" = ""
    _escape ('\\':xs) = "\\\\" ++ _escape xs
    _escape ('\n':xs) = "\\n" ++ _escape xs
    _escape (x:xs) = (x:_escape xs)

initPGE :: FilePath -> IO ParrotInterp
initPGE path = do
    rv <- readIORef _ParrotInterp
    case rv of
        Just interp@(_, _, _, pid) -> do
            gone <- getProcessExitCode pid
            if isNothing gone then return interp else do
            writeIORef _ParrotInterp Nothing
            initPGE path
        Nothing -> do
            cmd <- findParrot
            interp <- runInteractiveProcess cmd ["run_pge.pir"] (Just path) Nothing 
            writeIORef _ParrotInterp (Just interp)
            return interp

type ParrotInterp = (Handle, Handle, Handle, ProcessHandle)

{-# NOINLINE _ParrotInterp #-}
_ParrotInterp :: IORef (Maybe ParrotInterp)
_ParrotInterp = unsafePerformIO $ newIORef Nothing

_DoCompile :: Maybe (IORef (String -> FilePath -> String -> IO String))
_DoCompile = Nothing


{-# LINE 387 "Parrot.hsc" #-}
