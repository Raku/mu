{-# OPTIONS_GHC -fglasgow-exts -cpp #-}

{-
    POSIX calls and emulations.

    And now all those lands lie under the wave.
    And I walk in Ambarona, in Tauremorna, in Aldalome.
    In my own land, in the country of Fangorn,
    Where the roots are long,
    And the years lie thicker than the leaves
    In Tauremornalome.
-}

#undef PUGS_HAVE_POSIX
#include "pugs_config.h"

module Pugs.Compat (
    createLink,
    createSymbolicLink,
    readSymbolicLink,
    rename,
    removeLink,
    setFileMode,
    getEnvironment,
    getArg0,
    statFileSize,
    getProcessID,
    getRealUserID,
    getEffectiveUserID,
    getRealGroupID,
    getEffectiveGroupID,
    setEnv,
    unsetEnv,
    signalProcess,
) where

import Foreign
import Foreign.C
import System.Posix.Types

#ifdef PUGS_HAVE_POSIX
import System.Posix.Process
import System.Posix.Env
import System.Posix.Files
import System.Posix.User
import qualified System.Posix.Signals

statFileSize f = do
    s <- getFileStatus f
    return (toInteger (fileSize s))

type Signal = System.Posix.Signals.Signal
--type ProcessID = System.Posix.Types.ProcessID
signalProcess :: Signal -> ProcessID -> IO ()
signalProcess = System.Posix.Signals.signalProcess

#else

import Debug.Trace
import System.Environment
import IO
import System.IO

failWith s = fail $ "'" ++ s ++ "' not implemented on this platform."
warnWith s = trace ("'" ++ s ++ "' not implemented on this platform.") $ return ()

setEnv :: String -> String -> Bool -> IO ()
setEnv _ _ _ = warnWith "setEnv"

unsetEnv :: String -> IO ()
unsetEnv _ = warnWith "unsetEnv"

createLink :: FilePath -> FilePath -> IO ()
createLink _ _ = warnWith "link"

createSymbolicLink :: FilePath -> FilePath -> IO ()
createSymbolicLink _ _ = warnWith "symlink"

readSymbolicLink :: FilePath -> IO FilePath
readSymbolicLink _ = failWith "readlink"

rename :: FilePath -> FilePath -> IO ()
rename _ _ = warnWith "rename"

removeLink :: FilePath -> IO ()
removeLink _ = warnWith "unlink"

setFileMode :: FilePath -> FileMode -> IO ()
setFileMode _ _ = warnWith "chmod"

statFileSize :: FilePath -> IO Integer
statFileSize n = bracket (openFile n ReadMode) hClose hFileSize
-- statFileSize _ = failWith "-s"

getProcessID :: IO ProcessID
getProcessID = return 1

type UserID = Int
type GroupID = Int

getRealUserID :: IO UserID
getRealUserID = return 1

getEffectiveUserID :: IO UserID
getEffectiveUserID = return 1

getRealGroupID :: IO GroupID
getRealGroupID = return 1

getEffectiveGroupID :: IO GroupID
getEffectiveGroupID = return 1

signalProcess :: Int -> Int -> IO ()
signalProcess _ _ = failWith "kill"

#endif

foreign import ccall unsafe "getProgArgv"
  getProgArgv :: Ptr CInt -> Ptr (Ptr CString) -> IO ()

getArg0 :: IO String
getArg0 = do
    alloca $ \ p_argc -> do
    alloca $ \ p_argv -> do
        getProgArgv p_argc p_argv
        argv <- peek p_argv
        peekCString =<< peekElemOff argv 0