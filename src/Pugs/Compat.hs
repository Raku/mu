{-# OPTIONS_GHC -fglasgow-exts -cpp #-}

{-|
    POSIX calls and emulations.

>   And now all those lands lie under the wave.
>   And I walk in Ambarona, in Tauremorna, in Aldalome.
>   In my own land, in the country of Fangorn,
>   Where the roots are long,
>   And the years lie thicker than the leaves
>   In Tauremornalome.
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
    getProcessTimes,
    setEnv,
    getEnv,
    unsetEnv,
    signalProcess,
    executeFile,
    ProcessTimes(..),
) where

import Foreign
import Foreign.C
import System.Posix.Types

#ifdef PUGS_HAVE_POSIX
import System.Posix.Process
import System.Posix.Env hiding (getEnvironment)
import System.Posix.Files
import System.Posix.User
import System.Environment (getEnvironment)
import qualified System.Posix.Signals

statFileSize :: FilePath -> IO Integer
statFileSize f = do
    s <- getFileStatus f
    return (toInteger (fileSize s))

type Signal = System.Posix.Signals.Signal
--type ProcessID = System.Posix.Types.ProcessID
signalProcess :: Signal -> ProcessID -> IO ()
signalProcess = System.Posix.Signals.signalProcess

#else

import Debug.Trace
-- import System.Environment
import qualified System.Environment
import IO
import System.IO
import Foreign.C.String
import Foreign.Ptr

failWith :: (Monad m) => String -> m a
failWith s = fail $ "'" ++ s ++ "' not implemented on this platform."

warnWith :: String -> IO ()
warnWith s = trace ("'" ++ s ++ "' not implemented on this platform.") $ return ()

-- This should all be moved into Compat.Win32, once we go that route

foreign import stdcall unsafe "SetEnvironmentVariableW" win32SetEnv :: CWString -> CWString -> IO ()
foreign import stdcall unsafe "GetEnvironmentVariableW" win32GetEnv :: CWString -> CWString -> Int -> IO Int
-- also implement/redefine getEnvironment as GetEnvironmentStrings

setEnv :: String -> String -> Bool -> IO ()
setEnv k v _ = withCWString k $ \ key ->
               withCWString v $ \ value -> do
                 rc <- win32SetEnv key value
                 return $ rc

getEnv :: String -> IO (Maybe String)
getEnv k = withCWString k $ \ key ->
    withCWString (replicate size ' ') $ \ buf -> do
      rc <- win32GetEnv key buf size
      if rc > 0
        then do
          t <- peekCWString buf
          return $ Just t
        else
          return $ Nothing
      where
        size = 32768

unsetEnv :: String -> IO ()
unsetEnv k = withCWString k $ \ key -> withCWString "" $ \ v -> do
               win32SetEnv key v
-- #unsetEnv _ = warnWith "unsetEnv"

getEnvironment :: IO [(String, String)]
getEnvironment = System.Environment.getEnvironment

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

-- Win32 specific
data ProcessTimes = ProcessTimes !Int !Int !Int !Int !Int
foreign import stdcall unsafe "GetProcessTimes" win32GetProcessTimes ::
  Int -> Ptr Int -> Ptr Int -> Ptr Int -> Ptr Int -> IO Int 
-- relies on Int == Int32 on Windows

-- See Perl5 win32/win32.c for the complete implementation
-- that works on Windows 95 as well
getProcessTimes :: IO ProcessTimes
getProcessTimes = do
                    pid <- getProcessID
                    alloca $ \ pDummy  -> do
                    alloca $ \ pKernel -> do
                    alloca $ \ pUser   -> do
                      rc      <- win32GetProcessTimes pid pDummy pDummy pKernel pUser
                      user    <- peek pUser
                      kernel  <- peek pKernel                    
                      return $ ProcessTimes 0 user kernel 0 0

-- This is Win32 specific, dunno about other non POSIX platforms
statFileSize :: FilePath -> IO Integer
statFileSize n = bracket (openFile n ReadMode) hClose hFileSize
-- statFileSize _ = failWith "-s"

-- Again, Win32 specific magic, as stolen from GHC
foreign import stdcall "GetCurrentProcessId" getProcessID :: IO Int -- relies on Int == Int32 on Windows

-- getProcessID :: IO Int
-- getProcessID = return $ 1

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

executeFile :: FilePath -> Bool -> [String] -> Maybe [(String, String)] -> IO ()
executeFile _ _ _ _ = failWith "executeFile"

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
