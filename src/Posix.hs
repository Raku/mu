{-# OPTIONS -fglasgow-exts -cpp #-}

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
#include "config.h"

module Posix (
    createLink,
    createSymbolicLink,
    readSymbolicLink,
    rename,
    removeLink,
    sleep,
    getEnvironment,
    rmdir,
) where

#ifdef PUGS_HAVE_POSIX
import System.Posix.Env
import System.Posix.Files
import System.Posix.Process
import System.Posix.Unistd
#else

import Data.Maybe
import System.IO.Error
import System.Environment

createLink :: FilePath -> FilePath -> IO ()
createLink _ _ = fail "'link' not implemented on this platform."

createSymbolicLink :: FilePath -> FilePath -> IO ()
createSymbolicLink _ _ = fail "'symlink' not implemented on this platform."

readSymbolicLink :: FilePath -> IO FilePath
readSymbolicLink _ = fail "'readlink' not implemented on this platform."

rename :: FilePath -> FilePath -> IO ()
rename _ _ = fail "'rename' not implemented on this platform."

removeLink :: FilePath -> IO ()
removeLink _ = fail "'unlink' not implemented on this platform."

sleep :: Int -> IO ()
sleep _ = fail "'sleep' not implemented on this platform."

getEnvironment :: IO [(String, String)]
getEnvironment = do
    pairs <- mapM getEnvPair envs
    return $ catMaybes pairs
    where
    getEnvPair :: String -> IO (Maybe (String, String))
    getEnvPair key = (`catch` (\_ -> return Nothing)) $ do
        val <- getEnv key
        return $ Just (key, val)
    envs = words $ " PATH USERNAME USERDOMAIN USERPROFILE "
                ++ " COMPUTERNAME HOMEDRIVE HOMEPATH HOME "
                ++ " OS PROMPT SESSIONNAME APPDATA ALLUSERSPROFILE "
                ++ " WINDOW COMSPEC PROGRAMFILES SYSTEMDRIVE SYSTEMROOT "
                ++ " LOGDIR PERL5LIB PERL6LIB PERL5OPT PERL6OPT "
                ++ " PERLIO PERLIO_DEBUG PERLLIB PERL5DB PERL6DB "
                ++ " PERL5DB_THREADED PERL6DB_THREADED "
                ++ " PERL5SHELL PERL6SHELL PERL_ENCODING PERL_HASH_SEED "
                ++ " PERL_SIGNALS PERL_UNICODE PWD CWD "
                ++ " SERVER_SOFTWARE SERVER_NAME GATEWAY_INTERFACE "
                ++ " SERVER_PROTOCOL SERVER_PORT REQUEST_METHOD "
                ++ " PATH_INFO PATH_TRANSLATED SCRIPT_NAME "
                ++ " QUERY_STRING REMOTE_HOST REMOTE_ADDR "
                ++ " AUTH_TYPE REMOTE_USER REMOTE_IDENT "
                ++ " CONTENT_TYPE CONTENT_LENGTH "
                ++ " HTTP_ACCEPT HTTP_USER_AGENT "
#endif
