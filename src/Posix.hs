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
) where

#ifdef PUGS_HAVE_POSIX
import System.Posix.Env
import System.Posix.Files
import System.Posix.Process
import System.Posix.Unistd
#else

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
getEnvironment = []

#endif
