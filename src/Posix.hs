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
    createSymbolicLink,
    readSymbolicLink,
    rename,
    sleep,
) where

#ifdef PUGS_HAVE_POSIX
import System.Posix.Files
import System.Posix.Process
import System.Posix.Unistd
#else

createSymbolicLink :: String -> String -> IO ()
createSymbolicLink = error "'symlink' not implemented on this platform."

readSymbolicLink :: String -> String -> IO ()
readSymbolicLink = error "'readlink' not implemented on this platform."

rename :: String -> String -> IO ()
rename = error "'rename' not implemented on this platform."

sleep :: Int -> IO ()
sleep = error "'sleep' not implemented on this platform."

#endif
