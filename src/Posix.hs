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

#undef READLINE
#include "config.h"

module Posix (
    sleep,
    rename,
) where

#ifdef POSIX
import System.Posix.Files
import System.Posix.Process
import System.Posix.Unistd
#else

sleep :: Int -> IO ()
sleep = error "'sleep' not implemented on this platform."

rename :: String -> String -> IO ()
rename = error "'rename' not implemented on this platform."

#endif
