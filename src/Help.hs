{-# OPTIONS -fglasgow-exts -cpp #-}

{-
    Online help and banner text.

    But if of ships I now should sing,
    what ship would come to me,
    What ship would bear me ever back
    across so wide a Sea?
-}

#define PUGS_VERSION "6"
#define PUGS_DATE ""
#define PUGS_SVN_REVISION "0"
#include "pugs_config.h"
#include "pugs_version.h"

module Help (printInteractiveHelp, printCommandLineHelp,
             banner, versnum, version, 
             copyright, disclaimer, intro) where

printInteractiveHelp :: IO ()
printInteractiveHelp
   = do putStrLn "Commands available from the prompt:"
        putStrLn ":h              = show this help message"
        putStrLn ":q              = quit"
        putStrLn ". <exp>         = show the syntax tree of an expression"
        putStrLn "? <exp>         = evaluate an expression in small steps"
        putStrLn "<exp>           = evaluate an expression"
{-
        putStrLn ":l <filename>   = load a pugs file (need quotes around the name)" 
        putStrLn ":r              = reload the current file"
-}

{- FIXME: Somebody with more UI skillz should make this nicer -}
printCommandLineHelp :: IO ()
printCommandLineHelp
   = do putStrLn "Command-line flags:"
        putStrLn "-e               executes it's argument"
        putStrLn "-h or --help     gives this message"
        putStrLn "-V               long configuration information & version"
        putStrLn "-v or --version  version"
        putStrLn "-c               parses the file or -e, but does not run it"
        putStrLn "-l -d and -w are ignored for compatability with perl 5"

name       = "Perl6 User's Golfing System"
versnum    = PUGS_VERSION
date	   = PUGS_DATE
version    = name ++ ", version " ++ versnum ++ ", " ++ date ++ revision
copyright  = "Copyright 2005 by Autrijus Tang"
revision
    | rev <- show(PUGS_SVN_REVISION :: Integer)
    , rev /= "0"
    = " (r" ++ rev ++ ")"
    | otherwise
    = ""
disclaimer =
    "This software is distributed under the terms of the " ++
    "GNU Public Licence.\n" ++
    "NO WARRANTY WHATSOEVER IS PROVIDED. " ++
    "See the details in the documentation."

versionFill n = fill ++ vstr
    where
    fill = replicate (n - vlen) ' '
    vlen = length vstr
    vstr = "Version: " ++ versnum ++ revision

banner :: IO ()
banner = putStrLn $ unlines
    [ "   ______                                                            "                                                             
    , " /\\   __ \\                                                           "
    , " \\ \\  \\/\\ \\ __  __  ______  ______     (P)erl6                       "
    , "  \\ \\   __//\\ \\/\\ \\/\\  __ \\/\\  ___\\    (U)ser's                      "
    , "   \\ \\  \\/ \\ \\ \\_\\ \\ \\ \\/\\ \\ \\___  \\   (G)olfing                     "
    , "    \\ \\__\\  \\ \\____/\\ \\____ \\/\\_____\\  (S)ystem                      "
    , "     \\/__/   \\/___/  \\/___/\\ \\/____/                                 "
    , "                       /\\____/   " ++ versionFill 27
    , "                       \\/___/    " ++ copyright       
    , "--------------------------------------------------------------------"
    , " Web: http://pugscode.org/             Email: autrijus@autrijus.org "
    ]

intro :: IO ()
intro = putStrLn $ unlines
    [ "Welcome to Pugs -- " ++ name
    , "Type :h for help"
    ]
