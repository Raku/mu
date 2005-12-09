{-# OPTIONS_GHC -fglasgow-exts -cpp #-}

{-|
    Version information.

>   Tree and flower and leaf and grass,
>   Let them pass! Let them pass!
>   Hill and water under sky,
>   Pass them by! Pass them by!

-}

#include "pugs_config.h"
#include "pugs_version.h"

#ifndef PUGS_VERSION
#define PUGS_VERSION "6"
#endif
#ifndef PUGS_DATE
#define PUGS_DATE ""
#endif

module Pugs.Version (
    name, versnum, date, version, copyright, revnum, revision, disclaimer,
) where

name :: String
name       = "Perl6 User's Golfing System"
versnum :: String
versnum    = PUGS_VERSION
date :: String
date       = PUGS_DATE
version :: String
version    = name ++ ", version " ++ versnum ++ ", " ++ date ++ revision
copyright :: String
copyright  = "Copyright 2005 by Audrey Tang"
revnum :: String
revnum     = show(PUGS_SVN_REVISION :: Integer)
revision :: String
revision
    | rev <- revnum
    , rev /= "0"
    = " (r" ++ rev ++ ")"
    | otherwise
    = ""
disclaimer :: String
disclaimer =
    "This software is distributed under the terms of the " ++
    "GNU Public Licence.\n" ++
    "NO WARRANTY WHATSOEVER IS PROVIDED. " ++
    "See the details in the documentation."
