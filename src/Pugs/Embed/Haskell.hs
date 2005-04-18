{-# OPTIONS_GHC -fglasgow-exts -cpp -package eval #-}

module Pugs.Embed.Haskell where

#include "../pugs_config.h"
#if !defined(PUGS_HAVE_HSPLUGINS)

evalHaskell :: String -> IO String
evalHaskell _ = do error "need hs-plugins for eval_haskell"

#else

import Eval

evalHaskell :: String -> IO String
evalHaskell code = do
    let imports = []
    -- eval_ code [import] [flags] [package.confs] [load paths] -> IO (Either [error-strings] (Maybe a))
    ret <- eval code imports
    case ret of
        Just x  -> return x
        Nothing -> fail $ "Couldn't eval haskell code: " ++ code

#endif
