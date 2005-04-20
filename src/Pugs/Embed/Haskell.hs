{-# OPTIONS_GHC -fglasgow-exts -cpp -package eval #-}

module Pugs.Embed.Haskell where

#include "../pugs_config.h"
#if !defined(PUGS_HAVE_HSPLUGINS)

evalHaskell :: String -> IO (Either String String)
evalHaskell _ = return $ Left "need hs-plugins for eval_haskell"

#else

import qualified Eval

{- Return is either (left) an error message, or (right) the return of the
eval) -}
evalHaskell :: String -> IO (Either String String)
evalHaskell code = do
    let imports = []
    -- eval_ code [import] [flags] [package.confs] [load paths]
    --   -> IO (Either [error-strings] (Maybe a))
    ret <- Eval.eval_ code imports [] [] []
    case ret of
        Right (Just x) -> return $ Right x
        Right Nothing  -> return $ Left "Something strange happened"
        Left x         -> return $ Left $ unlines x

#endif
