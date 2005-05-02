{-# OPTIONS_GHC -fglasgow-exts -fvia-C #-}
{-# OPTIONS_GHC -#include "pge.h" #-}

module Text.PGE (
    parseRule,
    pge_gen, pge_parse_new, pge_parse_free,
) where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

parseRule :: String -> IO String
parseRule str = withCString str $ \cstr -> do
    pge_init
    parsed <- pge_p6rule_pir cstr
    peekCString parsed

type PgeExp = Ptr ()

foreign import ccall
    pge_init :: IO ()

foreign import ccall
    pge_gen :: PgeExp -> IO CString

foreign import ccall
    pge_parse_new :: CInt -> PgeExp -> PgeExp -> IO ()

foreign import ccall
    pge_parse_free :: PgeExp -> IO ()

foreign import ccall
    pge_p6rule_pir :: CString -> IO CString
