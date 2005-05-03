{-# OPTIONS_GHC -fglasgow-exts -cpp -fvia-C #-}
#include "../pugs_config.h"
#if !defined(PUGS_HAVE_PARROT)

module Pugs.Embed.Parrot where

evalParrot :: FilePath -> IO ()
evalParrot _ = fail "need parrot for eval_parrot"

#else
{-# OPTIONS_GHC -#include "parrot/embed.h" #-}

module Pugs.Embed.Parrot where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

type ParrotInterp   = Ptr ()
type ParrotPackFile = Ptr ()
type ParrotRunCore  = CInt

evalParrot :: FilePath -> IO ()
evalParrot file = do
    interp  <- parrot_new nullPtr
    parrot_init interp
#if PARROT_JIT_CAPABLE && defined(PARROT_JIT_CORE)
    parrot_set_run_core interp PARROT_JIT_CORE
#endif
    pf      <- withCString file $ parrot_readbc interp
    parrot_loadbc interp pf
    parrot_runcode interp 0 nullPtr
    parrot_destroy interp

foreign import ccall "Parrot_new"
    parrot_new :: ParrotInterp -> IO ParrotInterp

foreign import ccall "Parrot_init"
    parrot_init :: ParrotInterp -> IO ()

foreign import ccall "Parrot_readbc"
    parrot_readbc :: ParrotInterp -> CString -> IO ParrotPackFile

foreign import ccall "Parrot_loadbc"
    parrot_loadbc :: ParrotInterp -> ParrotPackFile -> IO ()

foreign import ccall "Parrot_runcode"
    parrot_runcode :: ParrotInterp -> CInt -> Ptr CString -> IO ()

foreign import ccall "Parrot_destroy"
    parrot_destroy :: ParrotInterp -> IO ()

foreign import ccall "Parrot_set_run_core"
    parrot_set_run_core :: ParrotInterp -> ParrotRunCore -> IO ()

#endif
