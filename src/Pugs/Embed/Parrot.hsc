{-# OPTIONS_GHC -fglasgow-exts -cpp -fvia-C #-}
#if !defined(PUGS_HAVE_PARROT)

module Pugs.Embed.Parrot where

evalParrotFile :: FilePath -> IO ()
evalParrotFile _ = fail "need parrot for eval_parrot"

evalParrot :: String -> IO ()
evalParrot _ = fail "need parrot for eval_parrot"

#else
{-# OPTIONS_GHC -#include "parrot/embed.h" #-}
{-# OPTIONS_GHC -#include "parrot/extend.h" #-}

module Pugs.Embed.Parrot where

import Data.IORef
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Storable
import System.IO.Unsafe

type ParrotInterp   = Ptr ()
type ParrotPackFile = Ptr ()
type ParrotPackFileByteCode = Ptr ()
type ParrotPackFileDirectory = Ptr ()
type ParrotPMC = Ptr ()
type ParrotRunCore  = CInt

{-# NOINLINE _ParrotInterp #-}
_ParrotInterp :: IORef ParrotInterp
_ParrotInterp = unsafePerformIO $ newIORef nullPtr

initParrot :: IO ParrotInterp
initParrot = do
    interp <- readIORef _ParrotInterp 
    if interp /= nullPtr then return interp else do
    interp <- parrot_new nullPtr
    writeIORef _ParrotInterp interp
#if PARROT_JIT_CAPABLE && defined(PARROT_JIT_CORE)
    parrot_set_run_core interp PARROT_JIT_CORE
#endif
    parrot_imcc_init interp
    pf      <- parrot_packfile_new interp 0
    pf_dir  <- get_pf_directory pf
    seg     <- withCString "test_code" $ \p -> do
        parrot_packfile_segment_new_seg interp pf_dir 4 p 1
    set_pf_cur_cs pf seg
    parrot_loadbc interp pf
    return interp

evalParrotFile :: FilePath -> IO ()
evalParrotFile file = do
    interp  <- initParrot
    pf      <- withCString file $ parrot_readbc interp
    parrot_loadbc interp pf
    parrot_runcode interp 0 nullPtr

evalParrot :: String -> IO ()
evalParrot code = do
    interp  <- initParrot
    sub     <- withCString code $ \p -> do
        parrot_imcc_compile_pir interp p 0
    withCString "vv" $ parrot_call_sub interp sub

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

foreign import ccall "PackFile_new"
    parrot_packfile_new :: ParrotInterp -> CInt -> IO ParrotPackFile

foreign import ccall "PackFile_Segment_new_seg"
    parrot_packfile_segment_new_seg :: ParrotInterp -> ParrotPackFileDirectory -> CInt -> CString-> CInt -> IO ParrotPackFileByteCode

foreign import ccall "dod_register_pmc"
    parrot_dod_register_pmc :: ParrotInterp -> ParrotPMC -> IO ()

foreign import ccall "Parrot_set_run_core"
    parrot_set_run_core :: ParrotInterp -> ParrotRunCore -> IO ()

foreign import ccall "Parrot_call_sub"
    parrot_call_sub :: ParrotInterp -> ParrotPMC -> CString -> IO ()

foreign import ccall "imcc_init"
    parrot_imcc_init :: ParrotInterp -> IO ()

foreign import ccall "imcc_compile_pir"
    parrot_imcc_compile_pir :: ParrotInterp -> CString -> CInt ->IO ParrotPMC

#include <parrot/packfile.h>
get_pf_directory :: ParrotPackFile -> IO ParrotPackFileByteCode
get_pf_directory = #peek struct PackFile, directory

set_pf_cur_cs :: ParrotPackFile -> ParrotPackFileByteCode -> IO ()
set_pf_cur_cs = #poke struct PackFile, cur_cs

#endif
