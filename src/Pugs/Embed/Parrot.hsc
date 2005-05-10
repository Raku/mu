{-# OPTIONS_GHC -fglasgow-exts -cpp -fvia-C #-}
#if !defined(PUGS_HAVE_PARROT)

module Pugs.Embed.Parrot where
import Data.IORef
import System.Cmd
import System.Process
import System.Directory
import System.IO
import System.Exit
import Data.Maybe
import Control.Monad

findParrot :: IO FilePath
findParrot = do
    rv <- findExecutable "parrot"
    case rv of
        Nothing     -> fail "Cannot find the parrot executable in PATH"
        Just cmd    -> return cmd

evalParrotFile :: FilePath -> IO ()
evalParrotFile file = do
    cmd <- findParrot
    rawSystem cmd [file]
    return ()

evalParrot :: String -> IO ()
evalParrot str = do
    tmp         <- getTemporaryDirectory
    (file, fh)  <- openTempFile tmp "pugs.imc"
    hPutStr fh str
    hClose fh
    evalParrotFile file
    removeFile file

evalPGE :: FilePath -> String -> String -> [(String, String)] -> IO String
evalPGE path str pattern subrules = do
    cmd <- findParrot
    (_, out, err, pid) <- runInteractiveProcess cmd
        (["run_pge.pbc", str, pattern] ++ concatMap (\(n, r) -> [n, r]) subrules)
        (Just path) Nothing 
    rv      <- waitForProcess pid
    errMsg  <- hGetContents err
    case (errMsg, rv) of
        ("", ExitSuccess) -> hGetContents out
        ("", _) -> fail $ "*** Running external 'parrot' failed:\n" ++ show rv
        _       -> fail $ "*** Running external 'parrot' failed:\n" ++ errMsg

_DoCompile :: Maybe (IORef (String -> FilePath -> String -> IO String))
_DoCompile = Nothing

#else
{-# OPTIONS_GHC -#include "parrot/embed.h" #-}
{-# OPTIONS_GHC -#include "parrot/extend.h" #-}

#include <parrot/packfile.h>
#include <parrot/interpreter.h>
#include <parrot/register.h>
#include <parrot/string_funcs.h>

module Pugs.Embed.Parrot where

import Data.IORef
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Storable
import System.IO.Unsafe

type ParrotString               = Ptr ()
type ParrotInterp               = Ptr (Ptr ())
type ParrotPackFile             = Ptr ()
type ParrotPackFileByteCode     = Ptr ()
type ParrotPackFileDirectory    = Ptr ()
type ParrotPMC                  = Ptr ()
type ParrotRunCore              = CInt
type ParrotCompilerFunc         = ParrotInterp -> CString -> IO ParrotPMC

{-# NOINLINE _ParrotInterp #-}
_ParrotInterp :: IORef ParrotInterp
_ParrotInterp = unsafePerformIO $ newIORef nullPtr

{-# NOINLINE _DoCompile #-}
_DoCompile :: Maybe (IORef (String -> FilePath -> String -> IO String))
_DoCompile = Just $ unsafePerformIO $ newIORef (error "unregistered")

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
    callback    <- mkCompileCallback compileToParrot
    pugsStr     <- withCString "Pugs" (const_string interp)
    parrot_compreg interp pugsStr callback

    pf      <- parrot_packfile_new interp 0
    pf_dir  <- get_pf_directory pf
    seg     <- withCString "pugs" $ \p -> do
        parrot_packfile_segment_new_seg interp pf_dir 4 p 1
    set_pf_cur_cs pf seg
    parrot_loadbc interp pf
    return interp

loadPGE :: ParrotInterp -> FilePath -> IO (ParrotPMC, ParrotPMC)
loadPGE interp path = do
    ns      <- withCString "PGE::Hs" $ const_string interp
    sym     <- withCString "match" $ const_string interp
    match   <- parrot_find_global interp ns sym
    sym     <- withCString "add_rule" $ const_string interp
    add     <- parrot_find_global interp ns sym
    if match /= nullPtr then return (match, add) else do
    pf      <- withCString (path ++ "/PGE-Hs.pbc") $ parrot_readbc interp
    parrot_loadbc interp pf
    parrot_runcode interp 0 nullPtr
    loadPGE interp path

evalPGE :: FilePath -> String -> String -> [(String, String)] -> IO String
evalPGE path str pattern subrules = do
    interp          <- initParrot
    (match, add)    <- loadPGE interp path
    (`mapM_` subrules) $ \(name, rule) -> do
        s1  <- withCString name $ const_string interp
        s2  <- withCString rule $ const_string interp
        withCString "SSS" $ \sig -> do
            parrot_call_sub_SSS interp add sig s1 s2
    s1  <- withCString str $ const_string interp
    s2  <- withCString pattern $ const_string interp
    s5  <- withCString "SSS" $ \sig -> do
        parrot_call_sub_SSS interp match sig s1 s2
    peekCString =<< #{peek STRING, strstart} s5

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
        parrot_imcc_compile_pir interp p
    withCString "vv" $ parrot_call_sub_vv interp sub

compileToParrot :: ParrotInterp -> CString -> IO ParrotPMC
compileToParrot interp cstr = do
    doCompile   <- case _DoCompile of
        Just ioRef  -> readIORef ioRef
        Nothing     -> error "unregistered?"
    code        <- doCompile "Parrot" "-" =<< peekCString cstr
    withCString code $ \p -> do
        parrot_imcc_compile_pir interp p

foreign import ccall "wrapper"  
    mkCompileCallback :: ParrotCompilerFunc -> IO (FunPtr ParrotCompilerFunc)

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

foreign import ccall "Parrot_compreg"
    parrot_compreg :: ParrotInterp -> ParrotString -> FunPtr ParrotCompilerFunc -> IO ()

foreign import ccall "Parrot_call_sub"
    parrot_call_sub_vv :: ParrotInterp -> ParrotPMC -> CString -> IO ()

foreign import ccall "Parrot_call_sub"
    parrot_call_sub_SSS :: ParrotInterp -> ParrotPMC -> CString -> ParrotString -> ParrotString -> IO ParrotString

foreign import ccall "const_string"
    const_string :: ParrotInterp -> CString -> IO ParrotString

foreign import ccall "Parrot_find_global"
    parrot_find_global :: ParrotInterp -> ParrotString -> ParrotString -> IO ParrotPMC

foreign import ccall "Parrot_get_strreg"
    parrot_get_strreg :: ParrotInterp -> CInt -> IO ParrotString

foreign import ccall "imcc_init"
    parrot_imcc_init :: ParrotInterp -> IO ()

foreign import ccall "imcc_compile_pir"
    parrot_imcc_compile_pir :: ParrotInterp -> CString -> IO ParrotPMC

#def void imcc_init(Parrot_Interp interpreter)
#def PMC * imcc_compile_pir(Parrot_Interp interp, const char *s)

get_pf_directory :: ParrotPackFile -> IO ParrotPackFileByteCode
get_pf_directory = #peek struct PackFile, directory

set_pf_cur_cs :: ParrotPackFile -> ParrotPackFileByteCode -> IO ()
set_pf_cur_cs = #poke struct PackFile, cur_cs

#endif
