{-# OPTIONS_GHC -fglasgow-exts -cpp #-}

#undef PUGS_EMBED_PERL5
#include "../pugs_config.h"

#ifndef PUGS_EMBED_PERL5
module Pugs.Embed.Perl5 where

type PerlInterpreter = ()
type PerlSV = ()

initPerl5 :: String -> IO PerlInterpreter
initPerl5 _ = return ()

evalPerl5 :: String -> IO PerlSV
evalPerl5 _ = return ()

freePerl5 :: PerlInterpreter -> IO ()
freePerl5 _ = return ()

svToVStr :: PerlSV -> IO a
svToVStr _ = fail "not implemented"

#else

{-# INCLUDE <perl5.h> #-}

module Pugs.Embed.Perl5 where
import Foreign
import Foreign.C.Types
import Foreign.C.String

type PerlInterpreter = Ptr ()
type PerlSV = Ptr ()

foreign import ccall "perl.h perl_alloc"
    perl_alloc :: IO PerlInterpreter
foreign import ccall "perl.h perl_construct"
    perl_construct :: PerlInterpreter -> IO ()
foreign import ccall "perl.h perl_run"
    perl_run :: PerlInterpreter -> IO CInt
foreign import ccall "perl.h perl_destruct"
    perl_destruct :: PerlInterpreter -> IO CInt
foreign import ccall "perl.h perl_free"
    perl_free :: PerlInterpreter -> IO ()
foreign import ccall "perl.h Perl_eval_pv"
    eval_pv :: CString -> Word32 -> IO PerlSV
foreign import ccall "perl.h boot_DynaLoader"
    boot_DynaLoader :: Ptr () -> IO ()
foreign import ccall "perl5.h perl5_SvPV"
    perl5_SvPV :: PerlSV -> IO CString
foreign import ccall "perl5.h perl5_init"
    perl5_init :: CInt -> Ptr CString -> IO PerlInterpreter

initPerl5 :: String -> IO PerlInterpreter
initPerl5 str = do
    withCString "-e" $ \prog -> withCString str $ \cstr -> do
        withArray [prog, prog, cstr] $ \argv -> do
            perl5_init 3 argv

svToVStr :: PerlSV -> IO String
svToVStr sv = peekCString =<< perl5_SvPV sv

evalPerl5 :: String -> IO PerlSV
evalPerl5 str = do
    withCString str $ \cstr -> do
        eval_pv cstr 1

freePerl5 :: PerlInterpreter -> IO ()
freePerl5 my_perl = do
    perl_destruct my_perl
    return ()

#endif

