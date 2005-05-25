{-# OPTIONS_GHC -fglasgow-exts -cpp #-}

#undef PUGS_EMBED_PERL5
#include "../pugs_config.h"

#ifndef PUGS_EMBED_PERL5
module Pugs.Embed.Perl5 where

type PerlInterpreter = ()
type PerlSV = ()

constFail :: a -> IO b
constFail = const $ fail "perl5 not embedded"

initPerl5 :: String -> IO PerlInterpreter
initPerl5 _ = return ()

freePerl5 :: PerlInterpreter -> IO ()
freePerl5 _ = return ()

evalPerl5 :: String -> IO PerlSV
evalPerl5 = constFail

svToVStr :: PerlSV -> IO String
svToVStr = constFail

vstrToSV :: String -> IO PerlSV
vstrToSV = constFail

vintToSV :: Integer -> IO PerlSV
vintToSV = constFail

callPerl5 :: String -> [PerlSV] -> IO PerlSV
callPerl5 _ = constFail

canPerl5 :: PerlSV -> String -> IO Bool
canPerl5 _ = constFail

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
foreign import ccall "perl.h boot_DynaLoader"
    boot_DynaLoader :: Ptr () -> IO ()
foreign import ccall "perl5.h perl5_SvPV"
    perl5_SvPV :: PerlSV -> IO CString
foreign import ccall "perl5.h perl5_newSVpv"
    perl5_newSVpv :: CString -> IO PerlSV
foreign import ccall "perl5.h perl5_newSViv"
    perl5_newSViv :: CInt -> IO PerlSV
foreign import ccall "perl5.h perl5_call"
    perl5_call :: CString -> CInt -> Ptr PerlSV -> IO PerlSV
foreign import ccall "perl5.h perl5_can"
    perl5_can :: PerlSV -> CString -> IO Bool
foreign import ccall "perl.h perl5_eval"
    perl5_eval :: CString -> IO PerlSV
foreign import ccall "perl5.h perl5_init"
    perl5_init :: CInt -> Ptr CString -> IO PerlInterpreter

initPerl5 :: String -> IO PerlInterpreter
initPerl5 str = do
    withCString "-e" $ \prog -> withCString str $ \cstr -> do
        withArray [prog, prog, cstr] $ \argv -> do
            perl5_init 3 argv

svToVStr :: PerlSV -> IO String
svToVStr sv = peekCString =<< perl5_SvPV sv

vstrToSV :: String -> IO PerlSV
vstrToSV str = withCString str perl5_newSVpv 

vintToSV :: Integer -> IO PerlSV
vintToSV int = perl5_newSViv (fromIntegral int)

callPerl5 :: String -> [PerlSV] -> IO PerlSV
callPerl5 str args = do
    withCString str $ \cstr -> do
        withArray args $ \argv -> do
            perl5_call cstr (toEnum $ length args) argv

canPerl5 :: PerlSV -> String -> IO Bool
canPerl5 sv meth = withCString meth $ \cstr -> perl5_can sv cstr

evalPerl5 :: String -> IO PerlSV
evalPerl5 str = withCString str perl5_eval

freePerl5 :: PerlInterpreter -> IO ()
freePerl5 my_perl = do
    perl_destruct my_perl
    return ()

#endif

