{-# OPTIONS -fglasgow-exts -cpp -O #-}
{-# INCLUDE <EXTERN.h> #-}
{-# INCLUDE <perl.h> #-}

module Embed.Perl5 where
import Foreign
import Foreign.C.Types
import Foreign.C.String

type PerlInterpreter = Ptr ()

foreign import ccall "perl.h perl_alloc"
    perl_alloc :: IO PerlInterpreter
foreign import ccall "perl.h perl_construct"
    perl_construct :: PerlInterpreter -> IO ()
foreign import ccall "perl.h perl_parse"
    perl_parse :: PerlInterpreter -> FunPtr () -> CInt -> Ptr CString -> Ptr CString -> IO CInt
foreign import ccall "perl.h perl_run"
    perl_run :: PerlInterpreter -> IO CInt
foreign import ccall "perl.h perl_destruct"
    perl_destruct :: PerlInterpreter -> IO CInt
foreign import ccall "perl.h perl_free"
    perl_free :: PerlInterpreter -> IO ()
foreign import ccall "perl.h eval_pv"
    eval_pv :: CString -> Word32 -> IO ()

initPerl5 :: String -> IO PerlInterpreter
initPerl5 str = do
    my_perl <- perl_alloc
    perl_construct my_perl
    withCString "-e" $ \prog -> withCString str $ \cstr -> do
        withArrayLen [prog, prog, cstr] $ \argc argv -> do
            perl_parse my_perl nullFunPtr (fromIntegral argc) argv nullPtr
            perl_run my_perl
    return my_perl

evalPerl5 :: String -> IO ()
evalPerl5 str = do
    withCString str $ \cstr -> do
        eval_pv cstr 1
    return ()

freePerl5 :: PerlInterpreter -> IO ()
freePerl5 my_perl = do
    perl_destruct my_perl
    return ()

