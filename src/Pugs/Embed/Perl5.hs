{-# OPTIONS_GHC -fglasgow-exts -cpp #-}

#ifndef PUGS_HAVE_PERL5
module Pugs.Embed.Perl5 where
import Foreign.C.Types

type PerlInterpreter = ()
type PerlSV = ()

constFail :: a -> IO b
constFail = const $ fail "perl5 not embedded"

initPerl5 :: String -> Maybe a -> IO PerlInterpreter
initPerl5 _ _ = return ()

freePerl5 :: PerlInterpreter -> IO ()
freePerl5 _ = return ()

evalPerl5 :: String -> PerlSV -> CInt -> IO PerlSV
evalPerl5 _ _ = constFail

svToVStr :: PerlSV -> IO String
svToVStr = constFail

svToVInt :: (Num a) => PerlSV -> IO a
svToVInt = constFail

svToVNum :: (Fractional a) => PerlSV -> IO a
svToVNum = constFail

svToVBool :: PerlSV -> IO Bool
svToVBool = constFail

svToVal :: PerlSV -> IO a
svToVal = constFail

mkValRef :: a -> IO PerlSV
mkValRef = constFail

vstrToSV :: String -> IO PerlSV
vstrToSV = constFail

vintToSV :: (Integral a) => a -> IO PerlSV
vintToSV = constFail

vnumToSV :: (Real a) => a -> IO PerlSV
vnumToSV = constFail

callPerl5 :: PerlSV -> Maybe PerlSV -> [PerlSV] -> PerlSV -> CInt -> IO PerlSV
callPerl5 _ _ _ _ = constFail

canPerl5 :: PerlSV -> String -> IO Bool
canPerl5 _ = constFail

nullSV :: PerlSV
nullSV = error "perl5 not embedded"

#else

{-# INCLUDE <perl5.h> #-}
{-# INCLUDE <pugsembed.h> #-}

module Pugs.Embed.Perl5 where
import Foreign
import Foreign.C.Types
import Foreign.C.String

type PerlInterpreter = Ptr ()
type PerlSV = Ptr ()
type PugsVal = Ptr ()

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
foreign import ccall "perl5.h perl5_SvIV"
    perl5_SvIV :: PerlSV -> IO CInt
foreign import ccall "perl5.h perl5_SvNV"
    perl5_SvNV :: PerlSV -> IO CDouble
foreign import ccall "perl5.h perl5_SvTRUE"
    perl5_SvTRUE :: PerlSV -> IO Bool
foreign import ccall "perl5.h perl5_newSVpv"
    perl5_newSVpv :: CString -> IO PerlSV
foreign import ccall "perl5.h perl5_newSViv"
    perl5_newSViv :: CInt -> IO PerlSV
foreign import ccall "perl5.h perl5_newSVnv"
    perl5_newSVnv :: CDouble -> IO PerlSV
foreign import ccall "perl5.h perl5_get_sv"
    perl5_get_sv :: CString -> IO PerlSV
foreign import ccall "perl5.h perl5_set_svref"
    perl5_set_svref :: CString -> PugsVal -> IO ()
foreign import ccall "perl5.h perl5_apply"
    perl5_apply :: PerlSV -> PerlSV -> Ptr PerlSV -> PerlSV -> CInt -> IO PerlSV
foreign import ccall "perl5.h perl5_can"
    perl5_can :: PerlSV -> CString -> IO Bool
foreign import ccall "perl.h perl5_eval"
    perl5_eval :: CString -> PerlSV -> CInt -> IO PerlSV
foreign import ccall "perl5.h perl5_init"
    perl5_init :: CInt -> Ptr CString -> IO PerlInterpreter

foreign import ccall "pugsembed.h pugs_SvToVal"
    pugs_SvToVal :: PerlSV -> IO PugsVal
foreign import ccall "pugsembed.h pugs_MkValRef"
    pugs_MkValRef :: PugsVal -> IO PerlSV

initPerl5 :: (Show a) => String -> Maybe a -> IO PerlInterpreter
initPerl5 str env = do
    withCString "-e" $ \prog -> withCString str $ \cstr -> do
        withArray [prog, prog, cstr] $ \argv -> do
            interp <- perl5_init 3 argv
            case env of
                Just val -> withCString "pugs::env" $ \name -> do
                    ptr <- fmap castStablePtrToPtr $ newStablePtr val
                    perl5_set_svref name ptr
                Nothing -> return ()
            return interp

svToVStr :: PerlSV -> IO String
svToVStr sv = peekCString =<< perl5_SvPV sv

svToVInt :: (Num a) => PerlSV -> IO a
svToVInt sv = fmap fromIntegral $ perl5_SvIV sv

svToVNum :: (Fractional a) => PerlSV -> IO a
svToVNum sv = fmap realToFrac $ perl5_SvNV sv

svToVBool :: PerlSV -> IO Bool
svToVBool = perl5_SvTRUE

svToVal :: (Show a) => PerlSV -> IO a
svToVal sv = do
    ptr <- pugs_SvToVal sv
    deRefStablePtr (castPtrToStablePtr ptr)

mkValRef :: a -> IO PerlSV
mkValRef x = do
    ptr <- fmap castStablePtrToPtr $ newStablePtr x
    pugs_MkValRef ptr

vstrToSV :: String -> IO PerlSV
vstrToSV str = withCString str perl5_newSVpv 

vintToSV :: (Integral a) => a -> IO PerlSV
vintToSV int = perl5_newSViv (fromIntegral int)

vnumToSV :: (Real a) => a -> IO PerlSV
vnumToSV int = perl5_newSVnv (realToFrac int)

callPerl5 :: PerlSV -> PerlSV -> [PerlSV] -> PerlSV -> CInt -> IO PerlSV
callPerl5 sub inv args env cxt = do
    withArray0 nullPtr args $ \argv -> do
        perl5_apply sub inv argv env cxt

canPerl5 :: PerlSV -> String -> IO Bool
canPerl5 sv meth = withCString meth $ \cstr -> perl5_can sv cstr

evalPerl5 :: String -> PerlSV -> CInt -> IO PerlSV
evalPerl5 str env cxt = withCString str $ \cstr -> perl5_eval cstr env cxt

freePerl5 :: PerlInterpreter -> IO ()
freePerl5 my_perl = do
    perl_destruct my_perl
    return ()

nullSV :: PerlSV
nullSV = nullPtr

#endif

