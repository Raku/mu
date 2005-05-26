{-# OPTIONS_GHC -fglasgow-exts -cpp -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Pugs.Run.Perl5 () where

#ifdef PUGS_HAVE_PERL5

import Pugs.Internals
import Pugs.AST
import Pugs.Embed.Perl5
import Foreign
import Foreign.C.Types
import Foreign.C.String

foreign export ccall "pugs_MkSvRef"
    mkSvRef :: PerlSV -> IO PugsVal

foreign export ccall "pugs_IvToVal"
    ivToVal :: CInt -> IO PugsVal

foreign export ccall "pugs_NvToVal"
    nvToVal :: CDouble -> IO PugsVal

foreign export ccall "pugs_PvToVal"
    pvToVal :: CString -> IO PugsVal

mkVal :: Val -> IO PugsVal
mkVal val = fmap castStablePtrToPtr $ newStablePtr val

mkSvRef :: PerlSV -> IO PugsVal
mkSvRef = mkVal . PerlSV

ivToVal :: CInt -> IO PugsVal
ivToVal = mkVal . VInt . fromIntegral

nvToVal :: CDouble -> IO PugsVal
nvToVal = mkVal . VNum . realToFrac

pvToVal :: CString -> IO PugsVal
pvToVal = (mkVal . VStr =<<) . peekCString

#endif
