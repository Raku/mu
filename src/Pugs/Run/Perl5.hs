{-# OPTIONS_GHC -fglasgow-exts -cpp -fno-warn-unused-imports #-}

module Pugs.Run.Perl5 () where

#ifdef PUGS_HAVE_PERL5

import Pugs.Internals
import Pugs.AST
import Pugs.Embed.Perl5
import Foreign

foreign export ccall "pugs_MkSvRef"
    mkSvRef :: PerlSV -> IO PugsVal

mkSvRef :: PerlSV -> IO PugsVal
mkSvRef sv = fmap castStablePtrToPtr $ newStablePtr (PerlSV sv)

#endif
