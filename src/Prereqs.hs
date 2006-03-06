{-# OPTIONS_GHC -fvia-C -fglasgow-exts #-}

{-|
    FFI prerequisites that need to be compiled before "make ghci".

>   ...some poetry here...
-}

import Pugs.Compat ()
import Pugs.Embed.Parrot ()
import Pugs.Embed.Perl5 ()
import Pugs.Run.Perl5 ()
import Data.FastPackedString ()
import Data.Yaml.Syck ()

main :: IO a
main = main
