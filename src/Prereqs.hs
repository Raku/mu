{-# OPTIONS_GHC -fvia-C -fglasgow-exts #-}

{-|
    FFI prerequisites that need to be compiled before "make ghci".

>   ...some poetry here...
-}

module Prereqs where

import Pugs.Compat ()
import Pugs.Embed.Perl5 ()
import Pugs.Meta ()
import Pugs.Run.Perl5 ()
import Pugs.Parser.Charnames ()
import Pugs.Parser.Program ()
import Pugs.Lexer ()
import Data.ByteString ()
import Data.ByteString.Char8 ()
import Data.Yaml.Syck ()

main :: IO a
main = main
