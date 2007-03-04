{-# OPTIONS_GHC -fglasgow-exts #-}

{-|
    Perl 6 Meta-object definitions.

>   And though I oft have passed them by,
>   A day will come at last when I
>   Shall take the hidden paths that run
>   West of the Moon, East of the Sun.
-}

module Pugs.Meta
    ( module Pugs.Meta
    , module Pugs.Meta.Str
    , module Pugs.Meta.Perl5
--  , module Pugs.Meta.Class
    ) where

import Pugs.Meta.Str
import Pugs.Meta.Perl5
import Pugs.Meta.Class ()

-- XXX - Stub definition to work around ghci bug
pugsMetaLoaded :: ()
pugsMetaLoaded = ()
