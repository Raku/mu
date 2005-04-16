{-# OPTIONS_GHC -fglasgow-exts -cpp #-}

{-
    Embedded interpreters.

    As Beren looked into her eyes
    Within the shadows of her hair,
    The trembling starlight of the skies
    He saw there mirrored shimmering...
-}

module Pugs.Embed (
    module Pugs.Embed.Perl5,
    module Pugs.Embed.Haskell
    -- module Pugs.Embed.Parrot
    -- module Pugs.Embed.Ponie
) where
import Pugs.Embed.Perl5
import Pugs.Embed.Haskell
-- import Pugs.Embed.Parrot
-- import Pugs.Embed.Ponie

