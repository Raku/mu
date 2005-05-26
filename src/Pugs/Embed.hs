{-# OPTIONS_GHC -fglasgow-exts -cpp #-}

{-|
    Embedded interpreters.

>   As Beren looked into her eyes
>   Within the shadows of her hair,
>   The trembling starlight of the skies
>   He saw there mirrored shimmering...
-}

module Pugs.Embed (
    module Pugs.Embed.Perl5,
    module Pugs.Embed.Haskell,
    module Pugs.Embed.Parrot,
    evalEmbedded
    -- module Pugs.Embed.Ponie,
) where
import Pugs.Embed.Perl5
import Pugs.Embed.Haskell
import Pugs.Embed.Parrot
-- import Pugs.Embed.Ponie

evalEmbedded :: String -> String -> IO ()
evalEmbedded "Parrot"  code = do
    evalParrot code
{- evalEmbedded "Haskell" code = do
    evalHaskell code
    return () -}
evalEmbedded "Perl5" code = do
    interp <- initPerl5 ""
    evalPerl5 code 0
    freePerl5 interp
evalEmbedded s _ = fail $ "Cannot evaluate in " ++ s
