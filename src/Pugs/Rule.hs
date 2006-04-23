{-# OPTIONS_GHC -fglasgow-exts #-}

{-|
    Rule-based grammar engine.

>   From the ashes a fire shall be woken
>   A light from the shadows shall spring
>   Renewed shall be blade that was broken
>   The crownless again shall be king.

-}


module Pugs.Rule (
    module Pugs.Rule.Expr,
    module Text.ParserCombinators.Parsec,
    LanguageDef, javaStyle,
) where

import Pugs.Rule.Expr
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos
import Text.ParserCombinators.Parsec.Language
