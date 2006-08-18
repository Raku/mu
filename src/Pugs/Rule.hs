{-# OPTIONS_GHC -fglasgow-exts #-}

{-|
    Rule-based grammar engine.

>   From the ashes a fire shall be woken
>   A light from the shadows shall spring
>   Renewed shall be blade that was broken
>   The crownless again shall be king.

-}


module Pugs.Rule (
    module Text.ParserCombinators.Parsec,
) where

import Text.ParserCombinators.Parsec hiding (
    token       ,
    char        ,
    octDigit    ,
    satisfy     ,
    oneOf       ,
    string      ,
    anyChar     ,
    upper       ,
    hexDigit    ,
    digit       ,
    noneOf      ,
    )
