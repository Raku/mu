{-# OPTIONS_GHC -fglasgow-exts #-}

{-|
    Rule-based grammar engine.

>   From the ashes a fire shall be woken
>   A light from the shadows shall spring
>   Renewed shall be blade that was broken
>   The crownless again shall be king.

Based on the Parsec library, Copyright 1999-2000, Daan Leijen.
Parsec may be found at <http://www.cs.uu.nl/~daan/parsec.html>

> See src/Pugs/Rule/LICENSE for the full license text.

-}


module Pugs.Rule
               ( -- complete modules
                 module Pugs.Rule.Prim
               , module Pugs.Rule.Combinator
               , module Pugs.Rule.Char
               
               -- module Pugs.Rule.Error
               , ParseError   
               , errorPos   
               
               -- module Pugs.Rule.Pos
               , SourcePos
               , SourceName, Line, Column             
               , sourceName, sourceLine, sourceColumn             
               , incSourceLine, incSourceColumn
               , setSourceLine, setSourceColumn, setSourceName

             ) where

import Pugs.Rule.Pos            -- textual positions
import Pugs.Rule.Error          -- parse errors
import Pugs.Rule.Prim           -- primitive combinators
import Pugs.Rule.Combinator     -- derived combinators
import Pugs.Rule.Char           -- character parsers

