module Rule
               ( -- complete modules
                 module Rule.Prim
               , module Rule.Combinator
               , module Rule.Char
               
               -- module Rule.Error
               , ParseError   
               , errorPos   
               
               -- module Rule.Pos
               , SourcePos
               , SourceName, Line, Column             
               , sourceName, sourceLine, sourceColumn             
               , incSourceLine, incSourceColumn
               , setSourceLine, setSourceColumn, setSourceName

             ) where

import Rule.Pos            -- textual positions
import Rule.Error          -- parse errors
import Rule.Prim           -- primitive combinators
import Rule.Combinator     -- derived combinators
import Rule.Char           -- character parsers

