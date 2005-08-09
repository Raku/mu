{-# OPTIONS_GHC -fglasgow-exts #-}

module PIL.Parser (parse) where
import PIL.Syn
import PIL.Monads
import PIL.Internals
import Pugs.Rule hiding (parse)

-- Parsing needs to handle BEGIN and such.
parse :: String -> Parse Syn
parse = undefined

