{-# OPTIONS_GHC -fglasgow-exts #-}

module PIL.PIL where
import PIL.Exp

data PIL = MkPIL Exp
    deriving (Show)
