{-# OPTIONS_GHC -fglasgow-exts #-}

module PIL.PIL where
import PIL.Syn

data PIL = MkPIL Exp
    deriving (Show)
