{-# OPTIONS_GHC -cpp -fglasgow-exts -fno-warn-orphans -fallow-overlapping-instances -funbox-strict-fields -fallow-undecidable-instances #-}

module SimpleMod where

import Data.Typeable

data Simplest
    deriving Typeable {-!derive: Perl6Class, MooseClass!-}

data Simple
    = Variant1
    | Variant2
    | Variant3
    deriving Typeable {-!derive: Perl6Class, MooseClass!-}

data PosAttr
    = PosClass  Int [Int] String
    | PosClass2 Rational Int String
    deriving Typeable {-!derive: Perl6Class, MooseClass!-}

data RecAttr
    = MkRec
        { att1 :: [[Int]]
        , att2 :: Int
        , att3 :: String
        }
    | MkAltRec
        { att1 :: [[Int]]
        , atta :: Rational
        , attb :: String
        }
    deriving Typeable {-!derive: Perl6Class, MooseClass!-}


