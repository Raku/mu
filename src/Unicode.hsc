{-# OPTIONS -fglasgow-exts -cpp -O2 #-}

{-
    Unicode internals.

    Sí na veth bâden im derel
    Vi dúath dofn tummen.
    Atham meraid velig a tynd
    Athan eryd bain beraidh.
    Or 'waith bain nura Anor
    A panlû elin cuinar
    Ú-pedithon 'i-aur gwann'
    Egor nai îl 'namarië'.
-}

module Unicode (
    module Data.Char,
    isAlpha, isAlphaNum, isSpace, isLower, isUpper, 
    toLower, toUpper,
) where

import Data.Char (digitToInt, isHexDigit, isOctDigit, isDigit)

#if mingw32_HOST_OS
import GHC.Unicode (isAlpha, isAlphaNum, isSpace, isLower, isUpper, toLower, toUpper)
#else
# include "Unicode.c"

import System.IO.Unsafe (unsafePerformIO)

foreign import ccall toUpperC :: Char -> IO Char
foreign import ccall toLowerC :: Char -> IO Char
foreign import ccall isAlphaC :: Char -> IO Bool
foreign import ccall isAlphaNumC :: Char -> IO Bool
foreign import ccall isUpperC :: Char -> IO Bool
foreign import ccall isLowerC :: Char -> IO Bool
foreign import ccall isSpaceC :: Char -> IO Bool

toUpper     = unsafePerformIO . toUpperC
toLower     = unsafePerformIO . toLowerC
isAlpha     = unsafePerformIO . isAlphaC
isAlphaNum  = unsafePerformIO . isAlphaNumC
isUpper     = unsafePerformIO . isUpperC
isLower     = unsafePerformIO . isLowerC
isSpace     = unsafePerformIO . isSpaceC

#endif
