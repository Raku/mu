{-# OPTIONS -fglasgow-exts -cpp -fvia-c #-}

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
    digitToInt, isHexDigit, isOctDigit, isDigit,
    isAlpha, isAlphaNum, isSpace, isLower, isUpper, 
    toLower, toUpper,
) where

import Data.Char (digitToInt, isHexDigit, isOctDigit, isDigit)

#if mingw32_HOST_OS
import GHC.Unicode (isAlpha, isAlphaNum, isSpace, isLower, isUpper, toLower, toUpper)
#else
#def int isUpperC (unsigned int c)
#def int isLowerC (unsigned int c)
#def int isSpaceC (unsigned int c)
#def int isAlphaC (unsigned int c)
#def int isAlphaNumC (unsigned int c)
#def unsigned int toUpperC (unsigned int c)
#def unsigned int toLowerC (unsigned int c)
#include "Unicode.c"

import Data.Char (ord, chr)
import System.IO.Unsafe (unsafePerformIO)

foreign import ccall toUpperC :: Int -> IO Int
foreign import ccall toLowerC :: Int -> IO Int
foreign import ccall isAlphaC :: Int -> IO Bool
foreign import ccall isAlphaNumC :: Int -> IO Bool
foreign import ccall isUpperC :: Int -> IO Bool
foreign import ccall isLowerC :: Int -> IO Bool
foreign import ccall isSpaceC :: Int -> IO Bool

toUpper     = chr . unsafePerformIO . toUpperC . ord
toLower     = chr . unsafePerformIO . toLowerC . ord
isAlpha     = unsafePerformIO . isAlphaC . ord
isAlphaNum  = unsafePerformIO . isAlphaNumC . ord
isUpper     = unsafePerformIO . isUpperC . ord
isLower     = unsafePerformIO . isLowerC . ord
isSpace     = unsafePerformIO . isSpaceC . ord

#endif
