{-# OPTIONS -fglasgow-exts -cpp -fvia-c -fno-implicit-prelude #-}
{-# OPTIONS -#include "UnicodeC.h" #-}
{-# OPTIONS -#include "UnicodeC.c" #-}

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

import GHC.Base
import GHC.Real  (fromIntegral)
import GHC.Num   (fromInteger)
import Foreign.C.Types (CUInt)

foreign import ccall unsafe "stg_hack_toUpperC"
    toUpperH :: CUInt -> CUInt
foreign import ccall unsafe "stg_hack_toLowerC"
    toLowerH :: CUInt -> CUInt
foreign import ccall unsafe "stg_hack_isAlphaC"
    isAlphaH :: CUInt -> CUInt
foreign import ccall unsafe "stg_hack_isAlphaNumC"
    isAlphaNumH :: CUInt -> CUInt
foreign import ccall unsafe "stg_hack_isUpperC"
    isUpperH :: CUInt -> CUInt
foreign import ccall unsafe "stg_hack_isLowerC"
    isLowerH :: CUInt -> CUInt
foreign import ccall unsafe "stg_hack_isSpaceC"
    isSpaceH :: CUInt -> CUInt

toUpper     = chr . fromIntegral . toUpperH . fromIntegral . ord
toLower     = chr . fromIntegral . toLowerH . fromIntegral . ord
isAlpha     = (/= 0) . isAlphaH . fromIntegral . ord
isAlphaNum  = (/= 0) . isAlphaNumH . fromIntegral . ord
isUpper     = (/= 0) . isUpperH . fromIntegral . ord
isLower     = (/= 0) . isLowerH . fromIntegral . ord
isSpace     = (/= 0) . isSpaceH . fromIntegral . ord

#endif
