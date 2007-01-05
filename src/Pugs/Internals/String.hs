{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans -fno-full-laziness -fno-cse -fno-warn-deprecations -fallow-undecidable-instances -fallow-overlapping-instances -funbox-strict-fields -cpp #-}

module Pugs.Internals.String (
    split,
    split_n,
    breakOnGlue,
    afterPrefix,
    decodeUTF8,
    encodeUTF8
) where

import Debug.Trace
import Pugs.Internals.Monads

split :: (Eq a) => [a] -> [a] -> [[a]]
split []  _   = internalError "splitting by an empty list"
split sep str =
   case breakOnGlue sep str of
     Just (before, after) -> before : split sep after
     Nothing -> [str]

split_n :: (Eq a) => [a] -> [a] -> Int -> [[a]]
split_n [] _ _ = internalError "splitting by an empty list"
split_n sep str n
   | n == 1 = [str]
   | otherwise =
   case breakOnGlue sep str of
       Just (before, after) -> before : split_n sep after (n-1)
       Nothing -> [str]

-- returns Nothing if the glue isn't there
breakOnGlue :: (Eq a) => [a] -> [a] -> Maybe ([a], [a])
breakOnGlue _    [] = Nothing
breakOnGlue glue list@(x:xs) =
   case afterPrefix glue list of
      Just rest -> Just ([], rest)
      Nothing -> case breakOnGlue glue xs of
                    Just (before, after) -> Just (x : before, after)
                    Nothing -> Nothing

afterPrefix :: (Eq a) => [a] -> [a] -> Maybe [a]
afterPrefix []     list = Just list
afterPrefix _      []   = Nothing  -- non-empty prefix of an empty list
afterPrefix (p:ps) (x:xs)
   | p == x = afterPrefix ps xs
   | otherwise = Nothing

{-# INLINE decodeUTF8 #-}
decodeUTF8 :: String -> String
decodeUTF8 xs = concatMap decodeUTF8' (chunk 4096 xs)

{-# INLINE decodeUTF8' #-}
decodeUTF8' :: String -> String
decodeUTF8' [] = []
decodeUTF8' (c:cs)
    | c < '\x80'
    = let rest = decodeUTF8' cs
       in seq rest
          (c:rest)
decodeUTF8' (c:d:cs)
    | '\xC0' <= c, c <= '\xDF'
    , '\x80' <= d, d <= '\xBF'
    = let rest = decodeUTF8' cs
       in seq rest
          ( toEnum ( (fromEnum c `mod` 0x20) * 0x40
                   + fromEnum d `mod` 0x40
                   )
          : rest
          )
decodeUTF8' (c:d:e:cs)
    | '\xE0' <= c, c <= '\xEF'
    , '\x80' <= d, d <= '\xBF'
    , '\x80' <= e, e <= '\xBF'
    = let rest = decodeUTF8' cs
       in seq rest
          ( toEnum ( (fromEnum c `mod` 0x10 * 0x1000)
                   + (fromEnum d `mod` 0x40) * 0x40
                   + fromEnum e `mod` 0x40
                   )
          : rest
          )
decodeUTF8' (c:d:e:f:cs)
    | '\xF0' <= c, c <= '\xF7'
    , '\x80' <= d, d <= '\xBF'
    , '\x80' <= e, e <= '\xBF'
    , '\x80' <= f, f <= '\xBF'
    = let rest = decodeUTF8' cs
       in seq rest
          ( toEnum ( (fromEnum c `mod` 0x10 * 0x40000)
                   + (fromEnum d `mod` 0x40) * 0x1000
                   + (fromEnum e `mod` 0x40) * 0x40
                   + fromEnum f `mod` 0x40
                   )
          : rest
          )
decodeUTF8' (x:xs) = trace ("decodeUTF8': bad data: " ++ show x) (x:decodeUTF8' xs)

{-# INLINE chunk #-}
chunk :: Int -> [a] -> [[a]]
chunk _    [] = []
chunk size xs = case splitAt size xs of (xs', xs'') -> xs' : chunk size xs''

{-# INLINE encodeUTF8 #-}
encodeUTF8 :: String -> String
encodeUTF8 xs = concatMap encodeUTF8' (chunk 4096 xs)

{-# INLINE encodeUTF8' #-}
encodeUTF8' :: String -> String
encodeUTF8' [] = []
-- In the \0 case, we diverge from the Unicode standard to remove any trace
-- of embedded nulls in our bytestrings, to allow the use of Judy.StrMap
-- and to make passing CString around easier.  See Java for the same treatment:
-- http://java.sun.com/j2se/1.5.0/docs/api/java/io/DataInput.html#modified-utf-8
encodeUTF8' ('\0':cs)
    = let rest = encodeUTF8' cs
       in seq rest
          ('\xC0':'\x80':rest)
encodeUTF8' (c:cs)
    | c < '\x80'
    = let rest = encodeUTF8' cs
       in seq rest
          (c:rest)
    | c < '\x800'
    = let i     = fromEnum c
          rest  = encodeUTF8' cs
       in seq rest
          ( toEnum (0xC0 + i `div` 0x40)
          : toEnum (0x80 + i `mod` 0x40)
          : rest
          )
    | c < '\x10000'
    = let i     = fromEnum c
          rest  = encodeUTF8' cs
       in seq rest
          ( toEnum (0xE0 + i `div` 0x1000)
          : toEnum (0x80 + (i `div` 0x40) `mod` 0x40)
          : toEnum (0x80 + i `mod` 0x40)
          : rest
          )
    | otherwise
    = let i     = fromEnum c
          rest  = encodeUTF8' cs
       in seq rest
          ( toEnum (0xF0 + i `div` 0x40000)
          : toEnum (0x80 + (i `div` 0x1000) `mod` 0x40)
          : toEnum (0x80 + (i `div` 0x40) `mod` 0x40)
          : toEnum (0x80 + i `mod` 0x40)
          : rest
          )

