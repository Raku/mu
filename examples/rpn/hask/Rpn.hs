{-# OPTIONS_GHC -fglasgow-exts #-}

module Rpn where

import Char
-- import Debug.Trace

-- Check that a string consists of just digits.
isStrDigit :: String -> Bool
isStrDigit = and . map isDigit

-- Check that a string matches regex /-?\d+/.
isSNum :: String -> Bool
isSNum []        = False
isSNum ['-']     = False
isSNum ('-':xs)  = isStrDigit xs
isSNum xs        = isStrDigit xs

-- Algorithm based on:
-- http://en.wikipedia.org/wiki/Haskell_programming_language
evalList :: String -> [Int]
evalList = foldl f [] . words
             where 
               f (x:y:zs) "+"   = y+x:zs
               f (x:y:zs) "-"   = y-x:zs
               f (x:y:zs) "*"   = y*x:zs
               f (x:y:zs) "/"   = y`div`x:zs
               f xs y           = (chkInt y):xs
                 where chkInt s = if isSNum s then
                                    read s
                                  else
                                    error ("Invalid token:" ++ show s)

evaluate :: String -> Int
evaluate expr
  | length el == 1  = head el
  | otherwise       = error ("Invalid stack:" ++ show el)
    where el = evalList expr
  
-- Example trace line
--   f (x:y:zs) "+" = trace ("k+" ++ show x ++ ":" ++ show y ++ ":" ++ show zs) (y+x:zs)
