{-# OPTIONS_GHC -fglasgow-exts -Wall #-}

-- The RPN evaluator algorithm was originally based on:
--   http://en.wikipedia.org/wiki/Haskell_programming_language
-- However, the error handling in that algorithm was not to my
-- liking, so this module has evolved since to mimic the method
-- used in the Perl versions.

-- Note that a Stack abstract data type seems unnecessary in
-- Haskell since built-in lists can be used as a stack.
-- (Indeed, I couldn't find a stack abstract data type
-- anywhere in the GHC libraries).

module Rpn (evaluate) where

import Char
-- import Debug.Trace

-- Check that a string consists of just digits.
isStrDigit :: String -> Bool
isStrDigit = all isDigit

-- Check that a string matches regex /^-?\d+$/.
isSNum :: String -> Bool
isSNum []        = False
isSNum "-"       = False
isSNum ('-':xs)  = isStrDigit xs
isSNum xs        = isStrDigit xs

calc :: Int -> String -> Int -> Int
calc x "+" y  = x+y
calc x "-" y  = x-y
calc x "*" y  = x*y
calc x "/" y  = x`div`y
calc _ tok _  = error $ "Invalid token:" ++ show tok

evalStack :: [Int] -> String -> [Int]
evalStack xs y
  | isSNum y        = (read y):xs
  | (a:b:cs) <- xs  = (calc b y a):cs
  | otherwise       = error "Stack underflow"

evaluate :: String -> Int
evaluate expr
  | [e] <- el       = e
  | otherwise       = error $ "Invalid stack:" ++ show el
  where
  el = foldl evalStack [] $ words expr

-- Example trace line
--   f (x:y:zs) "+" = trace ("k+" ++ show x ++ ":" ++ show y ++ ":" ++ show zs) (y+x:zs)
