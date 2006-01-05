{-# OPTIONS_GHC -fglasgow-exts #-}

-- This monadic version based on some excellent suggestions
-- and code sent to asavige by Cale Gibbard.

module Rpn (evaluate) where

import Data.Char
import Control.Monad
import Control.Monad.Error

isStrDigit :: String -> Bool
isStrDigit = all isDigit

-- Check that a string matches regex /^-?\d+$/.
isSNum :: String -> Bool
isSNum []        = False
isSNum "-"       = False
isSNum ('-':xs)  = isStrDigit xs
isSNum xs        = isStrDigit xs

calc :: Int -> String -> Int -> Either String Int
calc x "+" y  = return $ x + y
calc x "-" y  = return $ x - y
calc x "*" y  = return $ x * y
calc x "/" y  = return $ x `div` y
calc _ tok _  = throwError $ "Invalid token:" ++ show tok

evalStack :: [Int] -> String -> Either String [Int]
evalStack xs y
  | isSNum y        = return $ (read y):xs
  | (a:b:cs) <- xs  = do c <- calc b y a
                         return (c:cs)
  | otherwise       = throwError "Stack underflow"

evaluate :: String -> Either String Int
evaluate expr =
    do xs <- foldM evalStack [] $ words expr
       case xs of
          [x] -> return x
          _   -> throwError $ "Invalid stack:" ++ show xs
