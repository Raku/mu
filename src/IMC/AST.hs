{-# OPTIONS_GHC -fglasgow-exts -fth #-}

module Pugs.IMC.AST where
import Language.Haskell.TH
import Language.Haskell.TH.Ppr

type Statement = ()
type Sub = ()
type Op = String
type SubName = String

class Value a
instance Value String

data Term a where
    TStr     :: String -> Term String
    TOp1     :: (Show a, Value a) => Op -> Term a -> Term Statement
    TSub     :: SubName  -> [Term Statement] -> Term Sub

compile :: ExpQ -> ExpQ
compile str = [| putStrLn $ "Hello, " ++ $str ++ "!" |]

-- Haskell Equivalent of a BEGIN block!
{-
compile str = do
    runIO $ putStrLn "Please enter something!"
    -- line <- runIO getLine
    [| putStrLn $ "Hello, " ++ $str ++ "!" |]
-}

imcRun :: Term a -> IO a
imcRun (TStr a) = return a
imcRun (TOp1 "print" str) = do
    foo <- imcRun str
    putStrLn $ read $ show foo
imcRun (TSub _ stmts) = do
    mapM_ imcRun stmts

imcCompile :: Term a -> ExpQ
imcCompile (TStr a) = [| return a |]
imcCompile (TOp1 "print" (TStr str)) = [| putStrLn str |]
imcCompile (TSub _ stmts) = let foo = map imcCompile stmts in
    [| sequence_ $(listE foo) |]

