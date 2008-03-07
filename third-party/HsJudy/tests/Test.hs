module Test where

import Data.List (sort)
import Foreign
import Foreign.Ptr
import IO (stderr, hPutStrLn)
import System.IO.Unsafe

-- FIXME: /me wonders if IORef is the Right Thing to do...

{-# NOINLINE intPtr #-}
intPtr :: Ptr Int
intPtr = unsafePerformIO $ new 0

{-# INLINE newTest #-}
newTest :: IO Int
newTest = do { i <- peek intPtr; poke intPtr (i+1); return (i+1) }


no_plan :: IO t -> IO ()
no_plan test = do
    test
    total <- peek intPtr
    putStrLn $ "1.." ++ (show total)

t :: String -> IO b -> IO b
t name test = say name >> test

plan :: Int -> IO ()
plan = putStrLn . ("1.." ++) . show

diag :: String -> IO ()
diag = (hPutStrLn stderr) . ("# " ++)

-- Not best name, but a three char name..
say :: String -> IO ()
say = putStrLn . ("# " ++)

infix 0 .=>, ==>, =-=, .-=

(.=>) :: (Eq t, Show t) => IO t -> t -> IO ()
(.=>) act v = do
    x <- act
    is_ x v

(==>) :: (Eq a, Show a) => a -> a -> IO ()
(==>) a b = is_ a b

(.-=) :: (Show a, Ord a) => IO [a] -> [a] -> IO ()
(.-=) act l = do
    x <- act
    is_ (sort x) (sort l)

(=-=) :: (Show a, Ord a) => [a] -> [a] -> IO ()
(=-=) a b = is_ (sort a) (sort b)

is_ :: (Show a, Eq a) => a -> a -> IO ()
is_ a b = do
    cool <- ok (a == b)
    if not cool
        then do say $ "     got: " ++ (show a)
                say $ "expected: " ++ (show b)
        else return ()

ok :: Bool -> IO Bool
ok x = do
        tst <- newTest
        if x
            then putStrLn ("ok " ++ (show tst)) >> return True
            else putStrLn ("not ok " ++ (show tst)) >> return False

check :: [Bool] -> IO ()
check l = do
    mapM_ ok l
