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


no_plan test = do
    test
    total <- peek intPtr
    putStrLn $ "1.." ++ (show total)

t name test = say name >> test

plan :: Int -> IO ()
plan = putStrLn . ("1.." ++) . show

diag = (hPutStrLn stderr) . ("# " ++)

-- Not best name, but a three char name..
say = putStrLn . ("# " ++)

infix 0 .=>, ==>, =-=, .-=


(.=>) act v = do
    x <- act
    is_ x v

(==>) a b = is_ a b

(.-=) act l = do
    x <- act
    is_ (sort x) (sort l)

(=-=) a b = is_ (sort a) (sort b)

is_ a b = do
    cool <- ok (a == b)
    if not cool
        then do say $ "     got: " ++ (show a)
                say $ "expected: " ++ (show b)
        else return ()

ok x = do
        t <- newTest
        if x
            then putStrLn ("ok " ++ (show t)) >> return True
            else putStrLn ("not ok " ++ (show t)) >> return False

check l = do
    mapM_ ok l
