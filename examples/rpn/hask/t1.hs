{-# OPTIONS_GHC -fglasgow-exts #-}

-- t1.hs
-- build with: ghc --make -o t1 t1.hs Rpn.hs
-- run with:   ./t1

module Main where

import Test.HUnit        -- GHC version of HUnit
-- import HUnit          -- hunit.sourceforge.net version of HUnit
-- dons drop-in replacement for HUnit
-- import TestFramework
-- import Test.HUnit hiding (runTestTT)

import Control.Exception
import Rpn

-- fst String is rpn expression, snd Int is result of evaluating it.
type NormalExpected = (String, Int)

-- Convert a NormalExpected into a TestCase.
makeNormalTest :: NormalExpected -> Test
makeNormalTest e = TestCase ( assertEqual "" (snd e) (Rpn.evaluate (fst e)) )

-- Take a list of NormalExpected and return a list of TestCases.
makeNormalTestList :: [NormalExpected] -> [Test]
makeNormalTestList = map makeNormalTest

normalTests = TestList ( makeNormalTestList [
    ( "1 -2 -", 3 ),
    ( "1 2 +", 3 ),
    ( "-1 2 +", 1 ),
    ( "-1 2 +", 1 ),
    ( "1 2 -", -1 ),
    ( "1 2 + 3 -", 0 ),
    ( "1 2 - 3 -", -4 ),
    ( "1 2 +", 3 ),
    ( "1 2 - 3 -", -4 ),
    ( "1 2 - 5 +", 4 ),
    ( "1 2 - 5 + 2 -", 2 ),
    ( "1 1 1 1 1 2 + + + + +", 7 ),
    ( "1 -5 +", -4 ),
    ( "5 3 *", 15 ),
    ( "-2 -5 *", 10 ),
    ( "2 -5 *", -10 ),
    ( "6 4 /", 1 ),
    ( "0 1 /", 0 ),
    ( "1 0 *", 0 ),
    ( "00 1 +", 1 ),
    ( "1 00 -", 1 ),
    ( "00", 0 ),
    ( "-00", 0 ),
    ( "-0001", -1 ),
    ( "010", 10 ),
    ( "1 2 3 * +", 7 ),
    ( "999 888 -", 111 ),
    ( "3 4 * 2 3 * +", 18 ),
    ( "3 4 * 2 / 3 *", 18 ),
    ( "3 4 * 5 / 3 *", 6 ),
    ( "3 4 / 6 * 3 /", 0 ),
    ( "3 4 / 6 * 3 * 4 * 5 * 6 * 78 *", 0 ),
    ( "12 1 / 2 /", 6 ),
    ( "3 4 * 2 3 * /", 2 ),
    ( "4 2 3 1 1 + - * -", 2 ),
    ( "1 2 * 3 * 4 * 5 * 6 * 7 * 6 / 5 / 4 / 3 /", 14 ),
    ( "0 6 * -0 5 / + 1 + 00000 - -2 -", 3 ),
    ( "05 5 06 * 2 / + 7 -", 13 ),
    ( "999999 1000 / 67 * 56 80 * 8 * -", 31093 ),
    ( "1 2 * 3 * 3 2 * 1 * - 4 5 / + 5 4 / - 9 6 * 6 / 9 * +", 80 ),
    ( "9998999 1000 / 67 * 56 80 * 8000 * - 6666 6969 * + 4657 250 / 780 * 890 * -", -1210380 ),
    ( "2 3 *", 6 ),
    ( "5 4 +", 9 ),
    ( "42", 42 )
  ])

-- Exception wrappers for Rpn.evaluate
-- The idea is that I want to catch calls to the error function
-- and verify that the expected error string was indeed written.
-- (These wrappers are incredibly fragile because I don't have
-- a clue what I'm doing;-).
evaluateWrap :: String -> IO String
evaluateWrap x = do let r = Rpn.evaluate x
                    putStr (show r)       -- fails without this!! why?
                    return (show r)

evaluateWrap2 :: String -> IO String
evaluateWrap2 x = do res <- tryJust errorCalls (evaluateWrap x)
                     case res of
                       Right r  -> return (show r)
                       Left  r  -> return r

-- fst is rpn expression, snd is error string produced when evaluating it.
type ExceptionExpected = (String, String)

-- Convert an ExceptionExpected into a TestCase.
makeExceptionTest :: ExceptionExpected -> Test
makeExceptionTest e = TestCase ( do x <- evaluateWrap2 (fst e)
                                    assertEqual "" (snd e) x )

-- Take a list of ExceptionExpected and return a list of TestCases.
makeExceptionTestList :: [ExceptionExpected] -> [Test]
makeExceptionTestList = map makeExceptionTest

exceptionTests = TestList ( makeExceptionTestList [
    ( "5 4 %",     "Invalid token:\"%\"" ),
    ( "5 +",       "Stack underflow" ),
    ( "+",         "Stack underflow" ),
    ( "5 4 + 42",  "Invalid stack:[42,9]" ),
    ( "",          "Invalid stack:[]" )
  ])

main = do runTestTT normalTests
          runTestTT exceptionTests
