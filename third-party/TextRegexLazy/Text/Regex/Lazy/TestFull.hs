module Main (main) where

{- By Chris Kuklewicz, 2006. BSD License, see the LICENSE file. -}

import Test.HUnit
--import Text.Regex (mkRegex, matchRegex, matchRegexAll)
--import Text.Regex.Lazy.ReadRegex
--import Debug.Trace
import qualified Text.Regex.Lazy.All as P
import Control.Monad.Identity
trace a b = b

type AllRes = Maybe (String, String, String, [String])
type Res = Maybe [String]

options = P.defaultRegexOption {P.strategy = P.Find_FirstLeft}

match :: String -> String -> Res
match = P.matchRegex . runIdentity . (P.mkRegexWithOptions options)

match_all :: String -> String -> AllRes
match_all = P.matchRegexAll . runIdentity . (P.mkRegexWithOptions options)

main :: IO ()
main = do
          putStrLn "Us (laziness):"
          runTestTT (lazy_tests match)

          putStrLn "Us (nul):"
          runTestTT (nul_tests match)

          putStrLn "Us (shared):"
          runTestTT (shared_tests match)

          putStrLn "Us (all):"
          runTestTT (shared_tests_all match_all)

          putStrLn "Us (backrefs):"
          runTestTT (backref_tests match)

          putStrLn "Us (lazy patterns):"
          runTestTT (lazy_matching match)

          putStrLn "Us (possessive patterns):"
          runTestTT (pos_matching match)

          putStrLn "All done!"
          return ()

backref_tests :: (String -> String -> Res) -> Test
backref_tests impl = TestList
                   $ map (uncurry (mk_testlist impl)) backref_tests_data

backref_tests_data :: [(String, [(String, Res)])]
backref_tests_data =
    [("foo(.*)\\1baz", [
           ("foobaz", Just [""]),
           ("foobarwibblebaz", Nothing),
           ("foobarbarbaz", Just ["bar"]),
           ("foobbbaz", Just ["b"]),
           ("", Nothing)
          ]),
     ("(a*)((b*)(c*\\3))\\2", [
           ("aaabbccccbbbbccccbb", Just ["aaa", "bbccccbb", "bb", "ccccbb"]),
           ("aaabbcccbbbbcccbb", Just ["aaa", "bbcccbb", "bb", "cccbb"]),
           ("aaabbccccbbbbcccbb", Just ["aaa", "", "", ""]),
           ("aaabbcccbbbbccccbb", Just ["aaa", "", "", ""]),
           ("", Just ["", "", "", ""])
          ]),
     ("^(a*)((b*)(c*\\3))\\2$", [
           ("aaabbccccbbbbccccbb", Just ["aaa", "bbccccbb", "bb", "ccccbb"]),
           ("aaabbcccbbbbcccbb", Just ["aaa", "bbcccbb", "bb", "cccbb"]),
           ("aaabbccccbbbbcccbb", Nothing),
           ("aaabbcccbbbbccccbb", Nothing),
           ("", Just ["", "", "", ""])
          ])]

mk_htest :: (Show res, Eq res)
         => (String -> String -> res)
         -> String -> (String, res) -> Test
mk_htest impl re (inp, res) = TestLabel ("inp:" ++ inp)
                            $ TestCase $ assertEqual inp res (impl re inp)

mk_testlist :: (Show res, Eq res)
            => (String -> String -> res)
            -> String -> [(String, res)] -> Test
mk_testlist impl re irs = TestLabel ("re:" ++ re)
                        $ TestList
                        $ map (mk_htest impl re) irs

shared_tests_all :: (String -> String -> AllRes) -> Test
shared_tests_all impl = TestList
                      $ map (\x -> trace (show x) $ uncurry (mk_testlist impl) x) shared_tests_data_all

shared_tests_data_all :: [(String, [(String, AllRes)])]
shared_tests_data_all =
                [("b(c)d", [
                       ("abcde", Just ("a", "bcd", "e", ["c"])),
                       ("bcde", Just ("", "bcd", "e", ["c"])),
                       ("abcd", Just ("a", "bcd", "", ["c"])),
                       ("bcd", Just ("", "bcd", "", ["c"])),
                       ("", Nothing)
                      ])]

shared_tests :: (String -> String -> Res) -> Test
shared_tests impl = TestList
                  $ map (\(x ,y)-> uncurry (mk_testlist impl) (trace x x,y)) shared_tests_data

shared_tests_data :: [(String, [(String, Res)])]
shared_tests_data =
                [

                 ("()", [
                       ("", Just [""]),
                       ("a", Just [""]),
                       ("abc", Just [""])
                      ]),

                 ("^", [
                       ("", Just []),
                       ("a", Just []),
                       ("abc", Just [])
                      ]),
                 ("$", [
                       ("", Just []),
                       ("a", Just []),
                       ("abc", Just [])
                      ]),
                 ("^$", [
                       ("", Just []),
                       ("a", Nothing),
                       ("abc", Nothing)
                      ]),
                 ("a()*b", [
                       ("", Nothing),
                       ("a", Nothing),
                       ("ab", Just [""]),
                       ("abc", Just [""])
                      ]),
                 ("a", [
                       ("", Nothing),
                       ("a", Just []),
                       ("b", Nothing),
                       ("abc", Just []),
                       ("bac", Just []),
                       ("bca", Just [])
                      ]),
                 ("^a", [
                       ("", Nothing),
                       ("a", Just []),
                       ("b", Nothing),
                       ("abc", Just []),
                       ("bac", Nothing),
                       ("bca", Nothing)
                      ]),
                 ("a^b", [
                       ("", Nothing),
                       ("a", Nothing),
                       ("b", Nothing),
                       ("ab", Nothing),
                       ("a^b", Nothing),
                       ("abc", Nothing),
                       ("dab", Nothing),
                       ("dabc", Nothing)
                      ]),
                 ("a$", [
                       ("", Nothing),
                       ("a", Just []),
                       ("b", Nothing),
                       ("abc", Nothing),
                       ("bac", Nothing),
                       ("bca", Just [])
                      ]),
                 ("a$b", [
                       ("", Nothing),
                       ("a", Nothing),
                       ("b", Nothing),
                       ("ab", Nothing),
                       ("a$b", Nothing),
                       ("abc", Nothing),
                       ("dab", Nothing),
                       ("dabc", Nothing)
                      ]),
                 ("^[a]$", [
                       ("", Nothing),
                       ("a", Just []),
                       ("b", Nothing),
                       ("aa", Nothing)
                      ]),
                 ("^[ab]$", [
                       ("", Nothing),
                       ("a", Just []),
                       ("b", Just []),
                       ("c", Nothing),
                       ("ab", Nothing)
                      ]),
                 ("^[^a]$", [
                       ("", Nothing),
                       ("a", Nothing),
                       ("b", Just []),
                       ("aa", Nothing)
                      ]),
                 ("^[^ab]$", [
                       ("", Nothing),
                       ("a", Nothing),
                       ("b", Nothing),
                       ("c", Just []),
                       ("ab", Nothing)
                      ]),
                 ("^[[:alnum:]]$", [
                       ("", Nothing),
                       ("1", Just []),
                       ("a", Just []),
                       ("A", Just []),
                       (" ", Nothing)
                      ]),
                 ("^[[:alpha:]]$", [
                       ("", Nothing),
                       ("1", Nothing),
                       ("a", Just []),
                       ("A", Just [])
                      ]),
                 ("^[^34[:alpha:]5]$", [
                       ("", Nothing),
                       ("1", Just []),
                       ("3", Nothing),
                       ("4", Nothing),
                       ("5", Nothing),
                       ("6", Just []),
                       ("a", Nothing),
                       ("A", Nothing)
                      ]),
                 ("^[[:blank:]]$", [
                       ("", Nothing),
                       (" ", Just []),
                       ("\t", Just []),
                       ("a", Nothing)
                      ]),
                 ("^[[:cntrl:]]$", [
                       ("", Nothing),
                       ("\003", Just []),
                       ("a", Nothing)
                      ]),
                 ("^[[:digit:]]$", [
                       ("", Nothing),
                       ("1", Just []),
                       ("12", Nothing),
                       ("a", Nothing)
                      ]),
                 ("^[[:graph:]]$", [
                       ("", Nothing),
                       (" ", Nothing),
                       ("\t", Nothing),
                       (",", Just []),
                       ("1", Just []),
                       ("a", Just []),
                       ("\003", Nothing)
                      ]),
                 ("^[[:lower:]]$", [
                       ("", Nothing),
                       ("1", Nothing),
                       ("a", Just []),
                       ("A", Nothing)
                      ]),
                 ("^[[:print:]]$", [
                       ("", Nothing),
                       (" ", Just []),
                       ("\t", Nothing),
                       (",", Just []),
                       ("1", Just []),
                       ("a", Just []),
                       ("\003", Nothing)
                      ]),
                 ("^[[:punct:]]$", [
                       ("", Nothing),
                       (" ", Nothing),
                       ("\t", Nothing),
                       (",", Just []),
                       ("1", Nothing),
                       ("a", Nothing),
                       ("\003", Nothing)
                      ]),
                 ("^[[:space:]]$", [
                       ("", Nothing),
                       (" ", Just []),
                       ("\t", Just []),
                       ("a", Nothing)
                      ]),
                 ("^[[:upper:]]$", [
                       ("", Nothing),
                       ("1", Nothing),
                       ("a", Nothing),
                       ("A", Just [])
                      ]),
                 ("^[[:xdigit:]]$", [
                       ("", Nothing),
                       ("1", Just []),
                       ("12", Nothing),
                       ("a", Just []),
                       ("f", Just []),
                       ("g", Nothing),
                       ("z", Nothing),
                       ("A", Just []),
                       ("F", Just []),
                       ("G", Nothing),
                       ("Z", Nothing)
                      ]),
                 ("^\\\\$", [
                       ("", Nothing),
                       ("\\", Just []),
                       ("1", Nothing),
                       ("12", Nothing),
                       ("a", Nothing),
                       ("A", Nothing),
                       (".", Nothing),
                       ("\001", Nothing),
                       ("\000", Nothing)
                      ]),
                 ("^.$", [
                       ("", Nothing),
                       ("1", Just []),
                       ("12", Nothing),
                       ("a", Just []),
                       ("A", Just []),
                       (".", Just []),
                       ("\001", Just []),
                       ("\000", Just [])
                      ]),
                 ("^[.]$", [
                       ("", Nothing),
                       ("1", Nothing),
                       ("12", Nothing),
                       ("a", Nothing),
                       ("A", Nothing),
                       (".", Just []),
                       ("\001", Nothing),
                       ("\000", Nothing)
                      ]),
                 ("^\\.$", [
                       ("", Nothing),
                       ("1", Nothing),
                       ("12", Nothing),
                       ("a", Nothing),
                       ("A", Nothing),
                       (".", Just []),
                       ("\001", Nothing),
                       ("\000", Nothing)
                      ]),
                 ("^a$", [
                       ("", Nothing),
                       ("a", Just []),
                       ("b", Nothing),
                       ("abc", Nothing),
                       ("bac", Nothing),
                       ("bca", Nothing)
                      ]),
                 ("^a*$", [
                       ("", Just []),
                       ("a", Just []),
                       ("b", Nothing),
                       ("aa", Just []),
                       ("ab", Nothing)
                      ]),
                 ("(a*)*", [
                       ("", Just [""]),
                       ("a", Just [""]),
                       ("b", Just [""]),
                       ("aa", Just [""]),
                       ("aaa", Just [""]),
                       ("aaaa", Just [""]),
                       ("ab", Just [""])
                      ]),
                 ("^(a*)*$", [
                       ("", Just [""]),
                       ("a", Just [""]),
                       ("b", Nothing),
                       ("aa", Just [""]),
                       ("aaa", Just [""]),
                       ("aaaa", Just [""]),
                       ("ab", Nothing)
                      ]),
                 ("^(a*)?$", [
                       ("", Just [""]),
                       ("a", Just ["a"]),
                       ("b", Nothing),
                       ("aa", Just ["aa"]),
                       ("aaa", Just ["aaa"]),
                       ("aaaa", Just ["aaaa"]),
                       ("ab", Nothing)
                      ]),
                 ("^(a?)*$", [
                       ("", Just [""]),
                       ("a", Just [""]),
                       ("b", Nothing),
                       ("aa", Just [""]),
                       ("aaa", Just [""]),
                       ("aaaa", Just [""]),
                       ("ab", Nothing)
                      ]),
                 ("^(a)*$", [
                       ("", Just [""]),
                       ("a", Just ["a"]),
                       ("b", Nothing),
                       ("aa", Just ["a"]),
                       ("aaa", Just ["a"]),
                       ("aaaa", Just ["a"]),
                       ("ab", Nothing)
                      ]),
                 ("^(a*)$", [
                       ("", Just [""]),
                       ("a", Just ["a"]),
                       ("b", Nothing),
                       ("aa", Just ["aa"]),
                       ("aaa", Just ["aaa"]),
                       ("aaaa", Just ["aaaa"]),
                       ("ab", Nothing)
                      ]),
                 ("^(a|())*$", [
                       ("", Just ["",""]),
                       ("a", Just ["",""]),
                       ("b", Nothing),
                       ("aa", Just ["",""]),
                       ("aaa", Just ["",""]),
                       ("aaaa", Just ["",""]),
                       ("ab", Nothing)
                      ]),
                 ("^(.)*$", [
                       ("abc", Just ["c"])
                      ]),
                 ("^a?$", [
                       ("", Just []),
                       ("a", Just []),
                       ("b", Nothing),
                       ("aa", Nothing),
                       ("ab", Nothing)
                      ]),
                 ("^a|b$", [
                       ("", Nothing),
                       ("a", Just []),
                       ("b", Just []),
                       ("c", Nothing),
                       ("aa", Just []),
                       ("ab", Just []),
                       ("ac", Just []),
                       ("ba", Nothing),
                       ("bb", Just []),
                       ("bc", Nothing),
                       ("ca", Nothing),
                       ("cb", Just []),
                       ("cc", Nothing),
                       ("abc", Just []),
                       ("acb", Just []),
                       ("bac", Nothing),
                       ("bca", Nothing),
                       ("cab", Just []),
                       ("cba", Nothing)
                      ]),
                 ("^(a|b)$", [
                       ("", Nothing),
                       ("a", Just ["a"]),
                       ("b", Just ["b"]),
                       ("c", Nothing),
                       ("aa", Nothing),
                       ("ab", Nothing),
                       ("ac", Nothing),
                       ("ba", Nothing),
                       ("bb", Nothing),
                       ("bc", Nothing),
                       ("ca", Nothing),
                       ("cb", Nothing),
                       ("cc", Nothing),
                       ("abc", Nothing)
                      ]),
                 ("^(a)b$|^a(b)", [
                       ("", Nothing),
                       ("a", Nothing),
                       ("b", Nothing),
                       ("c", Nothing),
                       ("aa", Nothing),
                       ("ab", Just ["a", ""]),
                       ("abc", Just ["", "b"])
                      ]),
                 ("^(a)b|^a(b)$", [
                       ("", Nothing),
                       ("a", Nothing),
                       ("b", Nothing),
                       ("c", Nothing),
                       ("aa", Nothing),
                       ("ab", Just ["a", ""]),
                       ("abc", Just ["a", ""])
                      ]),
                 ("^((abc){3})$", [
                       ("", Nothing),
                       ("a", Nothing),
                       ("b", Nothing),
                       ("c", Nothing),
                       ("abc", Nothing),
                       ("abca", Nothing),
                       ("abcab", Nothing),
                       ("abcabc", Nothing),
                       ("abcabca", Nothing),
                       ("abcabcab", Nothing),
                       ("abcabcabc", Just ["abcabcabc", "abc"]),
                       ("abcabcabca", Nothing),
                       ("abcabcabcab", Nothing),
                       ("abcabcabcabc", Nothing),
                       ("abcabcabcabca", Nothing),
                       ("abcabcabcabcab", Nothing),
                       ("abcabcabcabcabc", Nothing),
                       ("abcabcabcabcabca", Nothing),
                       ("abcabcabcabcabcab", Nothing),
                       ("abcabcabcabcabcabc", Nothing)
                      ]),
                 ("^((abc){3,})$", [
                       ("", Nothing),
                       ("a", Nothing),
                       ("b", Nothing),
                       ("c", Nothing),
                       ("abc", Nothing),
                       ("abca", Nothing),
                       ("abcab", Nothing),
                       ("abcabc", Nothing),
                       ("abcabca", Nothing),
                       ("abcabcab", Nothing),
                       ("abcabcabc", Just ["abcabcabc", "abc"]),
                       ("abcabcabca", Nothing),
                       ("abcabcabcab", Nothing),
                       ("abcabcabcabc", Just ["abcabcabcabc", "abc"]),
                       ("abcabcabcabca", Nothing),
                       ("abcabcabcabcab", Nothing),
                       ("abcabcabcabcabc", Just ["abcabcabcabcabc", "abc"]),
                       ("abcabcabcabcabca", Nothing),
                       ("abcabcabcabcabcab", Nothing)
                      ]),
                 ("^((abc){3,5})$", [
                       ("", Nothing),
                       ("a", Nothing),
                       ("b", Nothing),
                       ("c", Nothing),
                       ("abc", Nothing),
                       ("abca", Nothing),
                       ("abcab", Nothing),
                       ("abcabc", Nothing),
                       ("abcabca", Nothing),
                       ("abcabcab", Nothing),
                       ("abcabcabc", Just ["abcabcabc", "abc"]),
                       ("abcabcabca", Nothing),
                       ("abcabcabcab", Nothing),
                       ("abcabcabcabc", Just ["abcabcabcabc", "abc"]),
                       ("abcabcabcabca", Nothing),
                       ("abcabcabcabcab", Nothing),
                       ("abcabcabcabcabc", Just ["abcabcabcabcabc", "abc"]),
                       ("abcabcabcabcabca", Nothing),
                       ("abcabcabcabcabcab", Nothing),
                       ("abcabcabcabcabcabc", Nothing),
                       ("abcabcabcabcabcabca", Nothing),
                       ("abcabcabcabcabcabcab", Nothing),
                       ("abcabcabcabcabcabcabc", Nothing)
                      ])
                ]

lazy_tests :: (String -> String -> Res) -> Test
lazy_tests impl = TestList
                $ map (uncurry (mk_testlist impl)) lazy_tests_data

lazy_tests_data :: [(String, [(String, Res)])]
lazy_tests_data =
                [
                 ("()", [
                       ('X':error "Unneeded", Just [""])
                      ]),
                 ("^", [
                       ('X':error "Unneeded", Just [])
                      ]),
                 ("a", [
                       ("a" ++ error "Unneeded", Just []),
                       ("bca" ++ error "Unneeded", Just [])
                      ]),

                 ("^a", [
                       ("a" ++ error "Unneeded", Just [])
-- multiline
--                      ,("bX" ++ error "Unneeded", Nothing)
                      ]),

                 ("^a|b$", [
                       ("a" ++ error "Unneeded", Just [])
                      ])
                ]

nul_tests :: (String -> String -> Res) -> Test
nul_tests impl = TestList
               $ map (uncurry (mk_testlist impl)) nul_tests_data

nul_tests_data :: [(String, [(String, Res)])]
nul_tests_data =
                [
                 ("^(.|\000)$", [
                       ("", Nothing),
                       ("1", Just ["1"]),
                       ("12", Nothing),
                       ("a", Just ["a"]),
                       ("A", Just ["A"]),
                       (".", Just ["."]),
                       ("\001", Just ["\001"]),
                       ("\000", Just ["\000"])
                      ])
                ]

lazy_matching impl = TestList
                     $ map (uncurry (mk_testlist impl)) lazy_matching_data
lazy_matching_data =
    [("(a??)"
     ,[("",Just [""])
      ,("a",Just [""])
      ,("b",Just [""])])
    ,("(a??a)"
     ,[("a",Just["a"])
      ,("b",Nothing)
      ,("ab",Just["a"])
      ,("ba",Just["a"])
      ,("aa",Just ["a"])])
    ,("(a??b)"
     ,[("a",Nothing)
      ,("b",Just ["b"])
      ,("ab",Just ["ab"])
      ,("ba",Just ["b"])])
    ,("b(.*?)ac"
     ,[("bc",Nothing)
      ,("bac",Just [""])
      ,("baaaacaaac",Just ["aaa"])])
    ,("b()*?ac"
     ,[("bc",Nothing)
      ,("bac",Just [""])
      ,("baaaacaaac",Nothing)])
    ,("b(.+?)ac"
     ,[("bac",Nothing)
      ,("bacac",Just["ac"])
      ,("baac",Just ["a"])
      ,("baacac",Just ["a"])
      ,("baaaacaaac",Just ["aaa"])])
    ,("b(.{3,}?)ac"
    ,[("bacac",Nothing)
     ,("bacacacac",Just ["acac"])
     ,("baacacac",Just ["aac"])])
    ,("b(.{3,5}?)ac"
    ,[("bacac",Nothing)
     ,("bacacacac",Just ["acac"])
     ,("baacacac",Just ["aac"])
     ,("baaaaaacacac",Just ["aaaaa"])
     ,("baaaaaaacacac",Nothing)])
    ]

pos_matching impl = TestList
                     $ map (uncurry (mk_testlist impl)) pos_matching_data
pos_matching_data =
    [("(a?+)"
     ,[("",Just [""])
      ,("a",Just ["a"])
      ,("b",Just [""])])
    ,("(a?+a)"
     ,[("a",Nothing)
      ,("b",Nothing)
      ,("ab",Nothing)
      ,("ba",Nothing)
      ,("aa",Just ["aa"])])
    ,("(a?+b)"
     ,[("a",Nothing)
      ,("b",Just ["b"])
      ,("ab",Just ["ab"])
      ,("ba",Just ["b"])])
    ,("b(.*+)ac"
     ,[("bc",Nothing)
      ,("bac",Nothing)
      ,("baaaacaaac",Nothing)])
    ,("b()*+ac"
     ,[("bc",Nothing)
      ,("bac",Just [""])
      ,("baaaacaaac",Nothing)])
    ,("b(a*+)c"
     ,[("bc",Just [""])
      ,("bac",Just ["a"])
      ,("baaaacaaac",Just["aaaa"])])
    ,("b(a++)c"
     ,[("bc",Nothing)
      ,("bac",Just["a"])
      ,("bacac",Just["a"])
      ,("baac",Just ["aa"])
      ,("baacac",Just ["aa"])
      ,("baaaacaaac",Just ["aaaa"])])
    ]

