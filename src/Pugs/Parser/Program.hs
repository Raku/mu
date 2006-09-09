{-# OPTIONS_GHC -fglasgow-exts #-}
module Pugs.Parser.Program (
    -- Before you would import Pugs.Parser, now you import Pugs.Parser.Program.
    parseProgram,
) where
import Pugs.Internals
import Pugs.AST

import Pugs.Parser
import Pugs.Rule
import Text.ParserCombinators.Parsec.Error (showErrorMessages, errorMessages)
import qualified Data.Map as Map
import qualified Data.Set as Set

parseProgram :: Env -> FilePath -> String -> Env
parseProgram env path str = runRule env ruleProgram path (decodeProgram str)

-- Based on: http://hackage.haskell.org/trac/haskell-prime/wiki/SourceEncodingDetection
data EncodedSource
    = UTF8 !String
    | UTF16 !Endian !String
    | UTF32 !Endian !String
 -- | UserDefined ...

data Endian = LittleEndian | BigEndian

decodeProgram :: String -> String
decodeProgram str = case detectSourceEncoding str of
    UTF8 xs                 -> decodeUTF8 xs
    UTF16 LittleEndian xs   -> decodeUTF16LE xs
    UTF16 BigEndian xs      -> decodeUTF16BE xs
    UTF32 LittleEndian xs   -> decodeUTF32LE xs
    UTF32 BigEndian xs      -> decodeUTF32BE xs
    where
    decodeUTF16BE (a:b:c:d:xs)
        | a >= '\xD8', a <= '\xDB'  -- High surrogate
        , c >= '\xDC', c <= '\xDF'  -- Low surrogate
        = let rest = decodeUTF16BE xs
              hi   = (ord a - 0xD8) * 0x100 + ord b
              lo   = (ord c - 0xDC) * 0x100 + ord d
        in seq rest (chr (0x10000 + hi * 0x400 + lo) : rest)
    decodeUTF16BE (a:b:xs) = let rest = decodeUTF16BE xs
        in seq rest (chr (ord a * 0x100 + ord b) : rest)
    decodeUTF16BE _ = []
    decodeUTF16LE (a:b:c:d:xs)
        | b >= '\xD8', b <= '\xDB'  -- High surrogate
        , d >= '\xDC', d <= '\xDF'  -- Low surrogate
        = let rest = decodeUTF16LE xs
              hi   = (ord b - 0xD8) * 0x100 + ord a
              lo   = (ord d - 0xDC) * 0x100 + ord c
        in seq rest (chr (0x10000 + hi * 0x400 + lo) : rest)
    decodeUTF16LE (a:b:xs) = let rest = decodeUTF16LE xs
        in seq rest (chr (ord b * 0x100 + ord a) : rest)
    decodeUTF16LE _ = []
    decodeUTF32BE (a:b:c:d:xs) = let rest = decodeUTF32BE xs
        in seq rest (chr (ord a * 0x1000000 + ord b * 0x10000 + ord c * 0x100 + ord d) : rest)
    decodeUTF32BE _ = []
    decodeUTF32LE (a:b:c:d:xs) = let rest = decodeUTF32LE xs
        in seq rest (chr (ord d * 0x1000000 + ord c * 0x10000 + ord b * 0x100 + ord a) : rest)
    decodeUTF32LE _ = []

detectSourceEncoding :: String -> EncodedSource
detectSourceEncoding bytes = case bytes of
    []                                  -> UTF8 []
    ['\x00']                            -> invalidNulls
    xs@[_]                              -> UTF8 xs
    ['\xFF', '\xFE']                    -> UTF16 LittleEndian []
    ('\xFE':'\xFF':xs)                  -> UTF16 BigEndian xs
    ['\x00', '\x00']                    -> invalidNulls
    xs@['\x00', _]                      -> UTF16 BigEndian xs
    xs@[_, '\x00']                      -> UTF16 LittleEndian xs
    xs@[_, _]                           -> UTF8 xs
    ['\x00', '\x00', '\x00']            -> invalidNulls
    xs@[_, _, _]                        -> UTF8 xs
    ('\xEF':'\xBB':'\xBF':xs)           -> UTF8 xs
    ('\x00':'\x00':'\xFE':'\xFF':xs)    -> UTF32 BigEndian xs
    ('\xFF':'\xFE':'\x00':'\x00':xs)    -> UTF32 LittleEndian xs
    ('\xFF':'\xFE':xs)                  -> UTF16 LittleEndian xs
    ('\x00':'\x00':'\x00':'\x00':_)     -> invalidNulls
    xs@('\x00':'\x00':'\x00':_)         -> UTF32 BigEndian xs
    xs@(_:'\x00':'\x00':'\x00':_)       -> UTF32 LittleEndian xs
    ('\x00':'\x00':_)                   -> invalidNulls
    xs@('\x00':_)                       -> UTF16 BigEndian xs
    xs@(_:'\x00':_)                     -> UTF16 LittleEndian xs
    xs                                  -> UTF8 xs
    where
    invalidNulls = error "(invalid nulls)"

makeState :: Env -> RuleState
makeState env = MkState
    { s_env           = env
    , s_parseProgram  = parseProgram
    , s_dynParsers    = MkDynParsersEmpty
    , s_bracketLevel  = StatementBracket
    , s_char          = ' '
    , s_name          = nullID
    , s_pos           = 0
    , s_blockPads     = Map.empty
    , s_closureTraits = []
    , s_outerVars     = Set.empty
    }

runRule :: Env -> RuleParser Env -> FilePath -> String -> Env
runRule env p name str =
    case ( runParser p (makeState env) name str ) of
        Left err    -> env { envBody = Val $ VError (VStr msg) [mkPos pos pos] }
            where
            pos = errorPos err
            msg = showErr err
        Right env'  -> env'

showErr :: ParseError -> String
showErr err =
      showErrorMessages "or" "unknown parse error"
                        "expecting" "unexpected" "end of input"
                       (errorMessages err)

-- Lexical units --------------------------------------------------

ruleProgram :: RuleParser Env
ruleProgram = rule "program" $ do
    env <- getRuleEnv
    statements <- ruleBlockBody
    -- error $ show statements
    eof
    -- S04: CHECK {...}*      at compile time, ALAP
    --  $_() for @*CHECK
    rv <- unsafeEvalExp $ Syn "for"
        [ _Var "@*CHECK"
        , Syn "sub"
            [ Val . VCode $ mkSub
                { subBody   = App (_Var "$_") Nothing []
                , subParams = [defaultScalarParam]
                }
            ]
        ]
    -- If there was a exit() in a CHECK block, we have to exit.
    possiblyExit rv
    env' <- getRuleEnv
    return $ env'
        { envBody       = mergeStmts emptyExp statements
        , envPackage    = envPackage env
        }

