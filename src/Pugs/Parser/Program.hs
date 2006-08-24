-- Before you would import Pugs.Parser, now you import Pugs.Parser.Program.
module Pugs.Parser.Program (
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
    UTF8 x  -> decodeUTF8 x 
    UTF16{} -> error "UTF16 source not yet handled"
    UTF32{} -> error "UTF32 source not yet handled"

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
    ('\xFF':'\xFE':xs)                  -> UTF16 BigEndian xs
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

