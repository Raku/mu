{-# OPTIONS_GHC -fglasgow-exts #-}
module Pugs.Parser.Program (
    -- Before you would import Pugs.Parser, now you import Pugs.Parser.Program.
    parseProgram,
) where
import Pugs.Internals
import Pugs.AST
import Pugs.Types (isSigilChar)

import Pugs.Parser
import Pugs.Rule
import Text.ParserCombinators.Parsec.Error (showErrorMessages, errorMessages)
import qualified Data.Map as Map

parseProgram :: Env -> FilePath -> String -> Env
parseProgram env path str = runRule env ruleProgram path progWithEOL
    where
    prog = decodeProgram str
    progWithEOL
        | [] <- prog        = "\n"
        | last prog == '\n' = prog
        | otherwise         = prog ++ "\n"

-- Based on: <http://hackage.haskell.org/trac/haskell-prime/wiki/SourceEncodingDetection>
data EncodedSource
    = UTF8 !String
    | UTF16 !Endian !String
    | UTF32 !Endian !String
 -- ... | UserDefined ...

data Endian = LittleEndian | BigEndian

decodeProgram :: String -> String
decodeProgram str = case detectSourceEncoding str of
    UTF8 xs                 -> decodeUTF8 (removeCRLF xs)
    UTF16 LittleEndian xs   -> removeCRLF (decodeUTF16LE xs)
    UTF16 BigEndian xs      -> removeCRLF (decodeUTF16BE xs)
    UTF32 LittleEndian xs   -> removeCRLF (decodeUTF32LE xs)
    UTF32 BigEndian xs      -> removeCRLF (decodeUTF32BE xs)
    where
    removeCRLF ('\r':'\n':xs)   = '\n':removeCRLF xs
    removeCRLF (x:xs)           = x:removeCRLF xs
    removeCRLF []               = []
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
    { s_env             = env
    , s_parseProgram    = parseProgram
    , s_dynParsers      = MkDynParsersEmpty
    , s_bracketLevel    = StatementBracket
--  , s_char            = ' '
--  , s_name            = nullID
--  , s_pos             = 0
    , s_wsLine          = 0
    , s_wsColumn        = 0
    , s_closureTraits   = [id]
--  , s_freeVars        = Set.empty
    , s_knownVars       = Map.map (const topMPad) (padEntries (envLexical env))
    , s_outerVars       = Map.empty
    , s_protoPad        = emptyPad
    }

-- ^ A fake 'top' MPad for s_knownVars above to refer to things outside the eval scope.
{-# NOINLINE topMPad #-}
topMPad :: MPad
topMPad = unsafePerformIO $ do
    tvar <- newTVarIO emptyPad
    return $ MkMPad (addressOf tvar) tvar

-- XXX - Pending clarification about those 3 -- are they routine-implicit or block-implicit?
{-
protoPad :: Pad
protoPad = mkPad
    [ (cast "$_", PELexical
    , (cast "$/",
    , (cast "$!", 
    ]
-}

runRule :: Env -> RuleParser Env -> FilePath -> String -> Env
runRule env p name str =
    case ( runParser p (makeState env) name str ) of
        Left err    -> env { envBody = Val $ VError (VStr msg) [mkPos pos pos] }
            where
            msg = concat (intersperse "\n" (map filterUnexpected $ lines (showErr err)))
            pos = errorPos err
            cur = case takeSameClassWords (dropUntilPos pos str) of
                ""  -> "end of input"
                xs  -> show xs
            filterUnexpected ('!':_)    = "Unexpected " ++ cur
            filterUnexpected line       = line
        Right env'  -> env'

takeSameClassWords :: String -> String
takeSameClassWords "" = ""
takeSameClassWords (x:xs)
    | isSigilChar x = x : takeSameClassWords xs
    | otherwise = case charClassOf x of
        SpaceClass  -> x : takeSameClassWords xs
        cls         -> x : takeWhile ((== cls) . charClassOf) xs

dropUntilPos :: SourcePos -> String -> String
dropUntilPos pos str
    | (curline:_) <- drop (ln - 1) (lines str) = drop (col - 1) curline
    | otherwise = ""
    where
    col = sourceColumn pos
    ln  = sourceLine pos

showErr :: ParseError -> String
showErr err =
      showErrorMessages "or" "unknown parse error"
                        "expecting" "!" "end of input"
                       (errorMessages err)

-- Lexical units --------------------------------------------------

ruleProgram :: RuleParser Env
ruleProgram = rule "program" $ do
    env     <- getRuleEnv

    topPad  <- genParamEntries SubRoutine [defaultArrayParam]
    modify $ \s -> s{ s_protoPad = topPad }

    block   <- ruleBlockBody `finallyM` eof
    main    <- retVerbatimBlock SubPrim Nothing False $
        block{ bi_body = mergeStmts emptyExp $ bi_body block }

    -- We are still in the compile time.
    modify $ \s -> s{ s_env = (s_env s){ envCompPad = Just (error "no comp pad") } }

    -- Force a reclose-pad evaluation here by way of unsafeEvalExp.
    main'@(Val (VCode vc)) <- unsafeEvalExp $ Syn "" [unwrap main]

    -- S04: CHECK {...}*      at compile time, ALAP
    --  $_() for @*CHECK
    rv <- unsafeEvalExp $ Syn "for"
        [ _Var "@*CHECK"
        , Syn "sub"
            [ Val . VCode $ mkPrim
                { subBody       = App (_Var "$_") Nothing []
                , subParams     = [defaultScalarParam]
                , subInnerPad   = defaultScalarPad
                }
            ]
        ]

    -- If there was a exit() in a CHECK block, we have to exit.
    possiblyExit rv

    env' <- getRuleEnv
    return $ env'
        { envBody       = App (Syn "block" [main']) Nothing (replicate (length $ subParams vc) (_Var "$_")) -- _Var "@*ARGS"]
        , envPackage    = envPackage env
        , envCompPad    = Nothing
        }

