{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans -fno-full-laziness -fno-cse #-}
{-# OPTIONS_GHC -#include "../UnicodeC.h" #-}

{-|
    Primitive operators.

>   There hammer on the anvil smote,
>   There chisel clove, and graver wrote;
>   There forged was blade, and bound was hilt;
>   The delver mined, the mason built...
-}

module Pugs.Prim (
    primOp,
    primDecl,
    initSyms,
    op2DefinedOr,
    op2ChainedList,
    op1Exit,
    op1Return,
    -- used by Pugs.Compile.Haskell
    op0, op1, op2,
    -- used Pugs.Eval
    foldParam, op2Hyper, op1HyperPrefix, op1HyperPostfix,
) where
import Pugs.Internals
import Pugs.Junc
import Pugs.AST
import Pugs.Types
import Pugs.Monads
import Pugs.Pretty
import Text.Printf
import Pugs.External
import Pugs.Embed
import Pugs.Eval.Var
import qualified Data.Map as Map
import Data.IORef
import System.IO.Error (isEOFError)

import Pugs.Prim.Keyed
import Pugs.Prim.Yaml
import Pugs.Prim.Match
import qualified Pugs.Prim.FileTest as FileTest
import Pugs.Prim.List
import Pugs.Prim.Numeric
import Pugs.Prim.Lifts
import Pugs.Prim.Eval
import Pugs.Prim.Code
import Pugs.Prim.Param

-- |Implementation of 0-ary and variadic primitive operators and functions
-- (including list ops).
op0 :: String -> [Val] -> Eval Val
op0 "&"  = fmap opJuncAll  . mapM fromVal
op0 "^"  = fmap opJuncOne  . mapM fromVal
op0 "|"  = fmap opJuncAny  . mapM fromVal
op0 "want"  = const $ fmap VStr (asks (maybe "Void" envWant . envCaller))
op0 "bool::true" = const $ return (VBool True)
op0 "bool::false" = const $ return (VBool False)
op0 "time"  = const $ do
    clkt <- guardIO getClockTime
    return $ VRat $ fdiff $ diffClockTimes clkt epochClkT
    where
       epochClkT = toClockTime epoch
       epoch = CalendarTime 2000 January 1 0 0 0 0 Saturday 0 "UTC" 0 False
       -- 10^12 is expanded because the alternatives tried gave type warnings.
       fdiff = \d -> (fromInteger $ tdPicosec d)
                   / (clocksPerSecond * clocksPerSecond)
                   + (fromIntegral $ tdSec d)
op0 "times"  = const $ do
    ProcessTimes _ u s cu cs <- guardIO getProcessTimes
    return . VList $ map (castV . (% (clocksPerSecond :: VInt)) . toInteger . fromEnum)
        [u, s, cu, cs]
op0 "¥" = op0Zip
op0 "Y" = op0 "¥"
op0 "File::Spec::cwd" = const $ do
    cwd <- guardIO getCurrentDirectory
    return $ VStr cwd
op0 "File::Spec::tmpdir" = const $ do
    tmp <- guardIO getTemporaryDirectory
    return $ VStr tmp
op0 "pi" = const $ return (VNum pi)
op0 "self" = const $ evalExp $ Var "$?SELF"
op0 "say" = const $ op1 "IO::say" (VHandle stdout)
op0 "print" = const $ op1 "IO::print" (VHandle stdout)
op0 "return" = const $ op1Return (shiftT . const $ retEmpty)
op0 "yield" = const $ op1Yield (shiftT . const $ retEmpty)
op0 "take" = const $ retEmpty
op0 "nothing" = const $ return $ VBool True
op0 other = const $ fail ("Unimplemented listOp: " ++ other)

-- |Implementation of unary primitive operators and functions
op1 :: String -> Val -> Eval Val
op1 "!"    = op1Cast (VBool . not)
op1 "id" = \x -> do
    val <- fromVal x
    case val of
        VObject o   -> return . castV . hashUnique $ objId o
        _           -> return undef
op1 "clone" = \x -> do
    (VObject o) <- fromVal x
    attrs   <- readIVar (IHash $ objAttrs o)
    attrs'  <- liftSTM $ newTVar Map.empty
    uniq    <- liftIO $ newUnique
    writeIVar (IHash attrs') attrs
    return $ VObject o{ objAttrs = attrs', objId = uniq }
op1 "chop" = \x -> do
    str <- fromVal x
    if null str
        then return $ VStr str
        else return $ VStr $ init str
op1 "chomp" = \x -> do
    str <- fromVal x
    if null str || last str /= '\n'
        then return $ VStr str
        else do
            -- writeRef ref $ VStr (init str)
            return $ VStr $ init str
op1 "Str::split" = op1Cast (castV . words)
op1 "lc" = op1Cast (VStr . map toLower)
op1 "lcfirst" = op1StrFirst toLower
op1 "uc" = op1Cast (VStr . map toUpper)
op1 "ucfirst" = op1StrFirst toUpper
op1 "capitalize" = op1Cast $ VStr . (mapEachWord capitalizeWord)
  where
    mapEachWord _ [] = []
    mapEachWord f str@(c:cs)
        | isSpace c = c:(mapEachWord f cs)
        | otherwise = f word ++ mapEachWord f rest
          where (word,rest) = break isSpace str
    capitalizeWord []     = []
    capitalizeWord (c:cs) = toUpper c:(map toLower cs)
op1 "undef" = const $ return undef
op1 "undefine" = \x -> do
    when (defined x) $ do
        ref <- fromVal x
        clearRef ref
    return undef
op1 "+"    = op1Numeric id
op1 "abs"  = op1Numeric abs
op1 "Pugs::Internals::truncate" = op1Round truncate
op1 "Pugs::Internals::round"    = op1Round round
op1 "Pugs::Internals::floor"    = op1Round floor
op1 "Pugs::Internals::ceiling"  = op1Round ceiling
op1 "cos"  = op1Floating cos
op1 "sin"  = op1Floating sin
op1 "tan"  = op1Floating tan
op1 "sqrt" = op1Floating sqrt
op1 "atan" = op1Floating atan
op1 "post:++" = \x -> do
    val <- fromVal x
    ref <- fromVal x
    val' <- case val of
        (VStr str)  -> return (VStr $ strInc str)
        _           -> op1Numeric (+1) val
    writeRef ref val'
    case val of
        (VStr _)    -> return val
        _           -> op1 "+" val
op1 "++"   = \mv -> do
    op1 "post:++" mv
    fromVal mv
op1 "post:--"   = \x -> do
    val <- fromVal x
    ref <- fromVal x
    writeRef ref =<< op1Numeric (\x -> x - 1) val
    return val
op1 "--"   = \mv -> do
    op1 "post:--" mv
    fromVal mv
op1 "-"    = op1Numeric negate
op1 "item" = return -- XXX refify?
op1 "sort" = \v -> do
    args    <- fromVal v
    (valList, sortBy) <- case args of
        (v:vs) -> do
            ifValTypeIsa v "Code"
                (return (vs, Just v))
                (ifValTypeIsa (last args) "Code"
                    (return (init args, Just $ last args))
                    (return (args, Nothing)))
        _  -> return (args, Nothing)
    case sortBy of
        Nothing -> do
            strs    <- mapM fromVal valList
            returnList . map snd . sort $ (strs :: [VStr]) `zip` valList
        Just subVal -> do
            sub <- fromVal subVal
            sorted <- (`sortByM` valList) $ \v1 v2 -> do
                rv  <- enterEvalContext (cxtItem "Int") $ App (Val sub) Nothing [Val v1, Val v2]
                int <- fromVal rv
                return (int <= (0 :: Int))
            returnList sorted
op1 "reverse" = \v -> do
    case v of
        (VRef _) -> do
            ifValTypeIsa v "Scalar"
                (do ref     <- fromVal v
                    val     <- readRef ref
                    str     <- fromVal val
                    return (VStr $ reverse str))
                (do ref     <- fromVal v
                    vals    <- readRef ref
                    vlist   <- fromVal vals
                    return (VList $ reverse vlist))
        _ -> ifListContext
            (op1Cast (VList . reverse) v)
            (op1Cast (VStr . reverse) v)
op1 "list" = op1Cast VList
op1 "pair" = op1Cast $ VList . (map $ \(k, v) -> castV ((VStr k, v) :: VPair))
op1 "~"    = op1Cast VStr
op1 "?"    = op1Cast VBool
op1 "int"  = op1Cast VInt
op1 "+^"   = op1Cast (VInt . (toInteger . (complement :: Word -> Word)))
op1 "~^"   = op1Cast (VStr . mapStr complement)
op1 "?^"   = op1 "!"
op1 "\\"   = \v -> do
    return $ case v of
        (VRef (MkRef (IScalar _))) -> VRef . scalarRef $ v
        (VRef _)    -> v
        (VList vs)  -> VRef . arrayRef $ vs
        _           -> VRef . scalarRef $ v
op1 "post:..."  = op1Range
op1 "true" = op1 "?"
op1 "any"  = op1Cast opJuncAny
op1 "all"  = op1Cast opJuncAll
op1 "one"  = op1Cast opJuncOne
op1 "none" = op1Cast opJuncNone
op1 "perl" = fmap VStr . prettyVal 0
op1 "require_haskell" = \v -> do
    name    <- fromVal v
    externRequire "Haskell" name
    return $ VBool True
op1 "require_parrot" = \v -> do
    name    <- fromVal v
    liftIO $ evalParrotFile name
    return $ VBool True
op1 "require_perl5" = \v -> do
    name    <- fromVal v
    val     <- op1 "Pugs::Internals::eval_perl5" (VStr $ "require " ++ name ++ "; '" ++ name ++ "'")
    evalExp $ Sym SGlobal ('$':'*':name) (Syn ":=" [Var ('$':'*':name), Val val])
    case val of
        PerlSV _  -> return val
        _         -> fail "perl5 is not available"
op1 "Pugs::Internals::eval_parrot" = \v -> do
    code    <- fromVal v
    liftIO . evalParrot $ case code of
        ('.':_) -> code
        _       -> unlines
            [ ".sub pugs_eval_parrot"
            -- , "trace 1"
            , code
            , ".end"
            ]
    return $ VBool True
op1 "use" = opRequire True
op1 "require" = opRequire False
op1 "Pugs::Internals::eval" = \v -> do
    str <- fromVal v
    opEval quiet "<eval>" str
    where quiet = MkEvalStyle{evalResult=EvalResultLastValue
                             ,evalError=EvalErrorUndef}
op1 "evalfile" = \v -> do
    filename <- fromVal v
    opEvalFile filename
op1 "Pugs::Internals::eval_perl5" = \v -> do
    str <- fromVal v
    env <- ask
    tryIO undef $ do
        envSV <- mkVal (VControl $ ControlEnv env)
        sv <- evalPerl5 str envSV $ enumCxt (envContext env)
        return $ PerlSV sv
op1 "Pugs::Internals::eval_haskell" = op1EvalHaskell
op1 "Pugs::Internals::eval_yaml" = evalYaml
op1 "try" = \v -> do
    sub <- fromVal v
    val <- resetT $ evalExp (App (Val $ VCode sub) Nothing [])
    retEvalResult quiet val
    where quiet = MkEvalStyle{evalResult=EvalResultLastValue
                             ,evalError=EvalErrorUndef}
-- Tentative implementation of nothingsmuch's lazy proposal.
op1 "lazy" = \v -> do
    sub     <- fromVal v
    result  <- liftSTM $ newTVar Nothing
    let exp = App (Val $ VCode sub) Nothing []
        thunk = do
            cur <- liftSTM $ readTVar result
            maybe eval return cur
        eval = do
            res <- evalExp exp
            liftSTM $ writeTVar result (Just res)
            return res
    typ <- inferExpType exp
    return . VRef . thunkRef $ MkThunk thunk typ

op1 "defined" = op1Cast (VBool . defined)
op1 "last" = const $ fail "cannot last() outside a loop"
op1 "next" = const $ fail "cannot next() outside a loop"
op1 "redo" = const $ fail "cannot redo() outside a loop"
op1 "return" = op1Return . op1ShiftOut
op1 "yield" = op1Yield . op1ShiftOut
op1 "take" = \v -> do
    lex <- asks envLexical
    arr <- findSymRef "@?TAKE" lex
    op2 "push" (VRef arr) v
op1 "sign" = \v -> if defined v
    then op1Cast (VInt . signum) v
    else return undef

op1 "rand"  = \v -> do
    x    <- fromVal v
    rand <- guardIO $ randomRIO (0, if x == 0 then 1 else x)
    return $ VNum rand
op1 "say" = op2 "IO::say" (VHandle stdout)
op1 "print" = op2 "IO::print" (VHandle stdout)
op1 "IO::say" = \v -> op2 "IO::say" v =<< readVar "$_"
op1 "IO::print" = \v -> op2 "IO::print" v =<< readVar "$_"
op1 "IO::next" = \v -> do
    fh  <- fromVal v
    guardIO $ fmap (VStr . (++ "\n") . decodeUTF8) (hGetLine fh)
op1 "Pugs::Safe::safe_print" = \v -> do
    str  <- fromVal v
    guardIO $ hPutStr stdout $ encodeUTF8 str
    return $ VBool True
op1 "die" = \v -> do
    strs <- fromVal v
    fail (errmsg . concat $ strs)
    -- To avoid the uncatchable error "Prelude.last: empty list" and to present
    -- a nicer error message to the user.
    where
    errmsg "" = "Died"
    errmsg x  = x
op1 "warn" = \v -> do
    strs <- fromVal v
    errh <- readVar "$*ERR"
    pos  <- asks envPos
    op2 "IO::say" errh $ VList [ VStr $ pretty (VError (errmsg strs) [pos]) ]
    where
    errmsg "" = "Warning: something's wrong"
    errmsg x  = x
op1 "fail" = op1 "fail_" -- XXX - to be replaced by Prelude later
op1 "fail_" = \v -> do
    strs  <- fromVal v
    throw <- fromVal =<< readVar "$?FAIL_SHOULD_DIE"
    pos   <- asks envPos
    let msg = pretty (VError (errmsg strs) [pos]) ++ "\n"
    if throw
        -- "use fatal" is in effect, so die.
        then fail msg
        -- We've to return a unthrown exception.
        -- The error message to output
        else shiftT . const . return . VRef . thunkRef $ MkThunk (fail msg) anyType
    where
    errmsg "" = "Failed"
    errmsg x  = x
op1 "exit" = op1Exit
op1 "readlink" = \v -> do
    str  <- fromVal v
    guardIO $ fmap VStr (readSymbolicLink str)
op1 "sleep" = \v -> do
    x <- fromVal v
    guardIO $ do
        TOD t0s t0ps <- getClockTime
        threadDelay (x * clocksPerSecond)
        TOD t1s t1ps <- getClockTime
        return $ VRat ((fromInteger $ t1ps - t0ps)
                      / (clocksPerSecond * clocksPerSecond) -- 10^12
                      + (fromInteger $ t1s - t0s))
op1 "mkdir" = guardedIO createDirectory
op1 "rmdir" = guardedIO removeDirectory
op1 "chdir" = guardedIO setCurrentDirectory
op1 "-r"    = FileTest.isReadable
op1 "-w"    = FileTest.isWritable
op1 "-x"    = FileTest.isExecutable
op1 "-e"    = FileTest.exists
op1 "-z"    = FileTest.sizeIsZero
op1 "-s"    = FileTest.fileSize
op1 "-f"    = FileTest.isFile
op1 "-d"    = FileTest.isDirectory
op1 "List::end"   = op1Cast (VInt . (-1 +) . (genericLength :: VList -> VInt))
op1 "List::elems" = op1Cast (VInt . (genericLength :: VList -> VInt))
op1 "graphs"= op1Cast (VInt . (genericLength :: String -> VInt)) -- XXX Wrong
op1 "codes" = op1Cast (VInt . (genericLength :: String -> VInt))
op1 "chars" = op1Cast (VInt . (genericLength :: String -> VInt))
op1 "bytes" = op1Cast (VInt . (genericLength :: String -> VInt) . encodeUTF8)

op1 "unlink" = \v -> do
    vals <- fromVals v
    rets <- mapM (doBoolIO removeFile) vals
    return $ VInt $ sum $ map bool2n rets
op1 "readdir" = \v -> do
    path  <- fromVal v
    files <- guardIO $ getDirectoryContents path
    return . VList $ map VStr files
op1 "slurp" = \v -> do
    ifValTypeIsa v "IO"
        (do h <- fromVal v
            ifListContext
                (op1 "=" v)
                (fmap VStr (guardIO $ hGetContents h)))
        (do
            fileName    <- fromVal v
            ifListContext
                (slurpList fileName)
                (slurpScalar fileName))
    where
    slurpList file = op1 "=" (VList [VStr file])
    slurpScalar file = do
        content <- guardIO $ readFile file
        return $ VStr content
op1 "opendir" = \v -> do
    str <- fromVal v
    dir <- guardIO $ openDirStream str
    obj <- createObject (mkType "IO::Dir") []
    return . VObject $ obj{ objOpaque = Just $ toDyn dir }
op1 "IO::Dir::closedir" = guardedIO (closeDirStream . fromObject)
op1 "IO::Dir::rewinddir" = guardedIO (rewindDirStream . fromObject)
op1 "IO::Dir::readdir" = \v -> do
    dir <- fmap fromObject (fromVal v)
    ifListContext
        (fmap castV $ readDirStreamList dir)
        (guardIO $ fmap (\x -> if null x then undef else castV x) $ readDirStream dir)
    where
    readDirStreamList dir = do
        this <- tryIO "" $ readDirStream dir
        if null this then return [] else do
        rest <- readDirStreamList dir
        return $ (this:rest)
op1 "Pugs::Internals::runInteractiveCommand" = \v -> do
    str <- fromVal v
    guardIO $ do
        (inp,out,err,phand) <- runInteractiveCommand str
        return $ VList [ VHandle inp
                       , VHandle out
                       , VHandle err
                       , VProcess (MkProcess phand)
                       ]
op1 "Pugs::Internals::check_for_io_leak" = \v -> do
    rv      <- evalExp (App (Val v) Nothing [])
    leaked  <- fromVal =<< op2Match rv (VType $ MkType "IO")
    when leaked $ do
        fail $ "BEGIN and CHECK blocks may not return IO handles,\n" ++
               "as they would be invalid at runtime."
    return rv
op1 "system" = \v -> do
    cmd         <- fromVal v
    exitCode    <- guardIO $ system cmd
    case exitCode of
        ExitFailure x -> do
            glob    <- askGlobal
            errSV   <- findSymRef "$!" glob
            writeRef errSV (VInt $ toInteger x)
            return $ VBool False
        ExitSuccess -> return $ VBool True
op1 "accept" = \v -> do
    socket      <- fromVal v
    (h, _, _)   <- guardIO $ accept socket
    return $ VHandle h
op1 "detach" = \v -> do
    case v of
        VThread thr -> do
            liftSTM $ tryPutTMVar (threadLock thr) undef
            return $ VBool True
        _           -> fail $ "Not a thread: " ++ show v
op1 "kill" = \v -> do
    case v of
        VThread thr -> do
            guardIO . killThread $ threadId thr
            return $ VBool True
        _           -> fail $ "Not a thread: " ++ show v
op1 "join" = \v -> do
    case v of
        VThread thr -> liftSTM $ takeTMVar (threadLock thr)
        _           -> op2Join v (VList [])
op1 "async" = \v -> do
    env     <- ask
    code    <- fromVal v
    lock    <- liftSTM $ newEmptyTMVar
    tid     <- guardIO . (if rtsSupportsBoundThreads then forkOS else forkIO) $ do
        val <- runEvalIO env $ do
            enterEvalContext CxtVoid $ App (Val code) Nothing []
        liftSTM $ tryPutTMVar lock val
        return ()
    return . VThread $ MkThread
        { threadId      = tid
        , threadLock    = lock
        }
op1 "listen" = \v -> do
    port    <- fromVal v
    socket  <- guardIO $ listenOn (PortNumber $ fromInteger port)
    return $ VSocket socket
op1 "flush" = guardedIO hFlush
op1 "close" = \v -> do
    case v of
        (VSocket _) -> guardedIO sClose v
        _           -> guardedIO hClose v
op1 "key" = fmap fst . (fromVal :: Val -> Eval VPair)
op1 "value" = \v -> do
    ivar <- join $ doPair v pair_fetchElem
    return . VRef . MkRef $ ivar
op1 "pairs" = \v -> do
    pairs <- pairsFromVal v
    returnList pairs
op1 "List::kv" = \v -> do
    pairs <- pairsFromVal v
    kvs   <- forM pairs $ \(VRef ref) -> do
        pair   <- readRef ref
        fromVal pair
    return (VList $ concat kvs)
op1 "Pair::kv" = op1 "List::kv"
op1 "keys" = keysFromVal
op1 "values" = valuesFromVal
-- According to Damian
-- (http://www.nntp.perl.org/group/perl.perl6.language/21895),
-- =$obj should call $obj.next().
op1 "="        = \v -> case v of
    VObject _   -> evalExp $ App (Var "&shift") (Just $ Val v) []
    _           -> op1 "readline" v
op1 "readline" = \v -> op1Read v (getLines) (getLine)
    where
    getLines :: VHandle -> Eval Val
    getLines fh = do
        line <- getLine fh
        if defined line
            then do
                (VList rest) <- getLines fh
                return $ VList (line:rest)
            else return $ VList []
    getLine :: VHandle -> Eval Val
    getLine fh = guardIOexcept [(isEOFError, undef)] $
        fmap (VStr . (++ "\n") . decodeUTF8) (hGetLine fh)
op1 "getc"     = \v -> op1Read v (getChar) (getChar)
    where
    getChar :: VHandle -> Eval Val
    getChar fh = guardIOexcept [(isEOFError, undef)] $ do
        char <- hGetChar fh
        str  <- getChar' fh char
        return $ VStr $ decodeUTF8 str
    -- We may have to read more than one byte, as one utf-8 char can span
    -- multiple bytes.
    getChar' :: VHandle -> Char -> IO String
    getChar' fh char
        | ord char < 0x80 = return [char]
        | ord char < 0xE0 = readNmore 1
        | ord char < 0xEE = readNmore 2
        | ord char < 0xF5 = readNmore 3
        | otherwise       = fail "Invalid utf-8 read by getc()"
        where
        readNmore :: Int -> IO String
        readNmore n = do
            new <- sequence $ replicate n (hGetChar fh)
            return $ char:new

op1 "ref"   = fmap VType . evalValType
op1 "List::pop"   = \x -> join $ doArray x array_pop -- monadic join
op1 "List::shift" = \x -> join $ doArray x array_shift -- monadic join
op1 "pick"  = op1Pick
op1 "sum"   = op1Sum
op1 "min"   = op1Min
op1 "max"   = op1Max
op1 "uniq"  = op1Uniq
op1 "chr"   = op1Cast (VStr . (:[]) . chr)
op1 "ord"   = op1Cast $ \str -> if null str then undef else (castV . ord . head) str
op1 "hex"   = op1Convert "0x" "hexadecimal"
op1 "oct"   = op1Convert "0o" "octal"
op1 "log"   = op1Cast (VNum . log)
op1 "log10" = op1Cast (VNum . logBase 10)
op1 "from"  = op1Cast (castV . matchFrom)
op1 "to"    = op1Cast (castV . matchTo)
op1 "matches" = op1Cast (VList . matchSubPos)
op1 "gather" = \v -> do
    evl <- asks envEval
    evl (Syn "gather" [Val v])
op1 "Thread::yield" = const $ do
    ok <- tryIO False $ do { yield ; return True } -- change to guardIO?
    return $ VBool ok
op1 "DESTROYALL" = op1WalkAllNoArgs id "DESTROY"
-- [,] is a noop -- It simply returns the input list
op1 "prefix:[,]" = return
op1 "Code::assoc" = op1CodeAssoc
op1 "Code::name"  = op1CodeName
op1 "Code::arity" = op1CodeArity
op1 "Code::body"  = op1CodeBody
op1 "Code::pos"   = op1CodePos
op1 "IO::tell"    = \v -> do
    h <- fromVal v
    res <- guardIO $ hTell h
    return $ VInt res
op1 "TEMP" = \v -> do
    ref <- fromVal v
    val <- readRef ref
    return . VCode $ mkPrim
        { subBody = Prim . const $ do
            writeRef ref val
            retEmpty
        }
op1 "Pugs::Internals::hIsOpen" = op1IO hIsOpen
op1 "Pugs::Internals::hIsClosed" = op1IO hIsClosed
op1 "Pugs::Internals::hIsReadable" = op1IO hIsReadable
op1 "Pugs::Internals::hIsWritable" = op1IO hIsWritable
op1 "Pugs::Internals::hIsSeekable" = op1IO hIsSeekable
op1 "Pugs::Internals::reduceVar" = \v -> do
    str <- fromVal v
    evalExp (Var str)
op1 "Pugs::Internals::rule_pattern" = \v -> do
    case v of
        VRule MkRulePGE{rxRule=re} -> return $ VStr re
        VRule MkRulePCRE{rxRuleStr=re} -> return $ VStr re
        _ -> fail $ "Not a rule: " ++ show v
op1 "Pugs::Internals::rule_adverbs" = \v -> do
    case v of
        VRule MkRulePGE{rxAdverbs=hash} -> return hash
        VRule MkRulePCRE{rxAdverbs=hash} -> return hash
        _ -> fail $ "Not a rule: " ++ show v
op1 "Pugs::Internals::current_pragma_value" = \v -> do
    name <- fromVal v
    prags <- asks envPragmas
    return $ findPrag name prags
    where
        findPrag :: String -> [Pragma] -> Val
        findPrag _ [] = VUndef
        findPrag n (this:rest)
            | n == pragName this = VInt $ toInteger $ pragDat this
            | otherwise          = findPrag n rest
op1 "Pugs::Internals::caller_pragma_value" = \v -> do
    caller <- asks envCaller
    case caller of
        Just env -> local (const env) (op1 "Pugs::Internals::current_pragma_value" v)
        _        -> return $ VUndef
op1 other   = \_ -> fail ("Unimplemented unaryOp: " ++ other)

op1IO :: Value a => (Handle -> IO a) -> Val -> Eval Val
op1IO = \fun v -> do
    val <- fromVal v
    fmap castV (guardIO $ fun val)

returnList :: [Val] -> Eval Val
returnList vals = ifListContext
    (return . VRef $ arrayRef vals)
    (return . VList $ vals)

op1Convert :: VStr -> VStr -> Val -> Eval Val
op1Convert prefix desc v = do
    s <- fromVal v
    res <- tryIO VUndef $ fmap VInt (readIO $ prefix ++ s)
    when (res == VUndef) $
        -- throw exception (?)
        fail $ "Cannot parse " ++ desc ++ " string: " ++ s
    return res

op1WalkAllNoArgs :: ([VStr] -> [VStr]) -> VStr -> Val -> Eval Val
op1WalkAllNoArgs f meth v = do
    pkgs    <- pkgParents =<< fmap showType (evalValType v)
    forM_ (f pkgs) $ \pkg -> do
        let sym = ('&':pkg) ++ "::" ++ meth
        maybeM (fmap (findSym sym) askGlobal) $ \_ -> do
            enterEvalContext CxtVoid (App (Var sym) (Just $ Val v) [])
    return undef

op1WalkAll :: ([VStr] -> [VStr]) -> VStr -> Val -> Val -> Eval Val
op1WalkAll f meth v hashval = do
    pkgs    <- pkgParents =<< fmap showType (evalValType v)
    named   <- join $ doHash hashval hash_fetch
    forM_ (f pkgs) $ \pkg -> do
        let sym = ('&':pkg) ++ "::" ++ meth
        maybeM (fmap (findSym sym) askGlobal) $ \_ -> do
            enterEvalContext CxtVoid (App (Var sym) (Just $ Val v)
                [ Syn "named" [Val (VStr key), Val val]
                | (key, val) <- Map.assocs named ])
    return undef

op1Return :: Eval Val -> Eval Val
op1Return action = do
    depth <- asks envDepth
    if depth == 0 then fail "cannot return() outside a subroutine" else do
    sub   <- fromVal =<< readVar "&?SUB"
    -- If this is a coroutine, reset the entry point
    case subCont sub of
        Nothing -> action
        Just tvar -> do
            let thunk = (`MkThunk` anyType) . fix $ \redo -> do
                evalExp $ subBody sub
                liftSTM $ writeTVar tvar thunk
                redo
            liftSTM $ writeTVar tvar thunk
            action

op1Yield :: Eval Val -> Eval Val
op1Yield action = do
    depth <- asks envDepth
    if depth == 0 then fail "cannot yield() outside a coroutine" else do
    sub   <- fromVal =<< readVar "&?SUB"
    case subCont sub of
        Nothing -> fail $ "cannot yield() from a " ++ pretty (subType sub)
        Just tvar -> callCC $ \esc -> do
            liftSTM $ writeTVar tvar (MkThunk (esc undef) anyType)
            action

op1ShiftOut :: Val -> Eval Val
op1ShiftOut v = shiftT . const $ do
    evl <- asks envEval
    evl $ case v of
        VList [x]   -> Val x
        _           -> Val v

op1Exit :: Val -> Eval a
op1Exit v = do
    rv <- fromVal v
    if rv /= 0
        then shiftT . const . return . VControl . ControlExit . ExitFailure $ rv
        else shiftT . const . return . VControl . ControlExit $ ExitSuccess

op1StrFirst :: (Char -> Char) -> Val -> Eval Val
op1StrFirst f = op1Cast $ VStr .
    \str -> case str of
        []      -> []
        (c:cs)  -> (f c:cs)

{-|
Read a char or a line from a handle.
-}
op1Read :: Val                   -- ^ The handle to read from (packed in a 'Val')
        -> (VHandle -> Eval Val) -- ^ The function to call in list context
        -> (VHandle -> Eval Val) -- ^ The function to call in item context
        -> Eval Val              -- ^ The return value (a list of strings or a
                                 --   string, packed in a 'Val')
op1Read v fList fScalar = do
    -- XXX: primOp doesn't filter this prim, dunno why.
    if safeMode then fail "Can't use readline() or getc() in safemode." else do
    fh  <- handleOf v
    ifListContext
        (fList fh)
        (fScalar fh)
    where
    handleOf VUndef = handleOf (VList [])
    handleOf (VList []) = do
        argsGV  <- readVar "$*ARGS"
        gv      <- fromVal argsGV
        if defined gv
            then handleOf gv
            else do
                args    <- readVar "@*ARGS"
                files   <- fromVal args
                if null files
                    then return stdin
                    else do
                        hdl <- handleOf (VStr (head files)) -- XXX wrong
                        writeVar "$*ARGS" (VHandle hdl)
                        return hdl
    handleOf (VStr x) = do
        return =<< guardIO $ openFile x ReadMode
    handleOf (VList [x]) = handleOf x
    handleOf v = fromVal v

bool2n :: Bool -> VInt
bool2n v = if v
  then 1
  else 0

doBoolIO :: Value a => (a -> IO b) -> Val -> Eval Bool
doBoolIO f v = do
    x <- fromVal v
    tryIO False $ do
        f x
        return True

guardedIO :: Value a => (a -> IO b) -> Val -> Eval Val
guardedIO f v = do
    x <- fromVal v
    guardIO $ f x
    return $ VBool True

guardedIO2 :: (Value a, Value b)
    => (a -> b -> IO c) -> Val -> Val -> Eval Val
guardedIO2 f u v = do
    x <- fromVal u
    y <- fromVal v
    guardIO $ f x y
    return $ VBool True

mapStr :: (Word8 -> Word8) -> [Word8] -> String
mapStr f = map (chr . fromEnum . f)

mapStr2 :: (Word8 -> Word8 -> Word8) -> [Word8] -> [Word8] -> String
mapStr2 f x y = map (chr . fromEnum . uncurry f) $ x `zip` y

mapStr2Fill :: (Word8 -> Word8 -> Word8) -> [Word8] -> [Word8] -> String
mapStr2Fill f x y = map (chr . fromEnum . uncurry f) $ x `zipFill` y
    where
    zipFill [] [] = []
    zipFill as [] = zip as (repeat 0)
    zipFill [] bs = zip (repeat 0) bs
    zipFill (a:as) (b:bs) = (a,b) : zipFill as bs


-- |Implementation of 2-arity primitive operators and functions
op2 :: String -> Val -> Val -> Eval Val
op2 "rename" = guardedIO2 rename
op2 "symlink" = guardedIO2 createSymbolicLink
op2 "link" = guardedIO2 createLink
op2 "*"  = op2Numeric (*)
op2 "/"  = op2Divide
op2 "%"  = op2Modulus
op2 "x"  = op2Cast (\x y -> VStr . concat $ (y :: VInt) `genericReplicate` x)
op2 "xx" = op2Cast (\x y -> VList . concat $ (y :: VInt) `genericReplicate` x)
op2 "+&" = op2Int (.&.)
op2 "+<" = op2Int shiftL
op2 "+>" = op2Int shiftR
op2 "~&" = op2Str $ mapStr2 (.&.)
op2 "~<" = op2Cast (\x y -> VStr $ mapStr (`shiftL` y) x)
op2 "~>" = op2Cast (\x y -> VStr $ mapStr (`shiftR` y) x)
op2 "**" = op2Exp
op2 "+"  = op2Numeric (+)
op2 "-"  = op2Numeric (-)
op2 "atan" = op2Num atan2
op2 "~"  = op2Str (++)
op2 "+|" = op2Int (.|.)
op2 "+^" = op2Int xor
op2 "~|" = op2Str $ mapStr2Fill (.|.)
op2 "?|" = op2Bool (||)
op2 "~^" = op2Str $ mapStr2Fill xor
op2 "=>" = \x y -> return $ castV (x, y)
op2 "="  = \x y -> evalExp (Syn "=" [Val x, Val y])
op2 "cmp"= op2Ord vCastStr
op2 "<=>"= op2Ord vCastRat
op2 ".." = op2Range
op2 "..^" = op2RangeExclRight
op2 "^.." = op2RangeExclLeft
op2 "^..^" = op2RangeExclBoth
op2 "!=" = op2Cmp vCastRat (/=)
op2 "==" = op2Cmp vCastRat (==)
op2 "<"  = op2Cmp vCastRat (<)
op2 "<=" = op2Cmp vCastRat (<=)
op2 ">"  = op2Cmp vCastRat (>)
op2 ">=" = op2Cmp vCastRat (>=)
op2 "ne" = op2Cmp vCastStr (/=)
op2 "eq" = op2Cmp vCastStr (==)
op2 "lt" = op2Cmp vCastStr (<)
op2 "le" = op2Cmp vCastStr (<=)
op2 "gt" = op2Cmp vCastStr (>)
op2 "ge" = op2Cmp vCastStr (>=)
op2 "~~" = op2Match
op2 "!~" = \x y -> op1Cast (VBool . not) =<< op2Match x y
op2 "=:=" = op2Identity -- XXX wrong, needs to compare container only
op2 "===" = op2Identity -- XXX wrong, needs to compare objects only
op2 "&&" = op2Logical (fmap not . fromVal)
op2 "||" = op2Logical (fmap id . fromVal)
op2 "^^" = \x y -> do
    let xor True True   = VBool False
        xor True False  = x
        xor False True  = y
        xor False False = VBool False
    op2Cast xor x y
op2 "//" = op2Logical (return . defined)
op2 ".[]" = \x y -> do
    evl <- asks envEval
    evl $ Syn "[]" [Val x, Val y]
op2 ".{}" = \x y -> do
    evl <- asks envEval
    evl $ Syn "{}" [Val x, Val y]
-- XXX pipe forward XXX
op2 "and"= op2 "&&"
op2 "or" = op2 "||"
op2 "xor"= op2 "^^"
op2 "err"= op2 "//"
op2 "grep" = op2Grep
op2 "map"  = op2Map
op2 "join" = op2Join
op2 "reduce" = op2FoldL
op2 "kill" = \s v -> do
    sig  <- fromVal s
    pids <- fromVals v
    sig' <- fromVal sig
    pids'<- mapM fromVal pids
    let doKill pid = do
        signalProcess (toEnum sig') (toEnum pid)
        return 1
    rets <- mapM (tryIO 0 . doKill) pids'
    return . VInt $ sum rets
op2 "does"  = op2 "isa" -- XXX not correct
op2 "isa"   = \x y -> do
    typY <- case y of
        VStr str -> return $ mkType str
        _        -> fromVal y
    op2Match x (VType typY)
op2 "delete" = \x y -> do
    ref <- fromVal x
    deleteFromRef ref y
op2 "exists" = \x y -> do
    ref <- fromVal x
    fmap VBool (existsFromRef ref y)
op2 "unshift" = op2Array array_unshift
op2 "push" = op2Array array_push
op2 "split" = op2Split
op2 "Str::split" = flip op2Split
op2 "connect" = \x y -> do
    host <- fromVal x
    port <- fromVal y
    hdl  <- guardIO $ connectTo host (PortNumber $ fromInteger port)
    return $ VHandle hdl
op2 "Pugs::Internals::hSetBinaryMode" = \x y -> do
    fh    <- fromVal x
    mode  <- fromVal y
    guardIO $ hSetBinaryMode fh mode
    return $ VBool True
op2 "Pugs::Internals::openFile" = \x y -> do
    filename <- fromVal x
    mode     <- fromVal y
    hdl      <- guardIO $ openFile filename (modeOf mode)
    return $ VHandle $ hdl
    where
    modeOf "r"  = ReadMode
    modeOf "w"  = WriteMode
    modeOf "a"  = AppendMode
    modeOf "rw" = ReadWriteMode
    modeOf m    = error $ "unknown mode: " ++ m
op2 "exp" = \x y -> if defined y
    then op2Num (**) x y
    else op1Cast (VNum . exp) x
op2 "Pugs::Internals::sprintf" = \x y -> do
    -- a single argument is all Haskell can really handle.
    -- XXX printf should be wrapped in a catch so a mis-typed argument
    -- doesnt kill pugs with a runtime exception.
    -- XXX fail... doesnt?!
    str <- fromVal x
    arg <- fromVal y
    return $ VStr $ case arg of
       VNum n -> printf str n
       VRat r -> printf str ((fromRational r)::Double)
       VInt i -> printf str i
       VStr s -> printf str s
       _      -> fail "should never be reached given the type declared below"
op2 "system" = \x y -> do
    prog        <- fromVal x
    args        <- fromVals y
    exitCode    <- guardIO $ rawSystem prog args
    case exitCode of
        ExitFailure x -> do
            glob    <- askGlobal
            errSV   <- findSymRef "$!" glob
            writeRef errSV (VInt $ toInteger x)
            return $ VBool False
        ExitSuccess -> return $ VBool True
op2 "chmod" = \x y -> do
    mode  <- fromVal x
    files <- fromVals y
    rets  <- mapM (doBoolIO . flip setFileMode $ toEnum mode) files
    return . VInt . sum $ map bool2n rets
op2 "List::splice" = \x y -> do
    fetchSize   <- doArray x array_fetchSize
    len'        <- fromVal y
    sz          <- fetchSize
    let len = if len' < 0 then if sz > 0 then (len' `mod` sz) else 0 else len'
    op4 "List::splice" x y (castV (sz - len)) (VList [])
op2 "sort" = \x y -> do
    xs <- fromVals x
    ys <- fromVals y
    op1 "sort" . VList $ xs ++ ys
op2 "IO::say" = op2Print hPutStrLn
op2 "IO::print" = op2Print hPutStr
op2 "BUILDALL" = op1WalkAll reverse "BUILD"
op2 "Pugs::Internals::install_pragma_value" = \x y -> do
    name <- fromVal x
    val  <- fromVal y
    idat <- asks envInitDat
    idatval <- liftSTM $ readTVar idat
    --trace ("installing " ++ name ++ "/" ++ (show val)) $ return ()
    let prag = initPragmas idatval
    liftSTM $ writeTVar idat idatval{initPragmas = 
        MkPrag{ pragName=name, pragDat=val } : prag }
    return (VBool True)
op2 other = \_ _ -> fail ("Unimplemented binaryOp: " ++ other)

op2Print :: (Handle -> String -> IO ()) -> Val -> Val -> Eval Val
op2Print f h v = do
    handle <- fromVal h
    strs   <- mapM fromVal =<< case v of
        VList vs  -> return vs
        _         -> return [v]
    guardIO $ do
        f handle . concatMap encodeUTF8 $ strs
        return $ VBool True

op2Split :: Val -> Val -> Eval Val
op2Split x y = do
    val <- fromVal x
    str <- fromVal y
    case val of
        VRule rx -> do
            chunks <- rxSplit rx str
            return $ VList chunks
        _ -> do
            delim <- fromVal val
            return $ split' delim str
    where
    split' :: VStr -> VStr -> Val
    split' [] xs = VList $ map (VStr . (:[])) xs
    split' glue xs = VList $ map VStr $ split glue xs

-- |Implementation of 3-arity primitive operators and functions
op3 :: String -> Val -> Val -> Val -> Eval Val
op3 "Pugs::Internals::exec" = \x y z -> do
    prog  <- fromVal x
    shell <- fromVal y
    args  <- fromVals z
    exitCode    <- guardIO $ executeFile' prog shell args Nothing
    case exitCode of
        ExitFailure x -> do
            glob    <- askGlobal
            errSV   <- findSymRef "$!" glob
            writeRef errSV (VInt $ toInteger x)
            return $ VBool False
        ExitSuccess -> do
            guardIO $ exitWith ExitSuccess
            return $ VBool True -- not reached ;-)
op3 "Pugs::Internals::caller" = \x y z -> do
    --kind <- fromVal =<< op1 "ref" x
    kind <- case x of
        VStr str -> return $ mkType str
        _        -> fromVal x
    skip <- fromVal y
    when (skip < 0) $ do
        liftIO $ fail "Pugs::Internals::caller called with negative skip"
    label <- fromVal z
    op3Caller kind skip label
op3 "index" = \x y z -> do
    str <- fromVal x
    sub <- fromVal y
    pos <- fromVal z
    return . VInt $ doIndex 0 str sub pos
    where
    doIndex :: VInt -> VStr -> VStr -> VInt -> VInt
    doIndex n a b p
        | p > 0, null a     = doIndex n a b 0
        | p > 0             = doIndex (n+1) (tail a) b (p-1)
        | b `isPrefixOf` a  = n
        | null a            = -1
        | otherwise         = doIndex (n+1) (tail a) b 0
op3 "rindex" = \x y z -> do
    str <- fromVal x
    sub <- fromVal y
    pos <- fromVal z
    let skip | defined z = length str - pos - length sub
             | otherwise = 0
    return . VInt $ doRindex str sub skip
    where
    doRindex :: VStr -> VStr -> Int -> VInt
    doRindex a b skip
        | skip > 0         = doRindex (init a) b (skip-1)
        | b `isSuffixOf` a = toInteger $ length a - length b
        | null a           = -1
        | otherwise        = doRindex (init a) b 0

op3 "List::splice" = \x y z -> do
    op4 "List::splice" x y z (VList [])
op3 "split" = op3Split
op3 "Str::split" = \x y z -> do
    op3 "split" y x z
op3 "Object::new" = \t n p -> do
    positionals <- fromVal p
    typ     <- fromVal t
    named   <- fromVal n
    attrs   <- liftSTM $ newTVar Map.empty
    writeIVar (IHash attrs) named
    uniq    <- liftIO $ newUnique
    env     <- ask
    unless (positionals == VList []) (fail "Must only use named arguments to new() constructor")
    let obj = VObject $ MkObject
            { objType   = typ
            , objAttrs  = attrs
            , objId     = uniq
            , objOpaque = Nothing
            }
    -- Now start calling BUILD for each of parent classes (if defined)
    op2 "BUILDALL" obj $ (VRef . hashRef) named
    -- Register finalizers by keeping weakrefs somehow
    liftIO $ do
        objRef <- mkWeakPtr obj (Just $ objectFinalizer env obj)
        modifyIORef _GlobalFinalizer (>> finalize objRef)
    return obj
op3 "Pugs::Internals::localtime"  = \x y z -> do
    wantString <- fromVal x
    sec <- fromVal y
    pico <- fromVal z
    c <- guardIO $ toCalendarTime $ TOD (offset + sec) pico
    if wantString then return $ VStr $ calendarTimeToString c else
        returnList $ [ vI $ ctYear c
                     , vI $ (1+) $ fromEnum $ ctMonth c
                     , vI $ ctDay c
                     , vI $ ctHour c
                     , vI $ ctMin c
                     , vI $ ctSec c
                     , VInt $ ctPicosec c
                     , vI $ (1+) $ fromEnum $ ctWDay c
                     , vI $ ctYDay c
                     , VStr $ ctTZName c
                     , vI $ ctTZ c
                     , VBool $ ctIsDST c
                     ]
    where
       offset = 946684800 :: Integer -- diff between Haskell and Perl epochs (seconds)
       vI = VInt . toInteger
op3 "Pugs::Internals::hSeek" = \x y z -> do
    handle <- fromVal x
    pos <- fromVal y
    mode <- fromVal z
    guardIO $ hSeek handle (modeOf mode) pos
    retEmpty
    where
        modeOf :: Int -> SeekMode
        modeOf 0 = AbsoluteSeek
        modeOf 1 = RelativeSeek
        modeOf 2 = SeekFromEnd
        modeOf m = error ("Unknown seek mode: " ++ (show m))
op3 other = \_ _ _ -> fail ("Unimplemented 3-ary op: " ++ other)

op3Split :: Val -> Val -> Val -> Eval Val
op3Split x y z = do
    val <- fromVal x
    str <- fromVal y
    limit <- fromVal z
    case val of
        VRule rx -> do
            chunks <- rxSplit_n rx str limit
            return $ VList chunks
        _ -> do
            delim <- fromVal val
            return $ split' delim str limit
    where
    split' :: VStr -> VStr -> Int -> Val
    split' [] xs n = VList $ (map (VStr . (:[])) (take (n-1) xs)) ++ [ VStr $ drop (n-1) xs ]
    split' glue xs n = VList $ map VStr $ split_n glue xs n

-- |Implementation of 4-arity primitive operators and functions.
-- Only substr and splice
op4 :: String -> Val -> Val -> Val -> Val -> Eval Val
op4 "substr" = \x y z w -> do
    str  <- fromVal x
    pos  <- fromVal y
    lenP <- fromVal z
    let len | defined z = lenP
            | otherwise = length str
        (pre, result, post) = doSubstr str pos len
    let change = \new -> do
        var <- fromVal x
        rep <- fromVal new
        writeRef var (VStr $ concat [pre, rep, post])
    -- If the replacement is given in w, change the str.
    when (defined w && result /= VUndef) $ change w
    -- Return a proxy which will modify the str if assigned to.
    return $ VRef . MkRef $ proxyScalar (return result) change
    where
    doSubstr :: VStr -> Int -> Int -> (VStr, Val, VStr)
    doSubstr str pos len
        | abs pos > length str = ("", VUndef, "")
        | pos < 0   = doSubstr str (length str + pos) len
        | len < 0   = doSubstr str pos (length str - pos + len)
        | otherwise = ((take pos str), VStr (take len $ drop pos str), (drop (pos + len) str))

-- op4 "splice" = \x y z w-> do
op4 "List::splice" = \x y z w -> do
    splice  <- doArray x array_splice
    start   <- fromVal y
    count   <- fromVal z
    vals    <- fromVals w
    vals'   <- splice start count vals
    ifListContext
        (return $ VList vals')
        (return $ last (undef:vals'))

op4 other = \_ _ _ _ -> fail ("Unimplemented 4-ary op: " ++ other)

op1Range :: Val -> Eval Val
op1Range (VStr s)    = return . VList $ map VStr $ strRangeInf s
op1Range (VRat n)    = return . VList $ map VRat [n ..]
op1Range (VNum n)    = return . VList $ map VNum [n ..]
op1Range (VInt n)    = return . VList $ map VInt [n ..]
op1Range x           = do
    int <- fromVal x
    op1Range (VInt int)

op2Range :: Val -> Val -> Eval Val
op2Range (VStr s) y  = do
    y'  <- fromVal y
    return . VList $ map VStr $ strRange s y'
op2Range (VNum n) y  = do
    y'  <- fromVal y
    return . VList $ map VNum [n .. y']
op2Range x (VNum n)  = do
    x'  <- fromVal x
    return . VList $ map VNum [x' .. n]
op2Range (VRat n) y  = do
    y'  <- fromVal y
    return . VList $ map VRat [n .. y']
op2Range x (VRat n)  = do
    x'  <- fromVal x
    return . VList $ map VRat [x' .. n]
op2Range x y         = do
    x'  <- fromVal x
    y'  <- fromVal y
    return . VList $ map VInt [x' .. y']

op2RangeExclRight :: Val -> Val -> Eval Val
op2RangeExclRight x y = do
    VList vals <- op2Range x y
    return . VList $ init vals

op2RangeExclLeft :: Val -> Val -> Eval Val
op2RangeExclLeft x y = do
    VList vals <- op2Range x y
    return . VList $ tail vals

op2RangeExclBoth :: Val -> Val -> Eval Val
op2RangeExclBoth x y = do
    VList vals <- op2Range x y
    return . VList $ init (tail vals)

op2ChainedList :: Val -> Val -> Val
op2ChainedList x y
    | VList xs <- x, VList ys <- y  = VList $ xs ++ ys
    | VList xs <- x                 = VList $ xs ++ [y]
    | VList ys <- y                 = VList (x:ys)
    | otherwise                     = VList [x, y]

op2Logical :: (Val -> Eval Bool) -> Val -> Val -> Eval Val
op2Logical f x y = do
    ok <- f x
    if ok then return x else do
    ref <- fromVal y
    forceRef ref

op2DefinedOr :: Val
op2DefinedOr = undefined

op2Identity :: Val -> Val -> Eval Val
op2Identity (VObject x) (VObject y) = return $ VBool (objId x == objId y)
op2Identity (VRef ref) y = do
    x <- readRef ref
    op2Identity x y
op2Identity x (VRef ref) = do
    y <- readRef ref
    op2Identity x y
op2Identity x y = do
    return $ VBool (x == y)

op2Cmp :: (a -> Eval b) -> (b -> b -> VBool) -> a -> a -> Eval Val
op2Cmp f cmp x y = do
    x' <- f x
    y' <- f y
    return $ VBool $ x' `cmp` y'

op2Ord :: (Ord ord) => (a -> Eval ord) -> a -> a -> Eval Val
op2Ord f x y = do
    x' <- f x
    y' <- f y
    return $ VInt $ case x' `compare` y' of
        LT -> -1
        EQ -> 0
        GT -> 1

op3Caller :: Type -> Int -> Val -> Eval Val
--op3Caller kind skip label = do
op3Caller kind skip _ = do                                 -- figure out label
    chain <- callChain =<< ask
    formatFrame $ filter labelFilter $ drop skip $ filter kindFilter chain
    where
    formatFrame :: [(Env, Maybe VCode)] -> Eval Val
    formatFrame [] = retEmpty
    formatFrame ((env, Just sub):_) = returnList
        [ VStr $ envPackage env                        -- .package
        , VStr $ posName $ envPos env                  -- .file
        , VInt $ toInteger $ posBeginLine $ envPos env -- .line
        , VStr $ subName sub                           -- .subname
        , VStr $ show $ subType sub                    -- .subtype
        , VCode $ sub                                  -- .sub
        -- TODO: add more things as they are specced.
        ]
    formatFrame ((env, _):_) = returnList
        [ VStr $ envPackage env                        -- .package
        , VStr $ posName $ envPos env                  -- .file
        , VInt $ toInteger $ posBeginLine $ envPos env -- .line
        ]
    kindFilter :: (Env, Maybe VCode) -> Bool
    kindFilter (_, Just sub) =
        case (kind, subType sub) of
            (MkType "Any",      _)          -> True  -- I hope this is optimized
            (MkType "Method",   SubMethod)  -> True
            (MkType "Sub",      SubRoutine) -> True
            (MkType "Block",    SubBlock)   -> True
            (MkType "Block",    SubPointy)  -> True
            (_,                 _)          -> False
    kindFilter _ = kind == MkType "Any"
    labelFilter _ = True                             -- TODO: figure out how
    callChain :: Env -> Eval [(Env, Maybe VCode)]
    callChain cur = 
        case envCaller cur of
            Just caller -> do
                val <- local (const caller) (readVar "&?SUB")
                if (val == undef) then return [(caller, Nothing)] else do
                sub <- fromVal val
                rest <- callChain caller
                return ((caller, Just sub) : rest)
            _           -> return []


-- |Returns a transaction to install a primitive operator using
-- 'Pugs.AST.genMultiSym'.
-- The associativity determines the arity and fixity of ops.
-- The primitive\'s subBody is defined in 'op0', 'op1', etc depending on arity,
-- the default is 'op0'.
-- The Pad symbol name is prefixed with \"&*\" for functions and
-- \"&*\" ~ fixity ~ \":\" for operators.
primOp :: String -> String -> Params -> String -> Bool -> STM (Pad -> Pad)
primOp sym assoc prms ret isSafe =
    -- In safemode, we filter all prims marked as "unsafe".
    if (not isSafe) && safeMode
        then return id
        else genMultiSym name sub
    where
    name | isAlpha (head sym)
         , fixity == "prefix"
         = "&*" ++ sym
         | otherwise
         = "&*" ++ fixity ++ (':':sym)
    sub  = codeRef $ mkPrim
        { subName     = sym
        , subType     = if sym == "Object::new" then SubMethod else SubPrim
        , subAssoc    = assoc
        , subParams   = prms
        , subReturns  = mkType ret
        , subBody     = Prim f
        }
    symStr = encodeUTF8 sym
    f :: [Val] -> Eval Val
    f    = case (arity :: Integer) of
        0 -> \x -> op0 symStr x
        1 -> \x     -> case x of
            [a]       -> op1 symName a
            [a,b]     -> op2 symStr a b
            [a,b,c]   -> op3 symStr a b c
            [a,b,c,d] -> op4 symStr a b c d
            a         -> op0 symStr a
        2 -> \[x,y] -> op2 symStr x y
        3 -> \[x,y,z] -> op3 symStr x y z
        4 -> \[x,y,z,w] -> op4 symStr x y z w
        _ -> error (show arity)
    symName = if modify then assoc ++ ":" ++ symStr else symStr
    -- prefix symName with post, circum or other (not yet used)
    -- to disambiguate, for example, &*prefix:++ and &*postfix:++ in 'op0'
    (arity, fixity, modify) = case assoc of
        "pre"       -> (1, "prefix", False)
        "spre"      -> (1, "prefix", False)
        "post"      -> (1, "postfix", True)
        "circum"    -> (1, "circumfix", True)
        "left"      -> (2, "infix", False)
        "right"     -> (2, "infix", False)
        "non"       -> (2, "infix", False)
        "chain"     -> (2, "infix", False)
        "list"      -> (0, "infix", False)
        other       -> (0, other, True)

-- |Produce a Pad update transaction with 'primOp' from a string description
primDecl :: String -> STM (Pad -> Pad)
primDecl str = primOp sym assoc params ret (safe == "safe")
    where
    (ret:assoc:sym:safe:prms) = words str
    takeWord = takeWhile isWord . dropWhile (not . isWord)
    isWord = not . (`elem` "(),:")
    prms'  = map takeWord prms
    prms'' = foldr foldParam [] prms'
    params = map (\p -> p{ isWritable = isLValue p }) prms''


-- op1 "perl"
prettyVal :: Int -> Val -> Eval VStr
prettyVal 1024 _ = return " ... "
prettyVal d v@(VRef r) = do
    v'  <- readRef r
    ifValTypeIsa v "Pair"
        (case v' of
            VList [ks, vs] -> do
                kStr <- prettyVal (d+1) ks
                vStr <- prettyVal (d+1) vs
                return $ "(" ++ kStr ++ " => " ++ vStr ++ ")"
            _ -> prettyVal (d+1) v'
        )
        (do str <- prettyVal (d+1) v'
            ifValTypeIsa v "Array"
                (return $ ('[':(init (tail str))) ++ "]")
                (ifValTypeIsa v "Hash"
                    (return $ ('{':(init (tail str))) ++ "}")
                    (return ('\\':str)))
        )
prettyVal d (VList vs) = do
    vs' <- mapM (prettyVal (d+1)) vs
    -- (3,) should dump as (3,), not a (3), which would be the same as 3.
    return $ case vs' of
        []  -> "()"
        [x] -> "(" ++ x ++ ",)"
        _   -> "(" ++ concat (intersperse ", " vs') ++ ")"
prettyVal d v@(VObject obj) = do
    -- ... dump the objAttrs
    -- XXX this needs fixing WRT demagicalized pairs:
    -- currently, this'll return Foo.new((attr => "value)), with the inner
    -- parens, which is, of course, wrong.
    hash    <- fromVal v :: Eval VHash
    str     <- prettyVal d (VRef (hashRef hash))
    return $ showType (objType obj)
        ++ ".new(" ++ init (tail str) ++ ")"
prettyVal _ v = return $ pretty v

-- | Call object destructors when GC takes them away
objectFinalizer :: Env -> Val -> IO ()
objectFinalizer env obj = do
    runEvalIO env (evalExp (App (Var "&DESTROYALL") (Just $ Val $ obj) []))
    return ()

-- XXX -- Junctive Types -- XXX --

-- spre is "symbolic pre", that is, operators for which a precedence has
-- already been assigned in Parser.hs

-- |Initial set global symbols to populate the evaluation environment
--  in the form of Pad mutating transactions built with 'primDecl'.
--
--  The source string format is:
--
-- >  ret_val   assoc   op_name [safe|unsafe] args
initSyms :: STM [Pad -> Pad]
initSyms = mapM primDecl syms
    where
    syms = filter (not . null) . lines $ decodeUTF8 "\
\\n   Bool      spre    !       safe   (Bool)\
\\n   Num       spre    +       safe   (Num)\
\\n   Num       pre     abs     safe   (?Num=$_)\
\\n   Int       pre     Pugs::Internals::truncate safe   (?Num=$_)\
\\n   Int       pre     Pugs::Internals::round    safe   (?Num=$_)\
\\n   Int       pre     Pugs::Internals::floor    safe   (?Num=$_)\
\\n   Int       pre     Pugs::Internals::ceiling  safe   (?Num=$_)\
\\n   Num       pre     atan    safe   (Num)\
\\n   Num       pre     atan    safe   (Num, Num)\
\\n   Num       pre     cos     safe   (?Num=$_)\
\\n   Num       pre     sin     safe   (?Num=$_)\
\\n   Num       pre     tan     safe   (?Num=$_)\
\\n   Any       pre     pi      safe   ()\
\\n   Any       pre     self    safe   ()\
\\n   Bool      pre     nothing safe   ()\
\\n   Num       pre     exp     safe   (?Num=$_, ?Num)\
\\n   Num       pre     sqrt    safe   (?Num=$_)\
\\n   Bool      spre    -r      unsafe (?Str=$_)\
\\n   Bool      spre    -w      unsafe (?Str=$_)\
\\n   Bool      spre    -x      unsafe (?Str=$_)\
\\n   Bool      spre    -e      unsafe (?Str=$_)\
\\n   Bool      spre    -z      unsafe (?Str=$_)\
\\n   Int       spre    -s      unsafe (?Str=$_)\
\\n   Bool      spre    -f      unsafe (?Str=$_)\
\\n   Bool      spre    -d      unsafe (?Str=$_)\
\\n   Num       spre    -       safe   (Num)\
\\n   Str       spre    ~       safe   (Str)\
\\n   Bool      spre    ?       safe   (Bool)\
\\n   Str       spre    =       unsafe (?IO)\
\\n   List      spre    =       unsafe (?IO)\
\\n   Str       pre     readline unsafe (?IO)\
\\n   List      pre     readline unsafe (?IO)\
\\n   Str       pre     getc     unsafe (?IO)\
\\n   Int       pre     int     safe   (?Int=$_)\
\\n   List      pre     list    safe   (List)\
\\n   Hash      pre     hash    safe   (List)\
\\n   List      pre     pair    safe   (List)\
\\n   Scalar    pre     item    safe   (Scalar)\
\\n   Any       pre     reverse safe   (rw!Any)\
\\n   Any       pre     reverse safe   (List)\
\\n   Int       spre    +^      safe   (Int)\
\\n   Int       spre    ~^      safe   (Str)\
\\n   Bool      spre    ?^      safe   (Bool)\
\\n   Ref       spre    \\      safe   (rw!Any)\
\\n   List      post    ...     safe   (Str)\
\\n   List      post    ...     safe   (Scalar)\
\\n   Any       pre     undef     safe   ()\
\\n   Any       pre     undefine  safe   (?rw!Any)\
\\n   Str       pre     chop    safe   (?rw!Str=$_)\
\\n   Str       pre     chomp   safe   (?Str=$_)\
\\n   Any       right   =       safe   (rw!Any, Any)\
\\n   Int       pre     index   safe   (Str, Str, ?Int=0)\
\\n   Int       pre     rindex  safe   (Str, Str, ?Int)\
\\n   Int       pre     substr  safe   (rw!Str, Int, ?Int, ?Str)\
\\n   Str       pre     lc      safe   (?Str=$_)\
\\n   Str       pre     lcfirst safe   (?Str=$_)\
\\n   Str       pre     uc      safe   (?Str=$_)\
\\n   Str       pre     ucfirst safe   (?Str=$_)\
\\n   Str       pre     capitalize safe   (?Str=$_)\
\\n   Str       post    ++      safe   (rw!Str)\
\\n   Str       post    --      safe   (rw!Str)\
\\n   Num       post    ++      safe   (rw!Num)\
\\n   Num       post    --      safe   (rw!Num)\
\\n   Str       spre    ++      safe   (rw!Str)\
\\n   Str       spre    --      safe   (rw!Str)\
\\n   Num       spre    ++      safe   (rw!Num)\
\\n   Num       spre    --      safe   (rw!Num)\
\\n   Bool      pre     not     safe   (List)\
\\n   Bool      pre     true    safe   (Bool)\
\\n   List      pre     gather  safe   (Code)\
\\n   List      pre     map     safe   (Code, List)\
\\n   List      pre     grep    safe   (Code, List)\
\\n   List      pre     sort    safe   (Code, List)\
\\n   List      pre     reduce  safe   (Code, List)\
\\n   List      pre     sort    safe   (Array)\
\\n   List      pre     map     safe   (Array: Code)\
\\n   List      pre     grep    safe   (Array: Code)\
\\n   List      pre     sort    safe   (Array: Code)\
\\n   List      pre     reduce  safe   (Array: Code)\
\\n   Any       pre     List::splice  safe   (rw!Array, ?Int=0)\
\\n   Any       pre     List::splice  safe   (rw!Array, Int, Int)\
\\n   Any       pre     List::splice  safe   (rw!Array, Int, Int, List)\
\\n   Int       pre     push    safe   (rw!Array, List)\
\\n   Int       pre     unshift safe   (rw!Array, List)\
\\n   Scalar    pre     List::pop     safe   (rw!Array)\
\\n   Scalar    pre     List::shift   safe   (rw!Array)\
\\n   Scalar    pre     sum     safe   (List)\
\\n   Scalar    pre     min     safe   (List)\
\\n   Scalar    pre     max     safe   (List)\
\\n   List      pre     uniq    safe   (List)\
\\n   Str       pre     join    safe   (Array: Str)\
\\n   Str       pre     join    safe   (Str, List)\
\\n   Any       pre     join    safe   (Thread)\
\\n   Bool      pre     detach  safe   (Thread)\
\\n   List      pre     zip     safe   (List)\
\\n   List      pre     keys    safe   (rw!Hash)\
\\n   List      pre     values  safe   (rw!Hash)\
\\n   List      pre     List::kv      safe   (rw!Hash)\
\\n   List      pre     pairs   safe   (rw!Hash)\
\\n   List      pre     keys    safe   (rw!Array)\
\\n   List      pre     values  safe   (rw!Array)\
\\n   List      pre     List::kv      safe   (rw!Array)\
\\n   List      pre     pairs   safe   (rw!Array)\
\\n   Scalar    pre     delete  safe   (rw!Hash: List)\
\\n   Scalar    pre     delete  safe   (rw!Array: List)\
\\n   Bool      pre     exists  safe   (rw!Hash: Str)\
\\n   Bool      pre     exists  safe   (rw!Array: Int)\
\\n   Str       pre     perl    safe   (rw!Any|Junction|Pair)\
\\n   Any       pre     try     safe   (Code)\
\\n   Any       pre     lazy    safe   (Code)\
\\n   Any       pre     Pugs::Internals::eval    safe   (Str)\
\\n   Any       pre     evalfile     unsafe (Str)\
\\n   Any       pre     Pugs::Internals::eval_parrot  unsafe (Str)\
\\n   Any       pre     Pugs::Internals::eval_perl5   unsafe (Str)\
\\n   Any       pre     Pugs::Internals::eval_haskell unsafe (Str)\
\\n   Any       pre     Pugs::Internals::eval_yaml    safe   (Str)\
\\n   Any       pre     require unsafe (?Str=$_)\
\\n   Any       pre     use     unsafe (?Str=$_)\
\\n   Any       pre     require_haskell unsafe (Str)\
\\n   Any       pre     require_parrot  unsafe (Str)\
\\n   Any       pre     require_perl5   unsafe (Str)\
\\n   Any       pre     last    safe   (?Int=1)\
\\n   Any       pre     next    safe   (?Int=1)\
\\n   Any       pre     redo    safe   (?Int=1)\
\\n   Any       pre     exit    unsafe (?Int=0)\
\\n   Num       pre     rand    safe   (?Num=1)\
\\n   Bool      pre     defined safe   (Any)\
\\n   Str       pre     ref     safe   (rw!Any|Junction|Pair)\
\\n   Str       pre     isa     safe   (rw!Any|Junction|Pair, Str)\
\\n   Str       pre     does    safe   (rw!Any|Junction|Pair, Str)\
\\n   Num       pre     time    safe   ()\
\\n   List      pre     times   safe   ()\
\\n   List      pre     Pugs::Internals::localtime   safe   (Bool, Int, Int)\
\\n   Str       pre     want    safe   ()\
\\n   Str       pre     File::Spec::cwd     unsafe ()\
\\n   Str       pre     File::Spec::tmpdir  unsafe ()\
\\n   Str       pre     IO::next   unsafe (IO)\
\\n   Bool      pre     IO::print   unsafe (IO)\
\\n   Bool      pre     IO::print   unsafe (IO: List)\
\\n   Bool      pre     print   unsafe ()\
\\n   Bool      pre     print   unsafe (List)\
\\n   Str       pre     Pugs::Internals::sprintf safe   (Str, Num|Rat|Int|Str)\
\\n   Bool      pre     IO::say unsafe (IO)\
\\n   Bool      pre     IO::say unsafe (IO: List)\
\\n   Bool      pre     say     unsafe ()\
\\n   Bool      pre     say     unsafe (List)\
\\n   Bool      pre     Pugs::Safe::safe_print     safe     (Str)\
\\n   Bool      pre     close   unsafe (IO)\
\\n   Bool      pre     flush   unsafe (IO)\
\\n   Bool      pre     close   unsafe (Socket)\
\\n   Bool      pre     die     safe   (List)\
\\n   Bool      pre     warn    safe   (List)\
\\n   Bool      pre     fail_   safe   (List)\
\\n   Bool      pre     fail    safe   (List)\
\\n   Socket    pre     listen  unsafe (Int)\
\\n   Socket    pre     connect unsafe (Str, Int)\
\\n   Any       pre     accept  unsafe (Any)\
\\n   List      pre     slurp   unsafe (?Str=$_)\
\\n   List      pre     slurp   unsafe (Handle)\
\\n   List      pre     readdir unsafe (Str)\
\\n   Bool      pre     Pugs::Internals::exec    unsafe (Str, Bool, List)\
\\n   Int       pre     system  unsafe (Str)\
\\n   Int       pre     system  unsafe (Str: List)\
\\n   Bool      pre     binmode unsafe (IO: ?Int=1)\
\\n   Void      pre     return  safe   ()\
\\n   Void      pre     return  safe   (rw!Any)\
\\n   Void      pre     return  safe   (List)\
\\n   Void      pre     yield   safe   ()\
\\n   Void      pre     yield   safe   (rw!Any)\
\\n   Void      pre     yield   safe   (List)\
\\n   Void      pre     take    safe   ()\
\\n   Void      pre     take    safe   (rw!Any)\
\\n   Void      pre     take    safe   (List)\
\\n   Junction  pre     any     safe   (List)\
\\n   Junction  pre     all     safe   (List)\
\\n   Junction  pre     one     safe   (List)\
\\n   Junction  pre     none    safe   (List)\
\\n   Bool      pre     sleep   unsafe (Int)\
\\n   Bool      pre     rmdir   unsafe (?Str=$_)\
\\n   Bool      pre     mkdir   unsafe (Str)\
\\n   Bool      pre     chdir   unsafe (Str)\
\\n   Int       pre     List::elems   safe   (Array)\
\\n   Int       pre     List::end     safe   (Array)\
\\n   Int       pre     graphs  safe   (?Str=$_)\
\\n   Int       pre     codes   safe   (?Str=$_)\
\\n   Int       pre     chars   safe   (?Str=$_)\
\\n   Int       pre     bytes   safe   (?Str=$_)\
\\n   Int       pre     chmod   unsafe (Int, List)\
\\n   Scalar    pre     key     safe   (rw!Pair)\
\\n   Scalar    pre     value   safe   (rw!Pair)\
\\n   List      pre     keys    safe   (rw!Pair)\
\\n   List      pre     values  safe   (Pair|Junction)\
\\n   List      pre     Pair::kv      safe   (rw!Pair)\
\\n   List      pre     pairs   safe   (rw!Pair)\
\\n   Any       pre     pick    safe   (Any|Junction|Pair)\
\\n   Bool      pre     rename  unsafe (Str, Str)\
\\n   Bool      pre     symlink unsafe (Str, Str)\
\\n   Bool      pre     link    unsafe (Str, Str)\
\\n   Int       pre     unlink  unsafe (List)\
\\n   Str       pre     readlink unsafe (Str)\
\\n   List      pre     Str::split   safe   (Str)\
\\n   List      pre     Str::split   safe   (Str: Str)\
\\n   List      pre     Str::split   safe   (Str: Pugs::Internals::VRule)\
\\n   List      pre     Str::split   safe   (Str: Str, Int)\
\\n   List      pre     Str::split   safe   (Str: Pugs::Internals::VRule, Int)\
\\n   List      pre     split   safe   (Str, Str)\
\\n   List      pre     split   safe   (Str, Str, Int)\
\\n   List      pre     split   safe   (Pugs::Internals::VRule, Str)\
\\n   List      pre     split   safe   (Pugs::Internals::VRule, Str, Int)\
\\n   Str       spre    =       safe   (Any)\
\\n   List      spre    =       safe   (Any)\
\\n   Junction  list    |       safe   (Any|Junction|Pair)\
\\n   Junction  list    &       safe   (Any|Junction|Pair)\
\\n   Junction  list    ^       safe   (Any|Junction|Pair)\
\\n   Num       left    *       safe   (Num, Num)\
\\n   Num       left    /       safe   (Num, Num)\
\\n   Num       left    %       safe   (Num, Num)\
\\n   Str       left    x       safe   (Str, Int)\
\\n   List      left    xx      safe   (Any, Int)\
\\n   Int       left    +&      safe   (Int, Int)\
\\n   Int       left    +<      safe   (Int, Int)\
\\n   Int       left    +>      safe   (Int, Int)\
\\n   Str       left    ~&      safe   (Str, Str)\
\\n   Str       left    ~<      safe   (Str, Str)\
\\n   Str       left    ~>      safe   (Str, Str)\
\\n   Num       right   **      safe   (Num, Num)\
\\n   Num       left    +       safe   (Num, Num)\
\\n   Num       left    -       safe   (Num, Num)\
\\n   Str       left    ~       safe   (Str, Str)\
\\n   Int       left    +|      safe   (Int, Int)\
\\n   Int       left    +^      safe   (Int, Int)\
\\n   Str       left    ~|      safe   (Str, Str)\
\\n   Str       left    ~^      safe   (Str, Str)\
\\n   Str       left    ?|      safe   (Str, Str)\
\\n   Pair      right   =>      safe   (Any, Any)\
\\n   Int       non     cmp     safe   (Str, Str)\
\\n   Int       non     <=>     safe   (Num, Num)\
\\n   List      non     ..      safe   (Scalar, Scalar)\
\\n   List      non     ..^     safe   (Scalar, Scalar)\
\\n   List      non     ^..     safe   (Scalar, Scalar)\
\\n   List      non     ^..^    safe   (Scalar, Scalar)\
\\n   Bool      chain   !=      safe   (Num, Num)\
\\n   Bool      chain   ==      safe   (Num, Num)\
\\n   Bool      chain   =:=     safe   (rw!Any, rw!Any)\
\\n   Bool      chain   ===     safe   (Any, Any)\
\\n   Bool      chain   ~~      safe   (rw!Any, Any)\
\\n   Bool      chain   !~      safe   (Any, Any)\
\\n   Bool      chain   <       safe   (Num, Num)\
\\n   Bool      chain   <=      safe   (Num, Num)\
\\n   Bool      chain   >       safe   (Num, Num)\
\\n   Bool      chain   >=      safe   (Num, Num)\
\\n   Bool      chain   ne      safe   (Str, Str)\
\\n   Bool      chain   eq      safe   (Str, Str)\
\\n   Bool      chain   lt      safe   (Str, Str)\
\\n   Bool      chain   le      safe   (Str, Str)\
\\n   Bool      chain   gt      safe   (Str, Str)\
\\n   Bool      chain   ge      safe   (Str, Str)\
\\n   Scalar    left    &&      safe   (Bool, ~Bool)\
\\n   Scalar    left    ||      safe   (Bool, ~Bool)\
\\n   Scalar    left    ^^      safe   (Bool, Bool)\
\\n   Scalar    left    //      safe   (Bool, ~Bool)\
\\n   Scalar    left    .[]     safe   (Array, Int)\
\\n   Scalar    left    .{}     safe   (Hash, Str)\
\\n   List      list    ¥       safe   (Array)\
\\n   List      list    Y       safe   (Array)\
\\n   List      spre    <==     safe   (List)\
\\n   List      left    ==>     safe   (List, Code)\
\\n   Scalar    left    and     safe   (Bool, ~Bool)\
\\n   Scalar    left    or      safe   (Bool, ~Bool)\
\\n   Scalar    left    xor     safe   (Bool, Bool)\
\\n   Scalar    left    err     safe   (Bool, ~Bool)\
\\n   Str       pre     chr     safe   (?Int=$_)\
\\n   Int       pre     ord     safe   (?Str=$_)\
\\n   Str       pre     hex     safe   (?Str=$_)\
\\n   Str       pre     oct     safe   (?Str=$_)\
\\n   Int       pre     from    safe   (Match)\
\\n   Int       pre     to      safe   (Match)\
\\n   List      pre     matches safe   (Match)\
\\n   Str       pre     hex     safe   (Int)\
\\n   Str       pre     oct     safe   (Int)\
\\n   Num       pre     log     safe   (Int)\
\\n   Num       pre     log     safe   (Num)\
\\n   Num       pre     log10   safe   (Num)\
\\n   Thread    pre     async   safe   (Code)\
\\n   Int       pre     sign    safe   (Num)\
\\n   Bool      pre     kill    safe   (Thread)\
\\n   Int       pre     kill    unsafe (Int, List)\
\\n   Object    pre     Object::new     safe   (Object: Named)\
\\n   Object    pre     BUILDALL   safe   (Object)\
\\n   Object    pre     DESTROYALL safe   (Object)\
\\n   Code      pre     TEMP    safe   (rw!Any)\
\\n   Object    pre     clone   safe   (Any)\
\\n   Object    pre     id      safe   (Any)\
\\n   Bool      pre     Thread::yield   safe   (Thread)\
\\n   List      pre     Pugs::Internals::runInteractiveCommand  unsafe (Str)\
\\n   Bool      pre     Pugs::Internals::hSetBinaryMode         unsafe (IO, Str)\
\\n   Void      pre     Pugs::Internals::hSeek                  unsafe (IO, Int, Int)\
\\n   Int       pre     IO::tell                                unsafe (IO)\
\\n   Bool      pre     Pugs::Internals::hIsOpen                unsafe (IO)\
\\n   Bool      pre     Pugs::Internals::hIsClosed              unsafe (IO)\
\\n   Bool      pre     Pugs::Internals::hIsReadable            unsafe (IO)\
\\n   Bool      pre     Pugs::Internals::hIsWritable            unsafe (IO)\
\\n   Bool      pre     Pugs::Internals::hIsSeekable            unsafe (IO)\
\\n   IO        pre     Pugs::Internals::openFile               unsafe (Str, Str)\
\\n   List      pre     Pugs::Internals::caller                 safe (Any, Int, Str)\
\\n   Any       pre     Pugs::Internals::check_for_io_leak      safe (Code)\
\\n   Bool      pre     bool::true  safe   ()\
\\n   Bool      pre     bool::false safe   ()\
\\n   List      spre    prefix:[,]  safe   (List)\
\\n   Str       pre     Code::name    safe   (Code:)\
\\n   Int       pre     Code::arity   safe   (Code:)\
\\n   Str       pre     Code::assoc   safe   (Code:)\
\\n   Code::Exp pre     Code::body    safe   (Code:)\
\\n   Str       pre     Code::pos     safe   (Code:)\
\\n   IO::Dir   pre     opendir    unsafe (Str)\
\\n   Str       pre     IO::Dir::readdir    unsafe (IO::Dir)\
\\n   List      pre     IO::Dir::readdir    unsafe (IO::Dir)\
\\n   Bool      pre     IO::Dir::closedir   unsafe (IO::Dir)\
\\n   Bool      pre     IO::Dir::rewinddir  unsafe (IO::Dir)\
\\n   Any       pre     Pugs::Internals::reduceVar  unsafe (Str)\
\\n   Str       pre     Pugs::Internals::rule_pattern safe (Pugs::Internals::VRule)\
\\n   Hash      pre     Pugs::Internals::rule_adverbs safe (Pugs::Internals::VRule)\
\\n   Int       pre     Pugs::Internals::install_pragma_value safe (Str, Int)\
\\n   Bool      pre     Pugs::Internals::current_pragma_value safe (Str)\
\\n   Bool      pre     Pugs::Internals::caller_pragma_value safe (Str)\
\\n"
