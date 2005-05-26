{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans #-}
{-# OPTIONS_GHC -#include "UnicodeC.h" #-}

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
import qualified Data.Map as Map

import Pugs.Prim.Keyed
import Pugs.Prim.Yaml
import Pugs.Prim.Match
import qualified Pugs.Prim.FileTest as FileTest
import Pugs.Prim.List
import Pugs.Prim.Numeric
import Pugs.Prim.Lifts
import Pugs.Prim.Eval

-- |Implementation of 0-ary and variadic primitive operators and functions
-- (including list ops).
op0 :: String -> [Val] -> Eval Val
op0 "&"  = fmap opJuncAll  . mapM fromVal
op0 "^"  = fmap opJuncOne  . mapM fromVal
op0 "|"  = fmap opJuncAny  . mapM fromVal
op0 "want"  = const $ fmap VStr (asks envWant)
op0 "bool::true" = const $ return (VBool True)
op0 "bool::false" = const $ return (VBool False)
op0 "time"  = const $ do
    clkt <- liftIO getClockTime
    return $ VRat $ fdiff $ diffClockTimes clkt epochClkT
    where
       epochClkT = toClockTime epoch
       epoch = CalendarTime 2000 January 1 0 0 0 0 Saturday 0 "UTC" 0 False
       -- 10^12 is expanded because the alternatives tried gave type warnings.
       fdiff = \d -> (fromInteger $ tdPicosec d)
                   / (clocksPerSecond * clocksPerSecond)
                   + (fromIntegral $ tdSec d)
op0 "times"  = const $ do
    ProcessTimes _ u s cu cs <- liftIO getProcessTimes
    return . VList $ map (castV . (% (clocksPerSecond :: VInt)) . toInteger . fromEnum)
        [u, s, cu, cs]
op0 "so" = const (return $ VBool True)
op0 "¥" = op0Zip
op0 "Y" = op0 "¥"
op0 "File::Spec::cwd" = const $ do
    cwd <- liftIO getCurrentDirectory
    return $ VStr cwd
op0 "File::Spec::tmpdir" = const $ do
    tmp <- liftIO getTemporaryDirectory
    return $ VStr tmp
op0 "pi" = const $ return (VNum pi)
op0 "say" = const $ op1 "say" =<< readVar "$_"
op0 "print" = const $ op1 "print" =<< readVar "$_"
op0 "return" = const $ op1Return (shiftT . const $ retEmpty)
op0 "yield" = const $ op1Yield (shiftT . const $ retEmpty)
op0 "take" = const $ retEmpty
op0 other = const $ fail ("Unimplemented listOp: " ++ other)

-- |Implementation of unary primitive operators and functions
op1 :: String -> Val -> Eval Val
op1 "!"    = op1Cast (VBool . not)
op1 "id" = \x -> do
    (VObject o) <- fromVal x
    return . castV . hashUnique $ objId o
op1 "clone" = \x -> do
    (VObject o) <- fromVal x
    attrs   <- readIVar (IHash $ objAttrs o)
    attrs'  <- liftSTM $ newTVar Map.empty
    uniq    <- liftIO $ newUnique
    writeIVar (IHash attrs') attrs
    return $ VObject o{ objAttrs = attrs', objId = uniq }
op1 "chop" = \x -> do
    ref <- fromVal x
    str <- fromVal x
    if null str
        then return undef
        else do
            writeRef ref $ VStr (init str)
            return $ VStr [last str]
op1 "chomp" = \x -> do
    ref <- fromVal x
    str <- fromVal x
    if null str || last str /= '\n'
        then return undef
        else do
            writeRef ref $ VStr (init str)
            return $ VStr [last str]
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
op1 "undef" = \x -> do
    when (defined x) $ do
        ref <- fromVal x
        clearRef ref
    return undef
op1 "+"    = op1Numeric id
op1 "abs"  = op1Numeric abs
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
op1 "scalar" = return -- XXX refify?
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
            return . VList . map snd . sort $ (strs :: [VStr]) `zip` valList
        Just subVal -> do
            sub <- fromVal subVal
            sorted <- (`sortByM` valList) $ \v1 v2 -> do
                rv  <- enterEvalContext (cxtItem "Int") $ App (Val sub) [Val v1, Val v2] []
                int <- fromVal rv
                return (int <= (0 :: Int))
            return $ VList sorted
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
op1 "post:..."  = op1Cast op1Range
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
    val     <- op1 "eval_perl5" (VStr $ "require " ++ name ++ "; '" ++ name ++ "'");
    evalExp $ Sym SGlobal ('$':'*':name) (Syn ":=" [Var ('$':'*':name), Val val])
    return val
op1 "eval_parrot" = \v -> do
    code    <- fromVal v
    liftIO . evalParrot $ case code of
        ('.':_) -> code
        _       -> ".sub pugs_eval_parrot\n" ++ code ++ "\n.end\n"
    return $ VBool True
op1 "use" = opRequire True
op1 "require" = opRequire False
op1 "eval" = \v -> do
    str <- fromVal v
    opEval Nothing "<eval>" str
op1 "eval_perl5" = \v -> do
    str <- fromVal v
    cxt <- asks envContext
    tryIO undef $ do
        sv <- evalPerl5 str $ enumCxt cxt
        return $ PerlSV sv
op1 "eval_haskell" = op1EvalHaskell
op1 "eval_yaml" = evalYaml
op1 "try" = \v -> do
    sub <- fromVal v
    val <- resetT $ evalExp (App (Val $ VCode sub) [] [])
    retEvalResult False val
-- Tentative implementation of nothingsmuch's lazy proposal.
op1 "lazy" = \v -> do
    sub <- fromVal v
    return $ VRef . thunkRef . MkThunk . evalExp $ App (Val $ VCode sub) [] []
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
    rand <- liftIO $ randomRIO (0, if x == 0 then 1 else x)
    return $ VNum rand
op1 "print" = op1Print hPutStr
op1 "say" = op1Print hPutStrLn
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
    op2 "say" errh $ VList [ VStr $ pretty (VError (errmsg strs) [pos]) ]
    where
    errmsg "" = "Warning: something's wrong"
    errmsg x  = x
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
	else shiftT . const $ return . VRef . thunkRef . MkThunk $ fail msg
    where
    errmsg "" = "Failed"
    errmsg x  = x
op1 "exit" = op1Exit
op1 "readlink" = \v -> do
    str  <- fromVal v
    tryIO undef $ fmap VStr (readSymbolicLink str)
op1 "sleep" = boolIO (threadDelay . (* clocksPerSecond))
op1 "mkdir" = boolIO createDirectory
op1 "rmdir" = boolIO removeDirectory
op1 "chdir" = boolIO setCurrentDirectory
op1 "-r"    = FileTest.isReadable
op1 "-w"    = FileTest.isWritable
op1 "-x"    = FileTest.isExecutable
op1 "-e"    = FileTest.exists
op1 "-z"    = FileTest.sizeIsZero
op1 "-s"    = FileTest.fileSize
op1 "-f"    = FileTest.isFile
op1 "-d"    = FileTest.isDirectory
op1 "end"   = op1Cast (VInt . (-1 +) . (genericLength :: VList -> VInt))
op1 "elems" = op1Cast (VInt . (genericLength :: VList -> VInt))
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
    files <- liftIO $ getDirectoryContents path
    return . VList $ map VStr files
op1 "slurp" = \v -> do
    ifValTypeIsa v "IO"
        (do h <- fromVal v
            ifListContext
                (op1 "=" v)
                (fmap VStr (liftIO $ hGetContents h)))
        (do
            fileName    <- fromVal v
            ifListContext
                (slurpList fileName)
                (slurpScalar fileName))
    where
    slurpList file = op1 "=" (VList [VStr file])
    slurpScalar file = tryIO VUndef $ do
        content <- readFile file
        return $ VStr content
op1 "open" = \v -> do
    str <- fromVal v
    let (mode, filename) = span (`elem` "+<> ") str
    tryIO undef $ do
        fh <- openFile filename (modeOf $ takeWhile (not . isSpace) mode)
        return $ VHandle fh
    where
    modeOf ""   = ReadMode
    modeOf "<"  = ReadMode
    modeOf ">"  = WriteMode
    modeOf ">>" = AppendMode
    modeOf "+>" = ReadWriteMode
    modeOf m    = error $ "unknown mode: " ++ m
op1 "Pugs::Internals::runInteractiveCommand" = \v -> do
    str <- fromVal v
    tryIO undef $ do
        (inp,out,err,phand) <- runInteractiveCommand str
        return $ VList [ VHandle inp
                       , VHandle out
                       , VHandle err
                       , VProcess (MkProcess phand)
                       ]
op1 "system" = \v -> do
    cmd         <- fromVal v
    exitCode    <- liftIO $ system cmd
    case exitCode of
        ExitFailure x -> do
            glob    <- askGlobal
            errSV   <- findSymRef "$!" glob
            writeRef errSV (VInt $ toInteger x)
            return $ VBool False
        ExitSuccess -> return $ VBool True
op1 "accept" = \v -> do
    socket      <- fromVal v
    (h, _, _)   <- liftIO $ accept socket
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
            liftIO . killThread $ threadId thr
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
    tid     <- liftIO . (if rtsSupportsBoundThreads then forkOS else forkIO) $ do
        val <- runEvalIO env $ do
            enterEvalContext CxtVoid $ App (Val code) [] []
        liftSTM $ tryPutTMVar lock val
        return ()
    return . VThread $ MkThread
        { threadId      = tid
        , threadLock    = lock
        }
op1 "listen" = \v -> do
    port    <- fromVal v
    socket  <- liftIO $ listenOn (PortNumber $ fromInteger port)
    return $ VSocket socket
op1 "flush" = boolIO hFlush
op1 "close" = \v -> do
    case v of
        (VSocket _) -> boolIO sClose v
        _           -> boolIO hClose v
op1 "key" = fmap fst . (fromVal :: Val -> Eval VPair)
op1 "value" = \v -> do
    ivar <- join $ doPair v pair_fetchElem
    return . VRef . MkRef $ ivar
op1 "pairs" = \v -> do
    pairs <- pairsFromVal v
    return $ VList pairs
op1 "kv" = \v -> do
    pairs <- pairsFromVal v
    kvs   <- forM pairs $ \(VRef ref) -> do
        pair   <- readRef ref
        fromVal pair
    return (VList $ concat kvs)
op1 "keys" = keysFromVal
op1 "values" = valuesFromVal
op1 "readline" = op1 "="
op1 "=" = \v -> do
    fh  <- handleOf v
    ifListContext
        (getLines fh)
        (getLine fh)
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
    getLine fh = tryIO undef $
        fmap (VStr . (++ "\n") . decodeUTF8) (hGetLine fh)
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
        rv <- tryIO Nothing (fmap Just $ openFile x ReadMode)
        case rv of
            Nothing  -> retError "No such file or directory" x
            Just hdl -> return hdl
    handleOf (VList [x]) = handleOf x
    handleOf v = fromVal v
op1 "ref"   = fmap VType . evalValType
op1 "pop"   = \x -> join $ doArray x array_pop -- monadic join
op1 "shift" = \x -> join $ doArray x array_shift -- monadic join
op1 "pick"  = op1Pick
op1 "sum"   = op1Sum
op1 "min"   = op1Min
op1 "max"   = op1Max
op1 "uniq"  = op1Uniq
op1 "chr"   = op1Cast (VStr . (:[]) . chr)
op1 "ord"   = op1Cast $ \str -> if null str then undef else (castV . ord . head) str
op1 "hex"   = op1Cast (VInt . read . ("0x"++))
op1 "log"   = op1Cast (VNum . log)
op1 "log10" = op1Cast (VNum . logBase 10)
op1 "from"  = op1Cast (castV . matchFrom)
op1 "to"    = op1Cast (castV . matchTo)
op1 "matches" = op1Cast (VList . matchSubPos)
op1 "gather" = \v -> do
    evl <- asks envEval
    evl (Syn "gather" [Val v])
op1 "name" = \v -> do
    sub <- fromVal v
    return . castV $ subName sub
op1 "arity" = \v -> do
    sub <- fromVal v
    return . castV . length $ subParams sub
op1 "Thread::yield" = const $ do
    ok <- tryIO False $ do { yield ; return True }
    return $ VBool ok
op1 "BUILDALL" = \v -> do
    pkgs    <- pkgParents =<< fmap showType (evalValType v)
    forM_ pkgs $ \pkg -> do
        maybeM (fmap (findSym $ ('&':pkg) ++ "::BUILD") askGlobal) $ \tvar -> do
            ref <- liftSTM $ readTVar tvar
            enterEvalContext CxtVoid (App (Val $ VRef ref) [Val v] [])
    return undef
-- [,] is a noop -- It simply returns the input list
op1 "prefix:[,]" = return
op1 other   = \_ -> fail ("Unimplemented unaryOp: " ++ other)

pkgParents :: VStr -> Eval [VStr]
pkgParents pkg = do
    ref     <- readVar (':':'*':pkg)
    if ref == undef then return [] else do
    meta    <- readRef =<< fromVal ref
    fetch   <- doHash meta hash_fetchVal
    attrs   <- fromVal =<< fetch "traits"
    pkgs    <- mapM pkgParents attrs
    return $ nub (pkg:concat pkgs)

op1Return :: Eval Val -> Eval Val
op1Return action = do
    depth <- asks envDepth
    if depth == 0 then fail "cannot return() outside a subroutine" else do
    sub   <- fromVal =<< readVar "&?SUB"
    case subCont sub of
        Nothing -> action
        _       -> fail $ "cannot return() from a " ++ pretty (subType sub)

op1Yield :: Eval Val -> Eval Val
op1Yield action = do
    depth <- asks envDepth
    if depth == 0 then fail "cannot yield() outside a coroutine" else do
    sub   <- fromVal =<< readVar "&?SUB"
    case subCont sub of
        Nothing -> fail $ "cannot yield() from a " ++ pretty (subType sub)
        Just tvar -> callCC $ \esc -> do
            liftSTM $ writeTVar tvar (MkThunk (esc undef))
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

op1Print :: (Handle -> String -> IO ()) -> Val -> Eval Val
op1Print f v = do
    val  <- readRef =<< fromVal v
    vals <- case val of
        VList _   -> fromVal v
        _         -> return [v]
    (handle, vs) <- case vals of
        []      -> return (stdout, vals)
        (h:vs)  -> do
            ifValTypeIsa h "IO"
                (do
                    hdl <- fromVal h
                    vs' <- if null vs then fmap (:[]) (readVar "$_") else return vs
                    return (hdl, vs'))
                (return (stdout, vals))
    vs' <- mapM fromVal vs
    tryIO undef $ do
        f handle . concatMap encodeUTF8 $ vs'
        return $ VBool True

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

boolIO :: Value a => (a -> IO b) -> Val -> Eval Val
boolIO f v = do
    ok <- doBoolIO f v
    return $ VBool ok

boolIO2 :: (Value a, Value b)
    => (a -> b -> IO c) -> Val -> Val -> Eval Val
boolIO2 f u v = do
    x <- fromVal u
    y <- fromVal v
    tryIO (VBool False) $ do
        f x y
        return (VBool True)

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
op2 "rename" = boolIO2 rename
op2 "symlink" = boolIO2 createSymbolicLink
op2 "link" = boolIO2 createLink
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
op2 "cmp"= op2Ord vCastStr
op2 "<=>"= op2Ord vCastRat
op2 ".." = op2Cast op2Range
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
op2 "!~" = op2Cmp vCastStr (/=)
op2 "=:=" = op2Identity
op2 "&&" = op2Logical (fmap not . fromVal)
op2 "||" = op2Logical (fmap id . fromVal)
op2 "^^" = \x y -> do
    let xor True True   = VBool False
        xor True False  = x
        xor False True  = y
        xor False False = VBool False
    op2Cast xor x y
op2 "//" = op2Logical (return . defined)
op2 "!!" = \x y -> callCC $ \esc -> do
    bx <- fromVal x
    when bx $ esc (VBool False)
    by <- fromVal y
    when by $ esc (VBool False)
    return (VBool True)
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
op2 "nor"= op2 "!!"
op2 "grep" = op2Grep
op2 "map"  = op2Map
op2 "join" = op2Join
op2 "reduce" = op2FoldL
op2 "kill" = \s v -> do
    sig  <- fromVal s
    pids <- fromVals v
    let doKill pid = do
        signalProcess (toEnum $ vCast sig) (toEnum $ vCast pid)
        return 1
    rets <- mapM (tryIO 0 . doKill) pids
    return . VInt $ sum rets
op2 "isa"   = \x y -> do
    typ <- fromVal y
    ifValTypeIsa x typ
        (return $ VBool True)
        (return $ VBool False)
op2 "delete" = \x y -> do
    ref <- fromVal x
    deleteFromRef ref y
op2 "exists" = \x y -> do
    ref <- fromVal x
    fmap VBool (existsFromRef ref y)
op2 "unshift" = op2Array array_unshift
op2 "push" = op2Array array_push
op2 "split"= \x y -> do
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
op2 "connect" = \x y -> do
    host <- fromVal x
    port <- fromVal y
    hdl  <- liftIO $ connectTo host (PortNumber $ fromInteger port)
    return $ VHandle hdl
op2 "Pugs::Internals::openFile" = \x y -> do
    mode     <- fromVal x
    filename <- fromVal y
    tryIO undef $ do
        fh <- openFile filename (modeOf mode)
        return $ VHandle fh
    where
    modeOf "<"  = ReadMode
    modeOf ">"  = WriteMode
    modeOf ">>" = AppendMode
    modeOf "+>" = ReadWriteMode
    modeOf m    = error $ "unknown mode: " ++ m
op2 "exp" = \x y -> if defined y
    then op2Num (**) x y
    else op1Cast (VNum . exp) x
-- FIXME: Generalize to N args for arb N?  Is this possible?
op2 "sprintf" = \x y -> do
    str  <- fromVal x
    args <- fromVals y
    return $ VStr $ case (args :: [VInt]) of
        []          -> printf str
        [x]         -> printf str x
        [x, y]      -> printf str x y
        [x, y, z]   -> printf str x y z
        _           -> printf str
op2 "exec" = \x y -> do
    prog  <- fromVal x
    args  <- fromVals y
    tryIO (VBool False) $ do
        executeFile prog True args Nothing
        return $ VBool True
op2 "system" = \x y -> do
    prog        <- fromVal x
    args        <- fromVals y
    exitCode    <- liftIO $ rawSystem prog args
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
op2 "splice" = \x y -> do
    fetchSize   <- doArray x array_fetchSize
    len'        <- fromVal y
    sz          <- fetchSize
    let len = if len' < 0 then if sz > 0 then (len' `mod` sz) else 0 else len'
    op4 "splice" x y (castV (sz - len)) (VList [])
op2 "sort" = \x y -> do
    xs <- fromVals x
    ys <- fromVals y
    op1 "sort" . VList $ xs ++ ys
op2 "say" = \x (VList ys) -> op1Print hPutStrLn (VList (x:ys))
op2 "print" = \x (VList ys) -> op1Print hPutStr (VList (x:ys))
op2 other = \_ _ -> fail ("Unimplemented binaryOp: " ++ other)

-- |Implementation of 3-arity primitive operators and functions
op3 :: String -> Val -> Val -> Val -> Eval Val
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

op3 "splice" = \x y z -> do
    op4 "splice" x y z (VList [])
op3 "new" = \t n _ -> do
    metaTyp <- evalValType t
    typ <- if metaTyp == mkType "Type" then fromVal t else do
        meta    <- readRef =<< fromVal t
        fetch   <- doHash meta hash_fetchVal
        str     <- fromVal =<< fetch "name"
        return $ mkType str
    -- liftIO $ print (t, metaTyp, typ)
    named   <- fromVal n
    attrs   <- liftSTM $ newTVar Map.empty
    writeIVar (IHash attrs) named
    uniq    <- liftIO $ newUnique
    let obj = VObject $ MkObject{ objType = typ, objAttrs = attrs, objId = uniq }
    -- Now start calling BUILD for each of parent classes (if defined)
    op1 "BUILDALL" obj
    return obj
op3 other = \_ _ _ -> fail ("Unimplemented 3-ary op: " ++ other)

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
    when (defined w && result /= VUndef) $ do
        var <- fromVal x
        rep <- fromVal w
        writeRef var (VStr $ concat [pre, rep, post])
    return result
    where
    doSubstr :: VStr -> Int -> Int -> (VStr, Val, VStr)
    doSubstr str pos len
        | abs pos > length str = ("", VUndef, "")
        | pos < 0   = doSubstr str (length str + pos) len
        | len < 0   = doSubstr str pos (length str - pos + len)
        | otherwise = ((take pos str), VStr (take len $ drop pos str), (drop (pos + len) str))

-- op4 "splice" = \x y z w-> do
op4 "splice" = \x y z w -> do
    splice  <- doArray x array_splice
    start   <- fromVal y
    count   <- fromVal z
    vals    <- fromVals w
    vals'   <- splice start count vals
    ifListContext
        (return $ VList vals')
        (return $ last (undef:vals'))

op4 other = \_ _ _ _ -> fail ("Unimplemented 4-ary op: " ++ other)

op1HyperPrefix :: VCode -> Val -> Eval Val
op1HyperPrefix sub (VRef ref) = do
    x <- readRef ref
    op1HyperPrefix sub x
op1HyperPrefix sub x
    | VList x' <- x
    = fmap VList $ hyperList x'
    | otherwise
    = fail "Hyper OP only works on lists"
    where
    doHyper x
	| VRef x' <- x
	= doHyper =<< readRef x'
	| otherwise
	= enterEvalContext cxtItemAny $ App (Val $ VCode sub) [Val x] []
    hyperList []     = return []
    hyperList (x:xs) = do
        val  <- doHyper x
        rest <- hyperList xs
        return (val:rest)

op1HyperPostfix :: VCode -> Val -> Eval Val
op1HyperPostfix = op1HyperPrefix

op2Hyper :: VCode -> Val -> Val -> Eval Val
op2Hyper sub (VRef ref) y = do
    x <- readRef ref
    op2Hyper sub x y
op2Hyper sub x (VRef ref) = do
    y <- readRef ref
    op2Hyper sub x y
op2Hyper sub x y
    | VList x' <- x, VList y' <- y
    = fmap VList $ hyperLists x' y'
    | VList x' <- x
    = fmap VList $ mapM ((flip doHyper) y) x'
    | VList y' <- y
    = fmap VList $ mapM (doHyper x) y'
    | otherwise
    = fail "Hyper OP only works on lists"
    where
    doHyper x y = enterEvalContext cxtItemAny $ App (Val $ VCode sub) [Val x, Val y] []
    hyperLists [] [] = return []
    hyperLists xs [] = return xs
    hyperLists [] ys = return ys
    hyperLists (x:xs) (y:ys) = do
        val  <- doHyper x y
        rest <- hyperLists xs ys
        return (val:rest)

op1Range :: Val -> Val
op1Range (VStr s)    = VList $ map VStr $ strRangeInf s
op1Range (VRat n)    = VList $ map VRat [n ..]
op1Range (VNum n)    = VList $ map VNum [n ..]
op1Range x           = VList $ map VInt [vCast x ..]

op2Range :: Val -> Val -> Val
op2Range (VStr s) y  = VList $ map VStr $ strRange s (vCast y)
op2Range (VNum n) y  = VList $ map VNum [n .. vCast y]
op2Range x (VNum n)  = VList $ map VNum [vCast x .. n]
op2Range (VRat n) y  = VList $ map VRat [n .. vCast y]
op2Range x (VRat n)  = VList $ map VRat [vCast x .. n]
op2Range x y         = VList $ map VInt [vCast x .. vCast y]

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
op2Identity x y = return $ VBool (x == y)

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

-- |Returns a transaction to install a primitive operator using
-- 'Pugs.AST.genMultiSym'.
-- The associativity determines the arity and fixity of ops.
-- The primitive\'s subBody is defined in 'op0', 'op1', etc depending on arity,
-- the default is 'op0'.
-- The Pad symbol name is prefixed with \"&*\" for functions and
-- \"&*\" ~ fixity ~ \":\" for operators.
primOp :: String -> String -> Params -> String -> STM (Pad -> Pad)
primOp sym assoc prms ret = genMultiSym name sub
    where
    name | isAlpha (head sym)
         , fixity == "prefix"
         = "&*" ++ sym
         | otherwise
         = "&*" ++ fixity ++ (':':sym)
    sub  = codeRef $ mkPrim
        { subName     = sym
        , subType     = SubPrim
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
primDecl str = primOp sym assoc params ret
    where
    (ret:assoc:sym:prms) = words str
    takeWord = takeWhile isWord . dropWhile (not . isWord)
    isWord = not . (`elem` "(),:")
    prms'  = map takeWord prms
    prms'' = foldr foldParam [] prms'
    params = map (\p -> p{ isWritable = isLValue p }) prms''

doFoldParam :: String -> String -> [Param] -> [Param]
doFoldParam cxt [] []       = [(buildParam cxt "" "$?1" (Val VUndef)) { isLValue = False }]
doFoldParam cxt [] (p:ps)   = ((buildParam cxt "" (strInc $ paramName p) (Val VUndef)) { isLValue = False }:p:ps)
doFoldParam cxt (s:name) ps = ((buildParam cxt [s] name (Val VUndef)) { isLValue = False } : ps)

foldParam :: String -> Params -> Params
foldParam "Named" = \ps -> (
    (buildParam "Hash" "*" "@?0" (Val VUndef)):
    (buildParam "Hash" "*" "%?0" (Val VUndef)):ps)
foldParam "List"    = doFoldParam "List" "*@?1"
foldParam ('r':'w':'!':"List") = \ps -> ((buildParam "List" "" "@?0" (Val VUndef)) { isLValue = True }:ps)
foldParam ('r':'w':'!':str) = \ps -> ((buildParam str "" "$?1" (Val VUndef)) { isLValue = True }:ps)
foldParam ""        = id
foldParam ('?':str)
    | ('r':'w':'!':typ) <- str
    = \ps -> ((buildParam typ "?" "$?1" (Val VUndef)) { isLValue = True }:ps)
    | (('r':'w':'!':typ), "=$_") <- break (== '=') str
    = \ps -> ((buildParam typ "?" "$?1" (Var "$_")) { isLValue = True }:ps)
    | (typ, "=$_") <- break (== '=') str
    = \ps -> ((buildParam typ "?" "$?1" (Var "$_")) { isLValue = False }:ps)
    | (typ, ('=':def)) <- break (== '=') str
    = let readVal "Num" = Val . VNum . read
          readVal "Int" = Val . VInt . read
          readVal "Str" = Val . VStr . read
          readVal x     = error $ "Unknown type: " ++ x
      in \ps -> ((buildParam typ "?" "$?1" (readVal typ def)) { isLValue = False }:ps)
    | otherwise
    = \ps -> (buildParam str "?" "$?1" (Val VUndef):ps)
foldParam ('~':str) = \ps -> (((buildParam str "" "$?1" (Val VUndef)) { isLValue = False }) { isLazy = True }:ps)
foldParam x         = doFoldParam x []

-- op1 "perl"
prettyVal :: Int -> Val -> Eval VStr
prettyVal 10 _ = return "..."
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
    return $ "(" ++ concat (intersperse ", " vs') ++ ")"
prettyVal _ v = return $ pretty v

-- XXX -- Junctive Types -- XXX --

-- spre is "symbolic pre", that is, operators for which a precedence has
-- already been assigned in Parser.hs

-- |Initial set global symbols to populate the evaluation environment
--  in the form of Pad mutating transactions built with 'primDecl'.
--
--  The source string format is:
--
-- >  ret_val   assoc   op_name args
initSyms :: STM [Pad -> Pad]
initSyms = mapM primDecl . filter (not . null) . lines $ decodeUTF8 "\
\\n   Bool      spre    !       (Bool)\
\\n   Num       spre    +       (Num)\
\\n   Num       pre     abs     (?Num=$_)\
\\n   Num       pre     atan    (Num)\
\\n   Num       pre     atan    (Num, Num)\
\\n   Num       pre     cos     (?Num=$_)\
\\n   Num       pre     sin     (?Num=$_)\
\\n   Num       pre     tan     (?Num=$_)\
\\n   Any       pre     pi      ()\
\\n   Num       pre     exp     (?Num=$_, ?Num)\
\\n   Num       pre     sqrt    (?Num=$_)\
\\n   Bool      spre    -r      (?Str=$_)\
\\n   Bool      spre    -w      (?Str=$_)\
\\n   Bool      spre    -x      (?Str=$_)\
\\n   Bool      spre    -e      (?Str=$_)\
\\n   Bool      spre    -z      (?Str=$_)\
\\n   Int       spre    -s      (?Str=$_)\
\\n   Bool      spre    -f      (?Str=$_)\
\\n   Bool      spre    -d      (?Str=$_)\
\\n   Num       spre    -       (Num)\
\\n   Str       spre    ~       (Str)\
\\n   Bool      spre    ?       (Bool)\
\\n   Str       spre    =       (?IO)\
\\n   List      spre    =       (?IO)\
\\n   Str       pre     readline (?IO)\
\\n   List      pre     readline (?IO)\
\\n   Int       pre     int     (?Int=$_)\
\\n   List      pre     list    (List)\
\\n   Hash      pre     hash    (List)\
\\n   List      pre     pair    (List)\
\\n   Scalar    pre     scalar  (Scalar)\
\\n   Any       pre     reverse (rw!Any)\
\\n   Any       pre     reverse (List)\
\\n   Int       spre    +^      (Int)\
\\n   Int       spre    ~^      (Str)\
\\n   Bool      spre    ?^      (Bool)\
\\n   Ref       spre    \\      (rw!Any)\
\\n   List      post    ...     (Str)\
\\n   List      post    ...     (Scalar)\
\\n   Any       pre     undef   (?rw!Any)\
\\n   Str       pre     chop    (?rw!Str=$_)\
\\n   Str       pre     chomp   (?rw!Str=$_)\
\\n   Int       pre     index   (Str, Str, ?Int=0)\
\\n   Int       pre     rindex  (Str, Str, ?Int)\
\\n   Int       pre     substr  (rw!Str, Int, ?Int, ?Str)\
\\n   Str       pre     lc      (?Str=$_)\
\\n   Str       pre     lcfirst (?Str=$_)\
\\n   Str       pre     uc      (?Str=$_)\
\\n   Str       pre     ucfirst (?Str=$_)\
\\n   Str       pre     capitalize (?Str=$_)\
\\n   Any       post    ++      (rw!Num)\
\\n   Num       post    --      (rw!Num)\
\\n   Any       spre    ++      (rw!Num)\
\\n   Num       spre    --      (rw!Num)\
\\n   Bool      pre     not     (List)\
\\n   Bool      pre     true    (Bool)\
\\n   List      pre     gather  (Code)\
\\n   List      pre     map     (Code, List)\
\\n   List      pre     grep    (Code, List)\
\\n   List      pre     sort    (Code, List)\
\\n   List      pre     reduce  (Code, List)\
\\n   List      pre     sort    (Array)\
\\n   List      pre     map     (Array: Code)\
\\n   List      pre     grep    (Array: Code)\
\\n   List      pre     sort    (Array: Code)\
\\n   List      pre     reduce  (Array: Code)\
\\n   Any       pre     splice  (rw!Array, ?Int=0)\
\\n   Any       pre     splice  (rw!Array, Int, Int)\
\\n   Any       pre     splice  (rw!Array, Int, Int, List)\
\\n   Int       pre     push    (rw!Array, List)\
\\n   Int       pre     unshift (rw!Array, List)\
\\n   Scalar    pre     pop     (rw!Array)\
\\n   Scalar    pre     shift   (rw!Array)\
\\n   Scalar    pre     sum     (List)\
\\n   Scalar    pre     min     (List)\
\\n   Scalar    pre     max     (List)\
\\n   List      pre     uniq    (List)\
\\n   Str       pre     join    (Array: Str)\
\\n   Str       pre     join    (Str, List)\
\\n   Any       pre     join    (Thread)\
\\n   Bool      pre     detach  (Thread)\
\\n   List      pre     zip     (List)\
\\n   List      pre     keys    (rw!Hash)\
\\n   List      pre     values  (rw!Hash)\
\\n   List      pre     kv      (rw!Hash)\
\\n   List      pre     pairs   (rw!Hash)\
\\n   List      pre     keys    (rw!Array)\
\\n   List      pre     values  (rw!Array)\
\\n   List      pre     kv      (rw!Array)\
\\n   List      pre     pairs   (rw!Array)\
\\n   Scalar    pre     delete  (rw!Hash: List)\
\\n   Scalar    pre     delete  (rw!Array: List)\
\\n   Bool      pre     exists  (rw!Hash: Str)\
\\n   Bool      pre     exists  (rw!Array: Int)\
\\n   Str       pre     perl    (rw!Any|Junction)\
\\n   Any       pre     try     (Code)\
\\n   Any       pre     lazy    (Code)\
\\n   Any       pre     eval    (Str)\
\\n   Any       pre     eval_parrot   (Str)\
\\n   Any       pre     eval_perl5   (Str)\
\\n   Any       pre     eval_haskell (Str)\
\\n   Any       pre     eval_yaml    (Str)\
\\n   Any       pre     require (?Str=$_)\
\\n   Any       pre     use     (?Str=$_)\
\\n   Any       pre     require_haskell (Str)\
\\n   Any       pre     require_parrot  (Str)\
\\n   Any       pre     require_perl5   (Str)\
\\n   Any       pre     last    (?Int=1)\
\\n   Any       pre     next    (?Int=1)\
\\n   Any       pre     redo    (?Int=1)\
\\n   Any       pre     exit    (?Int=0)\
\\n   Num       pre     rand    (?Num=1)\
\\n   Bool      pre     defined (Any)\
\\n   Str       pre     ref     (rw!Any|Junction)\
\\n   Str       pre     isa     (rw!Any|Junction, Str)\
\\n   Num       pre     time    ()\
\\n   List      pre     times   ()\
\\n   Str       pre     want    ()\
\\n   Str       pre     File::Spec::cwd  ()\
\\n   Str       pre     File::Spec::tmpdir  ()\
\\n   Bool      pre     print   (IO)\
\\n   Bool      pre     print   (IO: List)\
\\n   Bool      pre     print   ()\
\\n   Bool      pre     print   (List)\
\\n   Str       pre     sprintf (Str, List)\
\\n   Bool      pre     say     (IO)\
\\n   Bool      pre     say     (IO: List)\
\\n   Bool      pre     say     ()\
\\n   Bool      pre     say     (List)\
\\n   Bool      pre     close   (IO)\
\\n   Bool      pre     flush   (IO)\
\\n   Bool      pre     close   (Socket)\
\\n   Bool      pre     die     (List)\
\\n   Bool      pre     warn    (List)\
\\n   Bool      pre     fail_   (List)\
\\n   IO        pre     open    (Str)\
\\n   Socket    pre     listen  (Int)\
\\n   Socket    pre     connect (Str, Int)\
\\n   Any       pre     accept  (Any)\
\\n   List      pre     slurp   (?Str=$_)\
\\n   List      pre     slurp   (Handle)\
\\n   List      pre     readdir (Str)\
\\n   Bool      pre     exec    (Str: List)\
\\n   Int       pre     system  (Str)\
\\n   Int       pre     system  (Str: List)\
\\n   Bool      pre     binmode (IO: ?Int=1)\
\\n   Void      pre     return  ()\
\\n   Void      pre     return  (rw!Any)\
\\n   Void      pre     return  (List)\
\\n   Void      pre     yield   ()\
\\n   Void      pre     yield   (rw!Any)\
\\n   Void      pre     yield   (List)\
\\n   Void      pre     take    ()\
\\n   Void      pre     take    (rw!Any)\
\\n   Void      pre     take    (List)\
\\n   Junction  pre     any     (List)\
\\n   Junction  pre     all     (List)\
\\n   Junction  pre     one     (List)\
\\n   Junction  pre     none    (List)\
\\n   Bool      pre     sleep   (Int)\
\\n   Bool      pre     rmdir   (?Str=$_)\
\\n   Bool      pre     mkdir   (Str)\
\\n   Bool      pre     chdir   (Str)\
\\n   Int       pre     elems   (Array)\
\\n   Int       pre     end     (Array)\
\\n   Int       pre     graphs  (?Str=$_)\
\\n   Int       pre     codes   (?Str=$_)\
\\n   Int       pre     chars   (?Str=$_)\
\\n   Int       pre     bytes   (?Str=$_)\
\\n   Int       pre     chmod   (Int, List)\
\\n   Scalar    pre     key     (rw!Pair)\
\\n   Scalar    pre     value   (rw!Pair)\
\\n   List      pre     keys    (rw!Pair)\
\\n   List      pre     values  (Pair|Junction)\
\\n   List      pre     kv      (rw!Pair)\
\\n   List      pre     pairs   (rw!Pair)\
\\n   Any       pre     pick    (Any|Junction)\
\\n   Bool      pre     rename  (Str, Str)\
\\n   Bool      pre     symlink (Str, Str)\
\\n   Bool      pre     link    (Str, Str)\
\\n   Int       pre     unlink  (List)\
\\n   Str       pre     readlink (Str)\
\\n   List      pre     split   (Str, Str)\
\\n   Str       spre    =       (IO)\
\\n   List      spre    =       (IO)\
\\n   Junction  list    |       (Any|Junction)\
\\n   Junction  list    &       (Any|Junction)\
\\n   Junction  list    ^       (Any|Junction)\
\\n   Num       left    *       (Num, Num)\
\\n   Num       left    /       (Num, Num)\
\\n   Num       left    %       (Num, Num)\
\\n   Str       left    x       (Str, Int)\
\\n   List      left    xx      (Any, Int)\
\\n   Int       left    +&      (Int, Int)\
\\n   Int       left    +<      (Int, Int)\
\\n   Int       left    +>      (Int, Int)\
\\n   Str       left    ~&      (Str, Str)\
\\n   Str       left    ~<      (Str, Str)\
\\n   Str       left    ~>      (Str, Str)\
\\n   Num       right   **      (Num, Num)\
\\n   Num       left    +       (Num, Num)\
\\n   Num       left    -       (Num, Num)\
\\n   Str       left    ~       (Str, Str)\
\\n   Int       left    +|      (Int, Int)\
\\n   Int       left    +^      (Int, Int)\
\\n   Str       left    ~|      (Str, Str)\
\\n   Str       left    ~^      (Str, Str)\
\\n   Str       left    ?|      (Str, Str)\
\\n   Pair      right   =>      (Any, Any)\
\\n   Int       non     cmp     (Str, Str)\
\\n   Int       non     <=>     (Num, Num)\
\\n   List      non     ..      (Scalar, Scalar)\
\\n   Bool      chain   !=      (Num, Num)\
\\n   Bool      chain   ==      (Num, Num)\
\\n   Bool      chain   =:=     (rw!Any, rw!Any)\
\\n   Bool      chain   ~~      (rw!Any, Any)\
\\n   Bool      chain   !~      (Any, Any)\
\\n   Bool      chain   <       (Num, Num)\
\\n   Bool      chain   <=      (Num, Num)\
\\n   Bool      chain   >       (Num, Num)\
\\n   Bool      chain   >=      (Num, Num)\
\\n   Bool      chain   ne      (Str, Str)\
\\n   Bool      chain   eq      (Str, Str)\
\\n   Bool      chain   lt      (Str, Str)\
\\n   Bool      chain   le      (Str, Str)\
\\n   Bool      chain   gt      (Str, Str)\
\\n   Bool      chain   ge      (Str, Str)\
\\n   Scalar    left    &&      (Bool, ~Bool)\
\\n   Scalar    left    !!      (Bool, ~Bool)\
\\n   Scalar    left    ||      (Bool, ~Bool)\
\\n   Scalar    left    ^^      (Bool, Bool)\
\\n   Scalar    left    //      (Bool, ~Bool)\
\\n   Scalar    left    .[]     (Array, Int)\
\\n   Scalar    left    .{}     (Hash, Str)\
\\n   List      list    ¥               (Array)\
\\n   List      list    Y               (Array)\
\\n   List      spre    <==     (List)\
\\n   List      left    ==>     (List, Code)\
\\n   Scalar    left    and     (Bool, ~Bool)\
\\n   Scalar    left    or      (Bool, ~Bool)\
\\n   Scalar    left    nor     (Bool, ~Bool)\
\\n   Scalar    left    xor     (Bool, Bool)\
\\n   Scalar    left    err     (Bool, ~Bool)\
\\n   Str       pre     chr     (?Int=$_)\
\\n   Int       pre     ord     (?Str=$_)\
\\n   Str       pre     hex     (?Str=$_)\
\\n   Int       pre     from    (Match)\
\\n   Int       pre     to      (Match)\
\\n   List      pre     matches (Match)\
\\n   Str       pre     hex     (Int)\
\\n   Num       pre     log     (Int)\
\\n   Num       pre     log     (Num)\
\\n   Num       pre     log10   (Num)\
\\n   Thread    pre     async   (Code)\
\\n   Int       pre     sign    (Num)\
\\n   Bool      pre     kill    (Thread)\
\\n   Int       pre     kill    (Int, List)\
\\n   Object    pre     new     (Object: Named)\
\\n   Object    pre     BUILDALL (Object)\
\\n   Object    pre     clone   (Any)\
\\n   Object    pre     id      (Any)\
\\n   Str       pre     name    (Code)\
\\n   Int       pre     arity   (Code)\
\\n   Bool      pre     Thread::yield   (Thread)\
\\n   List      pre     Pugs::Internals::runInteractiveCommand    (?Str=$_)\
\\n   List      pre     Pugs::Internals::openFile    (?Str,?Str=$_)\
\\n   Bool      pre     bool::true ()\
\\n   Bool      pre     bool::false ()\
\\n   List      spre    prefix:[,] (List)\
\\n"
