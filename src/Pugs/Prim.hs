{-# OPTIONS_GHC -fglasgow-exts #-}
{-# OPTIONS_GHC -#include "UnicodeC.h" #-}

{-
    Primitive operators.

    There hammer on the anvil smote,
    There chisel clove, and graver wrote;
    There forged was blade, and bound was hilt;
    The delver mined, the mason built...
-}

module Pugs.Prim where
import Pugs.Internals
import Pugs.Junc
import Pugs.AST
import Pugs.Types
import Pugs.Pretty
import Pugs.Parser
import Pugs.External
import Text.Printf
import qualified Data.Set as Set
import qualified Pugs.Types.Array  as Array
import qualified Pugs.Types.Hash   as Hash
import qualified Pugs.Types.Scalar as Scalar
import qualified Pugs.Types.Pair   as Pair

op0 :: Ident -> [Val] -> Eval Val
op0 "!"  = return . opJuncNone
op0 "&"  = return . opJuncAll
op0 "^"  = return . opJuncOne
op0 "|"  = return . opJuncAny
op0 "want"  = const $ return . VStr =<< asks envWant
op0 "time"  = const $ do
    clkt <- liftIO getClockTime
    return $ VInt $ toInteger $ tdSec $ diffClockTimes clkt epochClkT
    where
    epochClkT = toClockTime epoch
    epoch = CalendarTime 2000 January 1 0 0 0 0 Saturday 0 "UTC" 0 False
op0 "not" = const retEmpty
op0 "so" = const (return $ VBool True)
op0 "¥" = (return . VList . concat . op0Zip =<<) . mapM fromVal
op0 "Y" = op0 "¥"
op0 "File::Spec::cwd" = const $ do
    mycwd <- liftIO getCurrentDirectory
    return $ VStr mycwd
op0 "pi" = const $ return . VNum $ pi
op0 "say" = const $ op1 "say" =<< readVar "$_"
op0 "print" = const $ op1 "print" =<< readVar "$_"
op0 other = \x -> return $ VError ("unimplemented listOp: " ++ other) (App other (map Val x) [])

retEmpty :: ContT Val (ReaderT Env IO) Val
retEmpty = do
    ifListContext
        (return $ VList [])
        (return VUndef)

op0Zip :: [[Val]] -> [[Val]]
op0Zip lists | all null lists = []
op0Zip lists = (map zipFirst lists):(op0Zip (map zipRest lists))
    where
    zipFirst []     = undef
    zipFirst (x:_)  = x
    zipRest  []     = []
    zipRest  (_:xs) = xs

op1 :: Ident -> Val -> Eval Val
op1 "!"    = op1Cast (VBool . not)
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
op1 "post:++" = \x -> do
    val <- fromVal x
    ref <- fromVal x
    val' <- case val of
        (VStr str)  -> return . VStr $ strInc str
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
            evl <- asks envEval
            sorted <- (`sortByM` valList) $ \v1 v2 -> do
                rv  <- local (\e -> e{ envContext = cxtItem "Int" }) $ do
                    evl (Syn "()" [Val sub, Syn "invs" [Val v1, Val v2], Syn "args" []])
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
                    return . VStr $ reverse str)
                (do ref     <- fromVal v
                    vals    <- readRef ref
                    vlist   <- fromVal vals
                    return . VList $ reverse vlist)
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
op1 "\\"   = return . VRef . scalarRef -- XXX
op1 "post:..."  = op1Cast op1Range
op1 "not"  = op1 "!"
op1 "true" = op1 "?"
op1 "any"  = op1Cast opJuncAny
op1 "all"  = op1Cast opJuncAll
op1 "one"  = op1Cast opJuncOne
op1 "none" = op1Cast opJuncNone
op1 "perl" = (return . VStr =<<) . (prettyVal 0)
op1 "require_haskell" = \v -> do
    name    <- fromVal v
    externRequire "Haskell" name
    return $ VBool True
op1 "require" = \v -> do
    file    <- fromVal v
    incs    <- fromVal =<< readVar "@*INC"
    requireInc incs file (errMsg file incs)
    where
    errMsg file incs = "Can't locate " ++ file ++ " in @INC (@INC contains: " ++ unwords incs ++ ")."
    requireInc [] _ msg = do
        return $ VError msg (Val VUndef)
    requireInc (p:ps) file msg = do
        let pathName = p ++ "/" ++ file
        ok <- liftIO $ doesFileExist pathName
        if (not ok)
            then requireInc ps file msg
            else do
                str <- liftIO $ readFile pathName
                opEval True pathName (decodeUTF8 str)
op1 "eval" = \v -> do
    str <- fromVal v
    opEval False "<eval>" str
op1 "eval_perl5" = boolIO evalPerl5
op1 "eval_haskell" = op1EvalHaskell
op1 "defined" = op1Cast (VBool . defined)
op1 "last" = \v -> return (VError "cannot last() outside a loop" (Val v))
op1 "next" = \v -> return (VError "cannot next() outside a loop" (Val v))
op1 "redo" = \v -> return (VError "cannot redo() outside a loop" (Val v))
op1 "return" = \v -> return (VError "cannot return() outside a subroutine" (Val v))
op1 "sign" = \v -> if defined v
    then op1Cast (VInt . signum) v
    else return undef

-- Side-effectful function: how far into Monadic IO shall we go?
op1 "rand"  = \v -> do
    x    <- fromVal v
    rand <- liftIO $ randomRIO (0, if x == 0 then 1 else x)
    return $ VNum rand
op1 "print" = op1Print hPutStr
op1 "say" = op1Print hPutStrLn
op1 "die" = \v -> do
    strs <- fromVal v
    retError (concat strs) (Val v)
op1 "exit" = \v -> do
    rv <- fromVal v
    if rv /= 0
        then shiftT . const . return . VControl . ControlExit . ExitFailure $ rv
        else shiftT . const . return . VControl . ControlExit $ ExitSuccess
op1 "readlink" = \v -> do
    str  <- fromVal v
    tryIO undef $ return . VStr =<< readSymbolicLink str
op1 "sleep" = boolIO (threadDelay . (* 1000000))
op1 "mkdir" = boolIO createDirectory
op1 "rmdir" = boolIO removeDirectory
op1 "chdir" = boolIO setCurrentDirectory
op1 "-r"    = fileTestIO fileTestIsReadable
op1 "-w"    = fileTestIO fileTestIsWritable
op1 "-x"    = fileTestIO fileTestIsExecutable
op1 "-e"    = fileTestIO fileTestExists
op1 "-z"    = fileTestIO fileTestSizeIsZero
op1 "-s"    = fileTestIO fileTestFileSize
op1 "-f"    = fileTestIO fileTestIsFile
op1 "-d"    = fileTestIO fileTestIsDirectory
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
    val         <- fromVal v
    case val of
        (VHandle h) -> do
            ifListContext
                (op1 "=" val)
                (return . VStr =<< (liftIO $ hGetContents h))
        _ -> do
            fileName    <- fromVal val
            ifListContext
                (slurpList fileName)
                (slurpScalar fileName)
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
op1 "system" = boolIO system
op1 "accept" = \v -> do
    socket      <- fromVal v
    (h, _, _)   <- liftIO $ accept socket
    return $ VHandle h
op1 "yield" = const $ do
    ok <- tryIO False $ do { yield ; return True }
    return $ VBool ok
op1 "async" = \v -> do
    env     <- ask
    code    <- fromVal v
    tid     <- liftIO . (if rtsSupportsBoundThreads then forkOS else forkIO) $ do
        (`runReaderT` env) $ (`runContT` return) $ resetT $ do
            evl <- asks envEval
            local (\e -> e{ envContext = CxtVoid }) $ do
                evl (Syn "()" [Val code, Syn "invs" [], Syn "args" []])
        return ()
    return $ VThread tid
op1 "listen" = \v -> do
    port    <- fromVal v
    socket  <- liftIO $ listenOn (PortNumber $ fromInteger port)
    return $ VSocket socket
op1 "flush" = boolIO hFlush
op1 "close" = \v -> do
    val <- fromVal v
    case val of
        (VSocket _) -> boolIO sClose val
        _           -> boolIO hClose val
op1 "key" = (return . fst =<<) . (fromVal :: Val -> Eval VPair)
op1 "value" = (return . snd =<<) . (fromVal :: Val -> Eval VPair)
op1 "pairs" = \v -> do
    pairs <- op1Pairs v
    return $ VList pairs
op1 "kv" = \v -> do
    pairs <- op1Pairs v
    kvs   <- forM pairs $ \(VRef ref) -> do
        pair   <- readRef ref
        fromVal pair
    return . VList $ concat kvs 
op1 "keys" = op1Keys
op1 "values" = op1Values
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
        (return . VStr . (++ "\n") =<< hGetLine fh)
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
        rv <- tryIO Nothing (return . Just =<< openFile x ReadMode)
        case rv of
            Nothing  -> retError "No such file or directory" (Val $ VStr x)
            Just hdl -> return hdl
    handleOf (VList [x]) = handleOf x
    handleOf v = fromVal v
op1 "ref"   = (return . VStr . show =<<) . evalValType
op1 "pop"   = \x -> join $ doArray x Array.pop -- monadic join
op1 "shift" = \x -> join $ doArray x Array.shift -- monadic join
op1 "pick"  = op1Pick
op1 "sum"   = op1Sum
op1 "chr"   = op1Cast (VStr . (:[]) . chr)
op1 "ord"   = op1Cast $ \str -> if null str then undef else (castV . ord . head) str
op1 "hex"   = op1Cast (VInt . read . ("0x"++))
op1 "log"   = op1Cast (VNum . log)
op1 "log10" = op1Cast (VNum . logBase 10)
op1 other   = return . (\x -> VError ("unimplemented unaryOp: " ++ other) (App other [Val x] []))

op1EvalHaskell :: Val -> Eval Val
op1EvalHaskell cv = do
    cstr <- (fromVal cv) :: Eval String
    ret <- liftIO (evalHaskell cstr)
    glob <- askGlobal
    errSV <- findSymRef "$!" glob
    case ret of
        Right str -> do
            writeRef errSV VUndef
            return $ VStr str
        Left  err -> do
            writeRef errSV (VStr err)
            retEmpty

op1Cast :: (Value n) => (n -> Val) -> Val -> Eval Val
op1Cast f val = return . f =<< fromVal =<< fromVal' val

op2Cast :: (Value n, Value m) => (n -> m -> Val) -> Val -> Val -> Eval Val
op2Cast f x y = do
    x' <- fromVal =<< fromVal' x
    y' <- fromVal =<< fromVal' y
    return (f x' y')

op1Pairs :: Val -> Eval [Val]
op1Pairs v = do
    ref  <- fromVal v
    vals <- pairsFromRef ref
    return vals

op1Keys :: Val -> Eval Val
op1Keys v = do
    ref  <- fromVal v
    vals <- keysFromRef ref
    return $ VList vals

op1Values :: Val -> Eval Val
op1Values (VJunc j) = return . VList . Set.elems $ juncSet j
op1Values v = do
    ref  <- fromVal v
    vals <- valuesFromRef ref
    return $ VList vals

op1StrFirst f = op1Cast $ VStr .
    \str -> case str of
        []      -> []
        (c:cs)  -> (f c:cs)

op1Pick :: Val -> Eval Val
op1Pick (VRef r) = op1Pick =<< readRef r
op1Pick (VJunc (Junc JAny _ set)) = do -- pick mainly works on 'any'
    rand <- liftIO $ randomRIO (0 :: Int, (Set.cardinality set) - 1)
    return $ (Set.elems set) !! rand
op1Pick (VJunc (Junc _ _ set)) =
    if (Set.cardinality $ set) > 1 then return VUndef
    else return $ head $ Set.elems set
op1Pick v = return $ VError "pick not defined" (Val v)

op1Sum list = do
    vals <- fromVal list
    foldM (op2 "+") undef vals

op1Print :: (Handle -> String -> IO ()) -> Val -> Eval Val
op1Print f v@(VHandle _) = do
    def <- readVar "$_"
    op1Print f (VList [v, def])
op1Print f v = do
    vals <- case v of
        VList _   -> fromVal v
--      VArray _  -> fromVal v
        _         -> return [v]
    let (handle, vs) = case vals of
                        (VHandle h:vs)  -> (h, vs)
                        _               -> (stdout, vals)
    vs' <- mapM fromVal vs
    tryIO undef $ do
        f handle . concatMap encodeUTF8 $ vs'
        return $ VBool True

bool2n v = if v
  then 1
  else 0

doBoolIO f v = do
    x <- fromVal v
    tryIO False $ do
        f x
        return True

boolIO f v = do
    ok <- doBoolIO f v
    return $ VBool ok

boolIO2 f u v = do
    x <- fromVal u
    y <- fromVal v
    tryIO (VBool False) $ do
        f x y
        return (VBool True)

boolIO3 f v = do
    x <- fromVal v
    ok <- tryIO False $ f x
    return $ VBool ok

opEval :: Bool -> String -> String -> Eval Val
opEval fatal name str = do
    env <- ask
    let env' = runRule env id ruleProgram name str
    val <- resetT $ local (const env') $ do
        evl <- asks envEval
        evl (envBody env')
    retEvalResult fatal val

retEvalResult fatal val = do
    glob <- askGlobal
    errSV <- findSymRef "$!" glob
    case val of
        VError _ (Val errval) | not fatal  -> do
            writeRef errSV errval
            retEmpty
        VError _ _ | not fatal  -> do
            writeRef errSV (VStr $ show val)
            retEmpty
        _ -> do
            writeRef errSV VUndef
            return val

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


op2 :: Ident -> Val -> Val -> Eval Val
op2 op | "»" `isPrefixOf` op = op2Hyper . init . init . drop 2 $ op
op2 op | ">>" `isPrefixOf` op = op2Hyper . init . init . drop 2 $ op
op2 "rename" = boolIO2 rename
op2 "symlink" = boolIO2 createSymbolicLink
op2 "link" = boolIO2 createLink
op2 "*"  = op2Numeric (*)
op2 "/"  = op2Divide
op2 "%"  = op2Int mod
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
op2 "&&" = op2Logical not
op2 "||" = op2Logical id
op2 "^^" = \x y -> do
    let xor True True   = VBool False
        xor True False  = x
        xor False True  = y
        xor False False = VBool False
    op2Cast xor x y
op2 "//" = op2Logical defined
op2 "!!" = \x y -> callCC $ \esc -> do
    bx <- fromVal x
    when bx $ esc (VBool False)
    by <- fromVal y
    when by $ esc (VBool False)
    return (VBool True)
-- XXX pipe forward XXX
op2 "and"= op2 "&&"
op2 "or" = op2 "||"
op2 "xor"= op2 "^^"
op2 "err"= op2 "//"
op2 "nor"= op2 "!!"
op2 "grep" = op2Grep
op2 "map"  = op2Map
op2 "join" = op2Join
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
    return . VBool =<< existsFromRef ref y
op2 "unshift" = op2Array Array.unshift
op2 "push" = op2Array Array.push
op2 "split"= \x y -> do
    val <- fromVal x
    str <- fromVal y
    case val of
        VRule rx -> do
            chunks <- rxSplit rx (encodeUTF8 str)
            return $ VList $ map (VStr . decodeUTF8) chunks
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
op2 "chmod" = \x y -> do
    mode  <- fromVal x
    files <- fromVals y
    rets  <- mapM (doBoolIO . flip setFileMode $ toEnum mode) files
    return . VInt . sum $ map bool2n rets
op2 "splice" = \x y -> do
    fetchSize   <- doArray x Array.fetchSize
    len         <- fromVal y
    sz          <- fetchSize
    op4 "splice" x y (castV (sz - (len `mod` sz))) (VList []) 
op2 "sort" = \x y -> do
    xs <- fromVals x
    ys <- fromVals y
    op1 "sort" . VList $ xs ++ ys
op2 other = \x y -> return $ VError ("unimplemented binaryOp: " ++ other) (App other [Val x, Val y] [])

-- XXX - need to generalise this
op2Match x (VRef y) = do
    y' <- readRef y
    op2Match x y'

op2Match x (VSubst (rx@MkRule{ rxGlobal = True }, subst)) = do
    str     <- fromVal x
    rv      <- doReplace (encodeUTF8 str) Nothing
    case rv of
        (str', Just _) -> do
            ref     <- fromVal x
            writeRef ref (VStr $ decodeUTF8 str')
            return $ VBool True
        _ -> return $ VBool False
    where
    doReplace :: String -> Maybe [String] -> Eval (String, Maybe [String])
    doReplace str subs = do
        case str =~~ rxRegex rx of
            Nothing -> return (str, subs)
            Just mr -> do
                glob    <- askGlobal
                let subs = elems $ mrSubs mr
                matchAV <- findSymRef "$/" glob
                writeRef matchAV $ VList $ map (VStr . decodeUTF8) subs
                str'    <- fromVal =<< evalExp subst
                (after', rv) <- doReplace (mrAfter mr) (Just subs)
                let subs' = fromMaybe subs rv
                return (concat [mrBefore mr, encodeUTF8 str', after'], Just subs')

op2Match x (VSubst (rx@MkRule{ rxGlobal = False }, subst)) = do
    str     <- fromVal x
    ref     <- fromVal x
    case encodeUTF8 str =~~ rxRegex rx of
        Nothing -> return $ VBool False
        Just mr -> do
            glob <- askGlobal
            let subs = elems $ mrSubs mr
            matchAV <- findSymRef "$/" glob
            writeRef matchAV $ VList $ map (VStr . decodeUTF8) subs
            str' <- fromVal =<< evalExp subst
            writeRef ref $
                (VStr $ decodeUTF8 $ concat [mrBefore mr, encodeUTF8 str', mrAfter mr])
            return $ VBool True

op2Match x (VRule rx@MkRule{ rxGlobal = True }) = do
    str     <- fromVal x
    rv      <- doMatch (encodeUTF8 str)
    ifListContext
        (return . VList $ map (VStr . decodeUTF8) rv)
        (return . VInt $ genericLength rv)
    where
    doMatch str = do
        case str =~~ rxRegex rx of
            Nothing -> return []
            Just mr -> do
                rest <- doMatch $ mrAfter mr
                return $ (tail $ elems (mrSubs mr)) ++ rest

op2Match x (VRule rx@MkRule{ rxGlobal = False }) = do
    str     <- fromVal x
    case encodeUTF8 str =~~ rxRegex rx of
        Nothing -> return $ VBool False
        Just mr -> do
            --- XXX: Fix $/ and make it lexical.
            glob <- askGlobal
            let subs = elems $ mrSubs mr
            matchAV <- findSymRef "$/" glob
            writeRef matchAV $ VList $ map (VStr . decodeUTF8) subs
            return $ VBool True

op2Match x y = op2Cmp vCastStr (==) x y

rxSplit :: VRule -> String -> Eval [String]
rxSplit _  [] = return []
rxSplit rx str = do
    case str =~~ rxRegex rx of
        Nothing -> return [str]
        Just mr | null $ mrMatch mr -> do
            let (c:cs) = str
            rest <- rxSplit rx (cs)
            return ([c]:rest)
        Just mr -> do
            rest <- rxSplit rx (mrAfter mr)
            return $ (mrBefore mr:mrSubList mr) ++ rest

op3 :: Ident -> Val -> Val -> Val -> Eval Val
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
op3 other = \x y z -> return $ VError ("unimplemented 3-ary op: " ++ other) (App other [Val x, Val y, Val z] [])

op4 :: Ident -> Val -> Val -> Val -> Val -> Eval Val
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
    splice  <- doArray x Array.splice
    start   <- fromVal y
    count   <- fromVal z
    vals    <- fromVals w
    vals'   <- splice start count vals
    ifListContext
        (return $ VList vals')
        (return $ last (undef:vals'))

op4 other = \x y z w -> return $ VError ("unimplemented 4-ary op: " ++ other) (App other [Val x, Val y, Val z, Val w] [])

op2Hyper op x y
    | VList x' <- x, VList y' <- y
    = hyperLists x' y' >>= (return . VList)
    | VList x' <- x
    = mapM ((flip (op2 op)) y) x' >>= (return . VList)
    | VList y' <- y
    = mapM (op2 op x) y' >>= (return . VList)
    | otherwise
    = return $ VError "Hyper OP only works on lists" (Val VUndef)
    where
    hyperLists [] [] = return []
    hyperLists xs [] = return xs
    hyperLists [] ys = return ys
    hyperLists (x:xs) (y:ys) = do
        val  <- op2 op x y
        rest <- hyperLists xs ys
        return (val:rest)

op2Array :: (forall a. Array.Class a => a -> [Val] -> Eval ()) -> Val -> Val -> Eval Val
op2Array f x y = do
    f    <- doArray x f
    vals <- fromVal y
    f vals
    size <- doArray x Array.fetchSize
    idx  <- size
    return $ castV idx

op2Grep sub@(VCode _) list = op2Grep list sub
op2Grep list sub = do
    args <- fromVal list
    vals <- (`filterM` args) $ \x -> do
        evl <- asks envEval
        rv  <- local (\e -> e{ envContext = cxtItem "Bool" }) $ do
            evl (Syn "()" [Val sub, Syn "invs" [Val x], Syn "args" []])
        fromVal rv
    return $ VList vals

op2Map sub@(VCode _) list = op2Map list sub
op2Map list sub = do
    args <- fromVal list
    vals <- (`mapM` args) $ \x -> do
        evl <- asks envEval
        rv  <- local (\e -> e{ envContext = cxtSlurpyAny }) $ do
            evl (Syn "()" [Val sub, Syn "invs" [Val x], Syn "args" []])
        fromVal rv
    return $ VList $ concat vals

op2Join x y = do
    (strVal, listVal) <- ifValTypeIsa x "Scalar"
        (return (x, y))
        (return (y, x))
    str     <- fromVal strVal
    ref     <- fromVal listVal
    list    <- readRef ref
    strList <- fromVals list
    return . VStr . concat . intersperse str $ strList

vCastStr :: Val -> Eval VStr
vCastStr = fromVal
vCastRat :: Val -> Eval VRat
vCastRat = fromVal

op2Str f x y = do
    x' <- fromVal x
    y' <- fromVal y
    return $ VStr $ f x' y'

op2Num  f = op2Cast $ (VNum .) . f
op2Bool f = op2Cast $ (VBool .) . f
op2Int  f = op2Cast $ (VInt .) . f
op2Rat  f = op2Cast $ (VRat .) . f

op2Exp x y = do
    num2 <- fromVal =<< fromVal' y
    case reverse $ show (num2 :: VNum) of
        ('0':'.':_) -> do
            num1 <- fromVal =<< fromVal' x
            if isDigit . head $ show (num1 :: VNum)
                then op2Rat ((^^) :: VRat -> VInt -> VRat) x y
                else op2Num ((**) :: VNum -> VNum -> VNum) x y
        _ -> op2Num ((**) :: VNum -> VNum -> VNum) x y

op1Range (VStr s)    = VList $ map VStr $ strRangeInf s
op1Range (VRat n)    = VList $ map VRat [n ..]
op1Range (VNum n)    = VList $ map VNum [n ..]
op1Range x           = VList $ map VInt [vCast x ..]

op2Range (VStr s) y  = VList $ map VStr $ strRange s (vCast y)
op2Range (VNum n) y  = VList $ map VNum [n .. vCast y]
op2Range x (VNum n)  = VList $ map VNum [vCast x .. n]
op2Range (VRat n) y  = VList $ map VRat [n .. vCast y]
op2Range x (VRat n)  = VList $ map VRat [vCast x .. n]
op2Range x y         = VList $ map VInt [vCast x .. vCast y]

op2Divide x y
    | VInt x' <- x, VInt y' <- y
    = return $ if y' == 0 then err else VRat $ x' % y'
    | VInt x' <- x, VRat y' <- y
    = return $ if y' == 0 then err else VRat $ (x' % 1) / y'
    | VRat x' <- x, VInt y' <- y
    = return $ if y' == 0 then err else VRat $ x' / (y' % 1)
    | VRat x' <- x, VRat y' <- y
    = return $ if y' == 0 then err else VRat $ x' / y'
    | otherwise
    = op2Num (/) x y
    where
    err = VError ("Illegal division by zero: " ++ "/") (App "/" [] [Val x, Val y])

op2ChainedList x y
    | VList xs <- x, VList ys <- y  = VList $ xs ++ ys
    | VList xs <- x                 = VList $ xs ++ [y]
    | VList ys <- y                 = VList (x:ys)
    | otherwise                     = VList [x, y]

op2Logical f x y = do
    vx   <- fromVal x
    bool <- fromVal vx
    if f bool
        then return vx
        else fromVal y

op2DefinedOr = undefined

op2Cmp f cmp x y = do
    x' <- f x
    y' <- f y
    return $ VBool $ x' `cmp` y'

op2Ord f x y = do
    x' <- f x
    y' <- f y
    return $ VInt $ case x' `compare` y' of
        LT -> -1
        EQ -> 0
        GT -> 1

op1Floating :: (Double -> Double) -> Val -> Eval Val
op1Floating f v = do
    foo <- fromVal v
    return $ VNum $ f foo

op1Numeric :: (forall a. (Num a) => a -> a) -> Val -> Eval Val
op1Numeric f VUndef     = return . VInt $ f 0
op1Numeric f (VInt x)   = return . VInt $ f x
op1Numeric f l@(VList _)= return . VInt . f =<< fromVal l
op1Numeric f (VRat x)   = return . VRat $ f x
op1Numeric f (VRef x)   = op1Numeric f =<< readRef x
op1Numeric f x          = return . VNum . f =<< fromVal x

--- XXX wrong: try num first, then int, then vcast to Rat (I think)
op2Numeric :: (forall a. (Num a) => a -> a -> a) -> Val -> Val -> Eval Val
op2Numeric f x y
    | VUndef <- x = op2Numeric f (VInt 0) y
    | VUndef <- y = op2Numeric f x (VInt 0)
    | (VInt x', VInt y') <- (x, y)  = return $ VInt $ f x' y'
    | (VRat x', VInt y') <- (x, y)  = return $ VRat $ f x' (y' % 1)
    | (VInt x', VRat y') <- (x, y)  = return $ VRat $ f (x' % 1) y'
    | (VRat x', VRat y') <- (x, y)  = return $ VRat $ f x' y'
    | otherwise = do
        x' <- fromVal x
        y' <- fromVal y
        return . VNum $ f x' y'

primOp :: String -> String -> Params -> String -> IO Symbol
primOp sym assoc prms ret = return . MkSym name =<< newIORef sub
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
        , subFun      = (Prim f)
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

primDecl str = primOp sym assoc params ret
    where
    (ret:assoc:sym:prms) = words str
    takeWord = takeWhile isWord . dropWhile (not . isWord)
    isWord = not . (`elem` "(),:")
    prms'  = map takeWord prms
    prms'' = foldr foldParam [] prms'
    params = map (\p -> p{ isWritable = True }) prms''

doFoldParam cxt [] []       = [buildParam cxt "" "$?1" (Val VUndef)]
doFoldParam cxt [] (p:ps)   = (buildParam cxt "" (strInc $ paramName p) (Val VUndef):p:ps)
doFoldParam cxt (s:name) ps = (buildParam cxt [s] name (Val VUndef) : ps)

foldParam :: String -> Params -> Params
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
    = \ps -> (buildParam typ "?" "$?1" (Var "$_"):ps)
    | (typ, ('=':def)) <- break (== '=') str
    = let readVal "Num" = Val . VNum . read
          readVal "Int" = Val . VInt . read
          readVal "Str" = Val . VStr . read
          readVal x     = error $ "Unknown type: " ++ x
      in \ps -> (buildParam typ "?" "$?1" (readVal typ def):ps)
    | otherwise
    = \ps -> (buildParam str "?" "$?1" (Val VUndef):ps)
foldParam ('~':str) = \ps -> ((buildParam str "" "$?1" (Val VUndef)) { isThunk = True }:ps)
foldParam x         = doFoldParam x []

-- filetest operators --

-- Officially, these should return a stat object, which sometimes pretends
-- to be a boolean, and may(?) return the filename in string context.
-- DARCS was working on stat, and we should perhaps grab their work:
--  http://www.abridgegame.org/pipermail/darcs-users/2005-February/005499.html
-- They currently (2004-04-05) seem to be using:
--  http://abridgegame.org/cgi-bin/darcs.cgi/darcs/win32/System/Posix.hs
-- For the moment, these return filename and false or undef.
-- Known Bugs: multiple stat()s are done, and filename isnt a boolean.

fileTestIO :: (Value n) => (n -> IO Val) -> Val -> Eval Val
fileTestIO f v = do
    str <- fromVal =<< fromVal' v
    tryIO undef $ f str

fileTestIsReadable f = do
    p <- getPermissions f
    let b = readable p
    return $ if b then castV f else VBool False

fileTestIsWritable f = do
    p <- getPermissions f
    let b = writable p
    return $ if b then castV f else VBool False

fileTestIsExecutable f = do
    p <- getPermissions f
    let b = executable p || searchable p
    return $ if b then castV f else VBool False

fileTestExists f = do
    b1 <- doesFileExist f
    b2 <- doesDirectoryExist f
    return $ if b1 || b2 then castV f else VBool False

fileTestIsFile f = do
    b <- doesFileExist f
    return $ if b then castV f else VBool False

fileTestIsDirectory f = do
    b <- doesDirectoryExist f
    return $ if b then castV f else VBool False

fileTestFileSize f = do
    n <- statFileSize f
    return $ VInt n

fileTestSizeIsZero f = do
    n <- statFileSize f
    return $ if n == 0 then VBool True else VBool False

-- XXX These bulks of code below screams for refactoring

pairsFromRef :: VRef -> Eval [Val]
pairsFromRef r@(MkRef (IPair _)) = do
    return [VRef r]
pairsFromRef (MkRef (IHash hv)) = do
    pairs   <- Hash.fetch hv
    return $ map (\(k, v) -> castV (castV k, v)) pairs
pairsFromRef (MkRef (IArray av)) = do
    vals    <- Array.fetch av
    return $ map castV ((map VInt [0..]) `zip` vals)
pairsFromRef (MkRef (IScalar sv)) = do
    refVal  <- Scalar.fetch sv
    op1Pairs refVal
pairsFromRef ref = retError "Not a keyed reference" (Val $ VRef ref)

keysFromRef :: VRef -> Eval [Val]
keysFromRef (MkRef (IPair pv)) = do
    key     <- Pair.fetchKey pv
    return [key]
keysFromRef (MkRef (IHash hv)) = do
    keys    <- Hash.fetchKeys hv
    return $ map castV keys
keysFromRef (MkRef (IArray av)) = do
    keys    <- Array.fetchKeys av
    return $ map castV keys
keysFromRef (MkRef (IScalar sv)) = do
    refVal  <- Scalar.fetch sv
    if defined refVal
        then fromVal =<< op1Keys refVal
        else return []
keysFromRef ref = retError "Not a keyed reference" (Val $ VRef ref)

valuesFromRef :: VRef -> Eval [Val]
valuesFromRef (MkRef (IPair pv)) = do
    val   <- Pair.fetchVal pv
    return [val]
valuesFromRef (MkRef (IHash hv)) = do
    pairs <- Hash.fetch hv
    return $ map snd pairs
valuesFromRef (MkRef (IArray av)) = Array.fetch av
valuesFromRef (MkRef (IScalar sv)) = do
    refVal  <- Scalar.fetch sv
    if defined refVal
        then fromVal =<< op1Values refVal
        else return []
valuesFromRef ref = retError "Not a keyed reference" (Val $ VRef ref)

existsFromRef :: VRef -> Val -> Eval VBool
existsFromRef (MkRef (IHash hv)) val = do
    idx     <- fromVal val
    Hash.existsElem hv idx
existsFromRef (MkRef (IArray av)) val = do
    idx     <- fromVal val
    Array.existsElem av idx
existsFromRef (MkRef (IScalar sv)) val = do
    refVal  <- Scalar.fetch sv
    ref     <- fromVal refVal
    existsFromRef ref val
existsFromRef ref _ = retError "Not a keyed reference" (Val $ VRef ref)

deleteFromRef :: VRef -> Val -> Eval Val
deleteFromRef (MkRef (IHash hv)) val = do
    idxs    <- fromVals val
    rv      <- forM idxs $ \idx -> do
        val <- Hash.fetchVal hv idx
        Hash.deleteElem hv idx
        return val
    return $ VList rv
deleteFromRef (MkRef (IArray av)) val = do
    idxs    <- fromVals val
    rv      <- forM idxs $ \idx -> do
        val <- Array.fetchVal av idx
        Array.deleteElem av idx
        return val
    return $ VList rv
deleteFromRef (MkRef (IScalar sv)) val = do
    refVal  <- Scalar.fetch sv
    ref     <- fromVal refVal
    deleteFromRef ref val
deleteFromRef ref _ = retError "Not a keyed reference" (Val $ VRef ref)

prettyVal :: Int -> Val -> Eval VStr
prettyVal 10 _ = return "..."
prettyVal d (VRef r) = do
    v'  <- readRef r
    str <- prettyVal (d+1) v'
    return ('\\':str)
{-
prettyVal d (VPair (k, v)) = do
    k'  <- prettyVal (d+1) k
    v'  <- prettyVal (d+1) v
    return $ concat ["(", k', " => ", v', ")"]
-}
prettyVal d (VList vs) = do
    vs' <- mapM (prettyVal (d+1)) vs
    return $ "(" ++ concat (intersperse ", " vs') ++ ")"
prettyVal _ v = return $ pretty v

sortByM :: (Val -> Val -> Eval Bool) -> [Val] -> Eval [Val]
sortByM _ []  = return []
sortByM _ [x] = return [x]
sortByM f xs  = do
    let (as, bs) = splitAt (length xs `quot` 2) xs
    aSorted <- sortByM f as
    bSorted <- sortByM f bs
    doMerge f aSorted bSorted
    where
    doMerge :: (Val -> Val -> Eval Bool) -> [Val] -> [Val] -> Eval [Val]
    doMerge _ [] ys = return ys
    doMerge _ xs [] = return xs
    doMerge f (x:xs) (y:ys) = do
        isLessOrEqual <- f x y
        if isLessOrEqual
            then do
                rest <- doMerge f xs (y:ys)
                return (x:rest)
            else do
                rest <- doMerge f (x:xs) ys
                return (y:rest)

-- XXX -- Junctive Types -- XXX --

-- spre is "symbolic pre", that is, operators for which a precedence has
-- already been assigned in Parser.hs

--    ret_val   assoc   op_name args
initSyms = mapM primDecl . filter (not . null) . lines $ decodeUTF8 "\
\\n   Bool      spre    !       (Bool)\
\\n   Num       spre    +       (Num)\
\\n   Num       pre     abs     (?Num=$_)\
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
\\n   Any       post    ++      (rw!Num)\
\\n   Num       post    --      (rw!Num)\
\\n   Any       spre    ++      (rw!Num)\
\\n   Num       spre    --      (rw!Num)\
\\n   Any       pre     not     ()\
\\n   Bool      pre     not     (Bool)\
\\n   Bool      pre     true    (Bool)\
\\n   List      pre     map     (Code, List)\
\\n   List      pre     grep    (Code, List)\
\\n   List      pre     sort    (Code, List)\
\\n   List      pre     sort    (Array)\
\\n   List      pre     map     (Array: Code)\
\\n   List      pre     grep    (Array: Code)\
\\n   List      pre     sort    (Array: Code)\
\\n   Any       pre     splice  (rw!Array, ?Int=0)\
\\n   Any       pre     splice  (rw!Array, Int, Int)\
\\n   Any       pre     splice  (rw!Array, Int, Int, List)\
\\n   Int       pre     push    (rw!Array, List)\
\\n   Int       pre     unshift (rw!Array, List)\
\\n   Scalar    pre     pop     (rw!Array)\
\\n   Scalar    pre     shift   (rw!Array)\
\\n   Num       pre     sum     (List)\
\\n   Str       pre     join    (Array: Str)\
\\n   Str       pre     join    (Str, List)\
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
\\n   Str       pre     perl    (rw!Any)\
\\n   Any       pre     eval    (Str)\
\\n   Any       pre     eval_perl5   (Str)\
\\n   Any       pre     eval_haskell (Str)\
\\n   Any       pre     require (?Str=$_)\
\\n   Any       pre     require_haskell (Str)\
\\n   Any       pre     last    (?Int=1)\
\\n   Any       pre     next    (?Int=1)\
\\n   Any       pre     redo    (?Int=1)\
\\n   Any       pre     exit    (?Int=0)\
\\n   Num       pre     rand    (?Num=1)\
\\n   Bool      pre     defined (Any)\
\\n   Str       pre     ref     (rw!Any)\
\\n   Str       pre     isa     (rw!Any, Str)\
\\n   Num       pre     time    ()\
\\n   Str       pre     want    ()\
\\n   Str       pre     File::Spec::cwd  ()\
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
\\n   Any       pre     do      (Str)\
\\n   IO        pre     open    (Str)\
\\n   Socket    pre     listen  (Int)\
\\n   Socket    pre     connect (Str, Int)\
\\n   Any       pre     accept  (Any)\
\\n   List      pre     slurp   (?Str=$_)\
\\n   List      pre     slurp   (Handle)\
\\n   List      pre     readdir (Str)\
\\n   Bool      pre     system  (Str)\
\\n   Bool      pre     system  (Str: List)\
\\n   Bool      pre     binmode (IO: ?Int=1)\
\\n   Void      pre     return  (List)\
\\n   Junction  pre     any     (List)\
\\n   Junction  pre     all     (List)\
\\n   Junction  pre     one     (List)\
\\n   Junction  pre     none    (List)\
\\n   Bool      pre     sleep   (Int)\
\\n   Bool      pre     rmdir   (?Str=$_)\
\\n   Bool      pre     mkdir   (Str)\
\\n   Bool      pre     chdir   (Str)\
\\n   Int       pre     elems   (Array)\
\\n   Int       pre     graphs  (?Str=$_)\
\\n   Int       pre     codes   (?Str=$_)\
\\n   Int       pre     chars   (?Str=$_)\
\\n   Int       pre     bytes   (?Str=$_)\
\\n   Int       pre     chmod   (Int, List)\
\\n   Scalar    pre     key     (rw!Pair)\
\\n   Scalar    pre     value   (rw!Pair)\
\\n   List      pre     keys    (rw!Pair)\
\\n   List      pre     values  (rw!Pair)\
\\n   List      pre     kv      (rw!Pair)\
\\n   List      pre     pairs   (rw!Pair)\
\\n   List      pre     values  (rw!Junction)\
\\n   Any       pre     pick    (rw!Junction)\
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
\\n   Junction  list    ^       (Any)\
\\n   Junction  list    !       (Any|Junction)\
\\n   Num       left    *       (Num, Num)\
\\n   Num       left    /       (Num, Num)\
\\n   Num       left    %       (Num, Num)\
\\n   Str       left    x       (Str, Int)\
\\n   Any       left    is      (Any, Any)\
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
\\n   Pair      non     =>      (Any, Any)\
\\n   Int       non     cmp     (Str, Str)\
\\n   Int       non     <=>     (Num, Num)\
\\n   List      non     ..      (Scalar, Scalar)\
\\n   Bool      chain   !=      (Num, Num)\
\\n   Bool      chain   ==      (Num, Num)\
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
\\n   List      left    »+«     (Any, Any)\
\\n   List      left    »*«     (Any, Any)\
\\n   List      left    »/«     (Any, Any)\
\\n   List      left    »x«     (Any, Any)\
\\n   List      left    »xx«    (Any, Any)\
\\n   List      left    >>+<<   (Any, Any)\
\\n   List      left    >>*<<   (Any, Any)\
\\n   List      left    >>/<<   (Any, Any)\
\\n   List      left    >>x<<   (Any, Any)\
\\n   List      left    >>xx<<  (Any, Any)\
\\n   List      left    >>~<<   (Any, Any)\
\\n   List      list    ,       (List)\
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
\\n   Str       pre     hex     (Int)\
\\n   Num       pre     log     (Int)\
\\n   Num       pre     log     (Num)\
\\n   Num       pre     log10   (Num)\
\\n   Any       list    ;       (Any)\
\\n   Thread    pre     async   (Code)\
\\n   Bool      pre     yield   (?Thread)\
\\n   Int       pre     sign    (Num)\
\\n   Int       pre     kill    (Int, List)\
\\n"
