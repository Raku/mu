{-# OPTIONS_GHC -fglasgow-exts #-}

{-
    Primitive operators.

    There hammer on the anvil smote,
    There chisel clove, and graver wrote;
    There forged was blade, and bound was hilt;
    The delver mined, the mason built...
-}

module Prim where
import Internals
import Junc
import AST
import Pretty
import Parser
import External
import qualified Data.Map as Map
import qualified Data.Set as Set

op0 :: Ident -> [Val] -> Eval Val
-- op0 ","  = return . VList . concatMap vCast
op0 "!"  = return . VJunc . Junc JNone Set.empty . Set.fromList
op0 "&"  = return . opJuncAll
op0 "^"  = return . opJuncOne
op0 "|"  = return . opJuncAny
op0 "undef" = const $ return VUndef
op0 "time"  = const $ do
    clkt <- liftIO getClockTime
    return $ VInt $ toInteger $ tdSec $ diffClockTimes clkt epochClkT
    where
    epochClkT = toClockTime epoch
    epoch = CalendarTime 2000 January 1 0 0 0 0 Saturday 0 "UTC" 0 False
op0 "not" = const retEmpty
op0 "so" = const (return $ VBool True)
op0 "¥" = (>>= return . VList . concat . transpose) . mapM fromVal
op0 "Y" = op0 "¥"
op0 "File::Spec::cwd" = const $ do
    mycwd <- liftIO getCurrentDirectory
    return $ VStr mycwd
op0 other = \x -> return $ VError ("unimplemented listOp: " ++ other) (App other (map Val x) [])

retEmpty :: ContT Val (ReaderT Env IO) Val
retEmpty = do
    cxt <- asks envContext
    return $ case cxt of
        "List"  -> VList []
        "Array" -> VArray (MkArray [])
        "Hash"  -> VHash (MkHash Map.empty)
        _       -> VUndef

op1 :: Ident -> Val -> Eval Val
op1 "!"    = return . fmapVal not
op1 "chop" = \mv -> do
    val <- liftIO $ readIORef (vCast mv)
    case vCast val of
        ""  -> return VUndef
        str -> do
            liftIO $ writeIORef (vCast mv) $ VStr (init str)
            return $ VStr [last str]
op1 "chomp" = \mv -> do
    val <- liftIO $ readIORef (vCast mv)
    case vCast val of
        str@(_:_) | last str == '\n' -> do
            liftIO $ writeIORef (vCast mv) $ VStr (init str)
            return $ VStr [last str]
        _   -> return VUndef
op1 "lc" = return . VStr . (map toLower) . vCast
op1 "lcfirst" = return . VStr .
                (\x -> case x of { (a:as) -> toLower a : as ; a -> a}) . vCast
op1 "uc" = return . VStr . (map toUpper) . vCast
op1 "ucfirst" = return . VStr .
                (\x -> case x of { (a:as) -> toUpper a : as ; a -> a}) . vCast
op1 "undef" = \mv -> do
    liftIO $ writeIORef (vCast mv) $ VUndef
    return VUndef
op1 "+"    = return . op1Numeric id
op1 "abs"  = return . op1Numeric abs
op1 "cos"  = op1Floating cos
op1 "sin"  = op1Floating sin
op1 "sqrt" = op1Floating sqrt
op1 "post:++" = \mv -> do
    val <- readMVal mv
    ref <- fromVal mv
    liftIO $ writeIORef ref $ case val of
        (VStr str)  -> VStr $ strInc str
        _           -> op1Numeric (+1) (vCast val)
    case val of
        (VStr _)    -> return val
        _           -> op1 "+" val
op1 "++"   = \mv -> do
    op1 "post:++" mv
    readMVal mv
op1 "post:--"   = \mv -> do
    val <- liftIO $ readIORef (vCast mv)
    liftIO $ writeIORef (vCast mv) $
        op1Numeric (\x -> x - 1) (vCast val)
    return val
op1 "--"   = \mv -> do
    op1 "post:--" mv
    readMVal mv
op1 "-"    = return . op1Numeric negate
op1 "scalar" = return -- XXX refify?
op1 "sort" = return . VList . sortBy cmp . vCast
    where
    cmp x y = compare (vCast x :: VStr) (vCast y :: VStr)
op1 "reverse" = \v ->
    ifContextIsa "List"
        (return . VList . reverse . vCast $ v)
        (return . VStr . reverse . vCast $ v)
op1 "list" = return . VList . vCast
op1 "~"    = (>>= (return . VStr)) . (>>= fromVal) . readMVal
op1 "?"    = return . VBool . vCast
op1 "int"  = return . VInt . vCast
op1 "+^"   = return . VInt . (toInteger . (complement :: Word -> Word)) . vCast
op1 "~^"   = return . VStr . mapStr complement . vCast
op1 "?^"   = op1 "!"
op1 "\\"   = return . VRef
op1 "post:..."  = return . op1Range
op1 "not"  = op1 "!"
op1 "true" = op1 "?"
op1 "any"  = return . opJuncAny . vCast
op1 "all"  = return . opJuncAll . vCast
op1 "one"  = return . opJuncOne . vCast
op1 "none" = return . VJunc . Junc JNone Set.empty . Set.fromList . vCast
op1 "perl" = return . VStr . (pretty :: Val -> VStr)
op1 "require_haskell" = \v -> do
    name    <- fromVal v
    externRequire "Haskell" name
    return $ VBool True
op1 "require" = \v -> do
    fileVal <- readMVal v
    -- XXX - assuming a flat array
    incVals <- readVar "@*INC"
    let incs = map vCast $ vCast incVals
        file = vCast fileVal
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
                opEval True pathName str
op1 "eval" = opEval False "<eval>" . vCast
op1 "eval_perl5" = boolIO evalPerl5
op1 "defined" = \v -> do
    v <- readMVal v
    return . VBool $ case v of
        VUndef  -> False
        _       -> True
op1 "last" = \v -> return (VError "cannot last() outside a loop" (Val v))
op1 "next" = \v -> return (VError "cannot next() outside a loop" (Val v))
op1 "redo" = \v -> return (VError "cannot redo() outside a loop" (Val v))
op1 "return" = \v -> return (VError "cannot return() outside a subroutine" (Val v))
op1 "sign" = \v -> return $ case vCast v of
    Nothing -> VUndef
    _       -> VInt . signum . vCast $ v

-- Side-effectful function: how far into Monadic IO shall we go?
op1 "rand"  = \v -> do
    let x = vCast v
    rand <- liftIO $ randomRIO (0, if x == 0 then 1 else x)
    return $ VNum rand
op1 "print" = op1Print hPutStr
op1 "say" = op1Print hPutStrLn
op1 "die" = \v -> do
    return $ VError (concatMap vCast . vCast $ v) (Val v)
op1 "exit" = \v -> do
    if vCast v
        then liftIO $ exitWith (ExitFailure $ vCast v)
        else liftIO $ exitWith ExitSuccess
-- handle timely destruction
op1 "readlink" = \v -> do
    file <- liftIO $ catch (readSymbolicLink (vCast v)) (const $ return "")
    if file == ""
      then return VUndef
      else return $ VStr file
op1 "sleep" = boolIO sleep
op1 "mkdir" = boolIO createDirectory
op1 "rmdir" = boolIO removeDirectory
op1 "chdir" = boolIO setCurrentDirectory
op1 "-d"    = boolIO3 doesDirectoryExist
op1 "-f"    = boolIO3 doesFileExist
op1 "elems" = return . VInt . (genericLength :: VList -> VInt) . vCast
op1 "chars" = return . VInt . (genericLength :: String -> VInt) . vCast
op1 "bytes" = return . VInt . (genericLength :: String -> VInt) . encodeUTF8 . vCast
op1 "chmod" = \v -> do
    v <- readMVal v
    vals <- mapM readMVal (vCast v)
    rets <- liftIO $ mapM (doBoolIO $ flip setFileMode $ intCast $ head vals) $ map vCast $ tail vals
    return $ VInt $ sum $ map bool2n rets
op1 "unlink" = \v -> do
    v <- readMVal v
    vals <- mapM readMVal (vCast v)
    rets <- liftIO $ sequence $ map ( doBoolIO removeFile ) $ map vCast vals
    return $ VInt $ sum $ map bool2n rets
op1 "slurp" = \v -> do
    val         <- fromVal v
    case val of
        (VHandle h) -> do
            ifContextIsa "List"
                (op1 "=" val)
                (return . VStr =<< (liftIO $ hGetContents h))
        _ -> do
            fileName    <- fromVal val
            ifContextIsa "List"
                (slurpList fileName)
                (slurpScalar fileName)
    where
    slurpList file = op1 "=" (VList [VStr file])
    slurpScalar file = tryIO VUndef $ do
        content <- readFile file
        return $ VStr content
op1 "open" = \v -> do
    let (mode, filename) = span (`elem` "+<> ") (vCast v)
    liftIO $ (`catch` (const $ return VUndef)) $ do
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
            local (\e -> e{ envContext = "Void" }) $ do
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
op1 "key" = return . fst . (vCast :: Val -> VPair)
op1 "value" = return . snd . (vCast :: Val -> VPair)
op1 "kv" = return . VList . concatMap (\(k, v) -> [k, v]) . vCast
op1 "keys" = return . VList . map fst . (vCast :: Val -> [VPair])
op1 "values" = return . op1Values
op1 "readline" = op1 "="
op1 "=" = \v -> do
    fh  <- handleOf v
    ifContextIsa "List"
        (return . VList . map (VStr . (++ "\n")) . lines =<< liftIO (hGetContents fh))
        (return . VStr . (++ "\n") =<< liftIO (hGetLine fh))
    where
    handleOf (VRef x) = handleOf x
    handleOf (VPair (_, x)) = handleOf x
    handleOf (VList [VStr x]) = liftIO $ openFile x ReadMode
    handleOf (VList []) = do
        args    <- readVar "@*ARGS"
        files   <- fromVal args
        if null files
            then return stdin
            else handleOf (VList [VStr $ vCast (head files)]) -- XXX wrong
    handleOf v = fromVal v
op1 "ref"  = return . VStr . valType
op1 "pop"  = op1Pop (last, init)
op1 "shift"= op1Pop (head, tail)
op1 "pick" = op1Pick
op1 "chr"  = return . op1Chr
op1 "ord"  = return . op1Ord
op1 "hex"  = return . op1Hex
op1 "log"  = return . op1Log
op1 "log10" = return . op1Log10
op1 other  = return . (\x -> VError ("unimplemented unaryOp: " ++ other) (App other [Val x] []))


op1Values :: Val -> Val
op1Values (VJunc j) = VList $ Set.elems $ juncSet j
op1Values (VPair (_, v)) = VList $ [v] -- lwall: a pair is a really small hash.
op1Values v@(VHash _) = VList $ map snd $ (vCast :: Val -> [VPair]) v
op1Values v@(VList _) = VList $ map snd $ (vCast :: Val -> [VPair]) v -- hope it's a list of pairs
op1Values (VRef v) = op1Values v
op1Values v = VError "values not defined" (Val v)

-- what's less cheesy than read?
op1Hex          :: Val -> Val
op1Hex (VStr s) = VInt (read ("0x" ++ s))
op1Hex (VInt i) = VInt (read ("0x" ++ show i))  -- PerlJam asks "auto int to str coercion?"
op1Hex v        = VError "hex not defined" (Val v)

op1Ord              :: Val -> Val
op1Ord (VStr (s:_)) = VInt (fromIntegral (ord s))
op1Ord v            =  VError "ord not defined" (Val v)

op1Chr          :: Val -> Val
op1Chr (VInt i) = VStr [(chr . fromIntegral) i]
op1Chr v        = VError "chr not defined" (Val v)

op1Log                :: Val -> Val
op1Log v@(VNum _)     = VNum (log (vCast v))
op1Log v@(VInt _)     = VNum (log (vCast v))
op1Log v@(VRat _)     = VNum (log (vCast v))
op1Log v@(VComplex _) = VNum (log (vCast v))
-- op1Log v              = VNum (log (vCast v)) -- if vCast will work for every type
op1Log v              = VError "log not defined" (Val v)

op1Log10                :: Val -> Val
op1Log10 v@(VNum _)     = VNum (logBase 10 (vCast v))
op1Log10 v@(VInt _)     = VNum (logBase 10 (vCast v))
op1Log10 v@(VRat _)     = VNum (logBase 10 (vCast v))
op1Log10 v@(VComplex _) = VNum (logBase 10 (vCast v))
-- op1Log10 v              = VNum (logBase 10 (vCast v)) -- if vCast will work for every type
op1Log10 v              = VError "log10 not defined" (Val v)


op1Pick :: Val -> Eval Val
op1Pick (VJunc (Junc JAny _ set)) = do -- pick mainly works on 'any'
    rand <- liftIO $ randomRIO (0 :: Int, (Set.cardinality set) - 1)
    return $ (Set.elems set) !! rand
op1Pick (VJunc (Junc _ _ set)) =
    if (Set.cardinality $ set) > 1 then return VUndef
    else return $ head $ Set.elems set
op1Pick (VRef v) = op1Pick v
op1Pick v = return $ VError "pick not defined" (Val v)

op1Pop (fPick, fRem) list = do
    let array = vCast list
    old <- readMVal array
    let oldList = vCast old
    if null oldList
        then return VUndef
        else do
            liftIO $ writeIORef (vCast array) $ VList $ fRem oldList
            return $ fPick oldList

op1Print :: (Handle -> String -> IO ()) -> Val -> Eval Val
op1Print f v = do
    val <- readMVal v
    vals <- mapM readMVal (vCast val)
    let (handle, vs) = case vals of
                        (VHandle h:vs)  -> (h, vs)
                        _               -> (stdout, vals)
    liftIO . f handle . concatMap (encodeUTF8 . vCast) $ vs
    return $ VBool True

bool2n v = if v
  then 1
  else 0

tryIO :: (MonadIO m) => a -> IO a -> m a
tryIO err = liftIO . (`catch` (const $ return err))

doBoolIO f v = do
    ok <- tryIO False $ do
        f (vCast v)
        return True
    return ok

boolIO f v = do
    ok <- doBoolIO f v
    return $ VBool ok

boolIO2 f u v = do
    ok <- liftIO $ (`catch` (const $ return False)) $ do
        f (vCast u) (vCast v)
        return True
    return $ VBool ok

boolIO3 f v = do
    ok <- tryIO False $ do
        f (vCast v)
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
    let Just errSV = findSym "$!" glob
    case val of
        VError _ (Val errval) | not fatal  -> do
            writeMVal errSV errval
            retEmpty
        VError _ _ | not fatal  -> do
            writeMVal errSV (VStr $ show val)
            retEmpty
        _ -> do
            writeMVal errSV VUndef
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
op2 "x"  = \x y -> return $ VStr (concat $ (vCast y :: VInt) `genericReplicate` (vCast x :: VStr))
op2 "xx" = \x y -> return $ VList $ concatMap vCast ((vCast y :: VInt) `genericReplicate` x)
op2 "+&" = op2Int (.&.)
op2 "+<" = op2Int shiftL
op2 "+>" = op2Int shiftR
op2 "~&" = op2Str $ mapStr2 (.&.)
op2 "~<" = \x y -> return $ VStr $ mapStr (`shiftL` vCast y) (vCast x)
op2 "~>" = \x y -> return $ VStr $ mapStr (`shiftR` vCast y) (vCast x)
op2 "**" = op2Rat ((^^) :: VRat -> VInt -> VRat)
op2 "+"  = op2Numeric (+)
op2 "-"  = op2Numeric (-)
op2 "atan" = \x y -> return . VNum $ atan2 (vCast x) (vCast y)
op2 "~"  = op2Str (++)
op2 "+|" = op2Int (.|.)
op2 "+^" = op2Int xor
op2 "~|" = op2Str $ mapStr2Fill (.|.)
op2 "?|" = op2Bool (||)
op2 "~^" = op2Str $ mapStr2Fill xor
op2 "=>" = \x y -> return $ VPair (x, y)
op2 "cmp"= op2Ord vCastStr
op2 "<=>"= op2Ord vCastRat
op2 ".." = op2Range
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
op2 "^^" = op2Bool ((/=) :: Bool -> Bool -> Bool)
op2 "//" = op2Logical isJust
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
op2 "grep"= op2Grep
op2 "map"= op2Map
op2 "join"= op2Join
op2 "unshift" = op2Push (flip (++))
op2 "push" = op2Push (++)
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
op2 "exp" = \x y -> return . VNum $ case (vCast y) of
    Nothing -> exp (vCast x)
    _       -> vCast y ** vCast x
op2 other = \x y -> return $ VError ("unimplemented binaryOp: " ++ other) (App other [Val x, Val y] [])

-- XXX - need to generalise this
op2Match x y@(MVal _) = do
    y' <- fromVal y
    op2Match x y'

op2Match x (VSubst (rx@MkRule{ rxGlobal = True }, subst)) = do
    str     <- fromVal x
    rv      <- doReplace (encodeUTF8 str) Nothing
    case rv of
        (str', Just _) -> do
            writeMVal (vCast x) (VStr $ decodeUTF8 $ str')
            return $ VBool True
        _ -> return $ VBool False
    where
    doReplace :: String -> Maybe [String] -> Eval (String, Maybe [String])
    doReplace str subs = do
        case str =~~ rxRegex rx of
            Nothing -> return (str, subs)
            Just mr -> do
                glob    <- askGlobal
                let Just matchAV = findSym "$/" glob
                    subs = elems $ mrSubs mr
                writeMVal matchAV $ VList $ map VStr subs
                str'    <- fromVal =<< evalExp subst
                (after', rv) <- doReplace (mrAfter mr) (Just subs)
                let subs' = fromMaybe subs rv
                return (concat [mrBefore mr, str', after'], Just subs')

op2Match x (VSubst (rx@MkRule{ rxGlobal = False }, subst)) = do
    str     <- fromVal x
    case encodeUTF8 str =~~ rxRegex rx of
        Nothing -> return $ VBool False
        Just mr -> do
            glob <- askGlobal
            let Just matchAV = findSym "$/" glob
                subs = elems $ mrSubs mr
            writeMVal matchAV $ VList $ map (VStr . decodeUTF8) subs
            str' <- fromVal =<< evalExp subst
            writeMVal (vCast x) $
                (VStr $ decodeUTF8 $ concat [mrBefore mr, str', mrAfter mr])
            return $ VBool True

op2Match x (VRule rx@MkRule{ rxGlobal = True }) = do
    str     <- fromVal x
    rv      <- doMatch (encodeUTF8 str)
    ifContextIsa "List"
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
            let Just matchAV = findSym "$/" glob
                subs = elems $ mrSubs mr
            writeMVal matchAV $ VList $ map (VStr . decodeUTF8) subs
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
            let f = if null (mrBefore mr) then id else (mrBefore mr:)
            return $ (f $ mrSubList mr) ++ rest

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
    let skip | isJust (vCast z) = length str - (vCast z) - length sub
             | otherwise        = 0
    return . VInt $ doRindex str sub skip
    where
    doRindex :: VStr -> VStr -> Int -> VInt
    doRindex a b skip
        | skip > 0         = doRindex (init a) b (skip-1)
        | b `isSuffixOf` a = toInteger $ length a - length b
        | null a           = -1
        | otherwise        = doRindex (init a) b 0

op3 other = \x y z -> return $ VError ("unimplemented 3-ary op: " ++ other) (App other [Val x, Val y, Val z] [])

op4 :: Ident -> Val -> Val -> Val -> Val -> Eval Val
op4 "substr" = \x y z w -> do
    str <- fromMVal x
    pos <- fromVal y
    let len | isJust (vCast z) = vCast z
            | otherwise        = length str
    let (pre, result, post) = doSubstr str pos len
    when (isJust (vCast w) && result /= VUndef) $
        liftIO $ writeIORef (vCast x) $ VStr $ pre ++ (vCast w) ++ post
    return result
    where
    doSubstr :: VStr -> Int -> Int -> (VStr, Val, VStr)
    doSubstr str pos len
        | abs pos > length str = ("", VUndef, "")
        | pos < 0   = doSubstr str (length str + pos) len
        | len < 0   = doSubstr str pos (length str - pos + len)
        | otherwise = ((take pos str), VStr (take len $ drop pos str), (drop (pos + len) str))

op4 other = \x y z w -> return $ VError ("unimplemented 4-ary op: " ++ other) (App other [Val x, Val y, Val z, Val w] [])

op2Hyper op x y
    | VList x' <- x, VList y' <- y
    = mapM (\(a,b) -> op2 op a b) (x' `zip` y') >>= (return . VList)
    | VList x' <- x
    = mapM ((flip (op2 op)) y) x' >>= (return . VList)
    | VList y' <- y
    = mapM (op2 op x) y' >>= (return . VList)
    | otherwise
    = return $ VError "Hyper OP only works on lsits" (Val VUndef)

op2Push f inv args = do
    let array = vCast inv
        rest = vCast args
    old <- readMVal array
    new <- mapM readMVal rest
    let vals = vCast old `f` concatMap vCast new
    liftIO $ writeIORef (vCast array) $ VList vals
    return $ VInt $ genericLength vals

op2Grep sub@(VSub _) list = op2Grep list sub
op2Grep list sub = do
    vals <- (`filterM` vCast list) $ \x -> do
        evl <- asks envEval
        rv  <- local (\e -> e{ envContext = "Bool" }) $ do
            evl (Syn "()" [Val sub, Syn "invs" [Val x], Syn "args" []])
        return $ vCast rv
    return $ VList vals

op2Map sub@(VSub _) list = op2Map list sub
op2Map list sub = do
    vals <- (`mapM` vCast list) $ \x -> do
        evl <- asks envEval
        rv  <- local (\e -> e{ envContext = "List" }) $ do
            evl (Syn "()" [Val sub, Syn "invs" [Val x], Syn "args" []])
        return $ vCast rv
    return $ VList $ concat vals

op2Join str@(VStr _) list = op2Join list str
op2Join list str = do
    vals <- mapM readMVal (vCast list)
    return $ VStr $ concat $ intersperse (vCast str) $ map vCast vals

vCastStr :: Val -> Eval VStr
vCastStr = fromVal
vCastRat :: Val -> Eval VRat
vCastRat = fromVal

op2Str  f x y = return $ VStr  $ f (vCast x) (vCast y)
op2Num  f x y = return $ VNum  $ f (vCast x) (vCast y)
op2Rat  f x y = return $ VRat  $ f (vCast x) (vCast y)
op2Bool f x y = return $ VBool $ f (vCast x) (vCast y)
op2Int  f x y = do
    x' <- fromVal x
    y' <- fromVal y
    return $ VInt $ f x' y'

op1Range (VStr s)    = VList $ map VStr $ strRangeInf s
op1Range (VRat n)    = VList $ map VRat [n ..]
op1Range (VNum n)    = VList $ map VNum [n ..]
op1Range x           = VList $ map VInt [vCast x ..]

op2Range (VStr s) y  = return $ VList $ map VStr $ strRange s (vCast y)
op2Range (VNum n) y  = return $ VList $ map VNum [n .. vCast y]
op2Range x (VNum n)  = return $ VList $ map VNum [vCast x .. n]
op2Range (VRat n) y  = return $ VList $ map VRat [n .. vCast y]
op2Range x (VRat n)  = return $ VList $ map VRat [vCast x .. n]
op2Range x y         = return $ VList $ map VInt [vCast x .. vCast y]

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
    vx <- fromVal x
    if f (vCast vx)
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

op1Numeric :: (forall a. (Num a) => a -> a) -> Val -> Val
op1Numeric f VUndef     = VInt $ f 0
-- op1Numeric f (MVal x)   = op1Numeric f (castV x)
op1Numeric f (VRef x)   = op1Numeric f x
op1Numeric f (VInt x)   = VInt $ f x
op1Numeric f l@(VList _)= VInt $ f (vCast l)
op1Numeric f (VRat x)   = VRat $ f x
op1Numeric f x          = VNum $ f (vCast x)

--- XXX wrong: try num first, then int, then vcast to Rat (I think)
op2Numeric :: (forall a. (Num a) => a -> a -> a) -> Val -> Val -> Eval Val
op2Numeric f x y
    | (VInt x', VInt y') <- (x, y)  = return $ VInt $ f x' y'
    | (VRat x', VInt y') <- (x, y)  = return $ VRat $ f x' (y' % 1)
    | (VInt x', VRat y') <- (x, y)  = return $ VRat $ f (x' % 1) y'
    | (VRat x', VRat y') <- (x, y)  = return $ VRat $ f x' y'
    | otherwise = do
        x' <- fromVal x
        y' <- fromVal y
        return . VNum $ f x' y'

primOp :: String -> String -> Params -> String -> Symbol Val
primOp sym assoc prms ret = SymVal SOur name sub
    where
    name | isAlpha (head sym)
         , fixity == "prefix"
         = "&*" ++ sym
         | otherwise
         = "&*" ++ fixity ++ (':':sym)
    sub  = VSub $ Sub { isMulti     = True
                      , subName     = sym
                      , subPad      = []
                      , subType     = SubPrim
                      , subAssoc    = assoc
                      , subParams   = prms
                      , subBindings = []
                      , subReturns  = ret
                      , subFun      = (Prim f)
                      }
    symStr = encodeUTF8 sym
    f :: [Val] -> Eval Val
    f    = case (arity :: Integer) of
        0 -> \x -> op0 symStr x
        1 -> \x     -> case x of
            [x]       -> op1 symName x
            [x,y]     -> op2 symStr x y
            [x,y,z]   -> op3 symStr x y z
            [x,y,z,w] -> op4 symStr x y z w
            x         -> op0 symStr x
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

primDecl str = primOp sym assoc (foldr foldParam [] prms) ret
    where
    (ret:assoc:sym:prms') = words str
    takeWord = takeWhile isWord . dropWhile (not . isWord)
    isWord = not . (`elem` "|(),:")
    prms = map takeWord prms'

doFoldParam cxt [] []       = [buildParam cxt "" "$?1" (Val VUndef)]
doFoldParam cxt [] (p:ps)   = (buildParam cxt "" (strInc $ paramName p) (Val VUndef):p:ps)
doFoldParam cxt (s:name) ps = (buildParam cxt [s] name (Val VUndef) : ps)

foldParam :: String -> Params -> Params
foldParam "List"    = doFoldParam "List" "*@?1"
foldParam ('r':'w':'!':"List") = \ps -> ((buildParam "List" "" "@?0" (Val VUndef)) { isLValue = True }:ps)
foldParam ('r':'w':'!':str) = \ps -> ((buildParam str "" "$?1" (Val VUndef)) { isLValue = True }:ps)
foldParam ""        = id
foldParam ('?':str)
    | (('r':'w':'!':typ), "=$_") <- break (== '=') str
    = \ps -> ((buildParam typ "?" "$?1" (Var "$_")) { isLValue = True }:ps)
    | (typ, "=$_") <- break (== '=') str
    = \ps -> (buildParam typ "?" "$?1" (Var "$_"):ps)
    | (typ, ('=':def)) <- break (== '=') str
    = let readVal "Num" = Val . VNum . read
          readVal "Int" = Val . VInt . read
          readVal "Str" = Val . VStr . read
          readVal x	= error $ "Unknown type: " ++ x
      in \ps -> (buildParam typ "?" "$?1" (readVal typ def):ps)
    | otherwise
    = \ps -> (buildParam str "?" "$?1" (Val VUndef):ps)
foldParam ('~':str) = \ps -> ((buildParam str "" "$?1" (Val VUndef)) { isThunk = True }:ps)
foldParam x         = doFoldParam x []

-- XXX -- Junctive Types -- XXX --

-- spre is "symbolic pre", that is, operators for which a precedence has
-- already been assigned in Parser.hs

--    ret_val   assoc	op_name args
initSyms = map primDecl . filter (not . null) . lines $ decodeUTF8 "\
\\n   Bool      spre    !       (Bool)\
\\n   Num       spre    +       (Num)\
\\n   Num       pre     abs     (?Num=$_)\
\\n   Num       pre     atan    (Num, Num)\
\\n   Num       pre     cos     (?Num=$_)\
\\n   Num       pre     sin     (?Num=$_)\
\\n   Num       pre     exp     (?Num=$_, ?Num)\
\\n   Num       pre     sqrt    (?Num=$_)\
\\n   Bool      spre    -d      (?Str=$_)\
\\n   Bool      spre    -f      (?Str=$_)\
\\n   Num       spre    -       (Num)\
\\n   Str       spre    ~       (Str)\
\\n   Bool      spre    ?       (Bool)\
\\n   Str       spre    =       (IO)\
\\n   List      spre    =       (IO)\
\\n   Str       pre     readline (IO)\
\\n   List      pre     readline (IO)\
\\n   Int       pre     int     (?Int=$_)\
\\n   List      pre     list    (List)\
\\n   Scalar    pre     scalar  (Scalar)\
\\n   List      pre     reverse (List)\
\\n   List      pre     sort    (List)\
\\n   Int       spre    +^      (Int)\
\\n   Int       spre    ~^      (Str)\
\\n   Bool      spre    ?^      (Bool)\
\\n   Ref       spre    \\      (List)\
\\n   Ref       spre    \\      (Scalar)\
\\n   List      post    ...     (Str)\
\\n   List      post    ...     (Scalar)\
\\n   Any       pre     undef   ()\
\\n   Any       pre     undef   (?rw!Any=$_)\
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
\\n   List      pre     map     (Array: Code)\
\\n   List      pre     grep    (Array: Code)\
\\n   Int       pre     push    (rw!Array, List)\
\\n   Int       pre     unshift (rw!Array, List)\
\\n   Scalar    pre     pop     (rw!Array)\
\\n   Scalar    pre     shift   (rw!Array)\
\\n   Str       pre     join    (Str, List)\
\\n   Str       pre     join    (Array, Str)\
\\n   List      left    zip     (List)\
\\n   List      pre     keys    (Hash)\
\\n   List      pre     values  (Hash)\
\\n   List      pre     kv      (Hash)\
\\n   Str       pre     perl    (List)\
\\n   Any       pre     eval    (Str)\
\\n   Any       pre     eval_perl5 (Str)\
\\n   Any       pre     require (?Str=$_)\
\\n   Any       pre     require_haskell (Str)\
\\n   Any       pre     last    (?Int=1)\
\\n   Any       pre     next    (?Int=1)\
\\n   Any       pre     redo    (?Int=1)\
\\n   Any       pre     exit    (?Int=0)\
\\n   Num       pre     rand    (?Num=1)\
\\n   Bool      pre     defined (Any)\
\\n   Str       pre     ref     (Any)\
\\n   Num       pre     time    ()\
\\n   Str       pre     File::Spec::cwd  ()\
\\n   Bool      pre     print   (List)\
\\n   Bool      pre     say     (IO: List)\
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
\\n   Int       pre     chars   (?Str=$_)\
\\n   Int       pre     bytes   (?Str=$_)\
\\n   Int       pre     chmod   (List)\
\\n   Scalar    pre     key     (Pair)\
\\n   Scalar    pre     value   (Pair)\
\\n   List      pre     kv      (Pair)\
\\n   List      pre     values  (Junction)\
\\n   Any       pre     pick    (Junction)\
\\n   Bool      pre     rename  (Str, Str)\
\\n   Bool      pre     symlink (Str, Str)\
\\n   Bool      pre     link    (Str, Str)\
\\n   Int       pre     unlink  (List)\
\\n   Str       pre     readlink (Str)\
\\n   List      pre     split   (Str, Str)\
\\n   Str       spre    =       (IO)\
\\n   List      spre    =       (IO)\
\\n   Junction  list    |       (Any)\
\\n   Junction  list    &       (Any)\
\\n   Junction  list    ^       (Any)\
\\n   Junction  list    !       (Any)\
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
\\n   List      non     ..      (Any, Any)\
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
\\n   List      list    ,       (List)\
\\n   List      list	¥		(Array)\
\\n   List      list	Y		(Array)\
\\n   List      spre    <==     (List)\
\\n   List      left    ==>     (List, Code)\
\\n   Scalar    left    and     (Bool, ~Bool)\
\\n   Scalar    left    or      (Bool, ~Bool)\
\\n   Scalar    left    nor     (Bool, ~Bool)\
\\n   Scalar    left    xor     (Bool, Bool)\
\\n   Scalar    left    err     (Bool, ~Bool)\
\\n   Int       pre     chr     (?Str=$_)\
\\n   Str       pre     ord     (?Int=$_)\
\\n   Str       pre     hex     (?Str=$_)\
\\n   Str       pre     hex     (Int)\
\\n   Num       pre     log     (Int)\
\\n   Num       pre     log     (Num)\
\\n   Num       pre     log10   (Num)\
\\n   Any       list    ;       (Any)\
\\n   Thread    pre     async   (Code)\
\\n   Bool      pre     yield   (?Thread)\
\\n   Int       pre     sign    (Num)\
\\n"

