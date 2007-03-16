type ArrayIndex = Int

class (Typeable a) => ArrayClass a where
    array_iType :: a -> Type
    array_iType = const $ mkType "Array"
    array_fetch       :: a -> Eval VArray
    array_fetch av = do
        size <- array_fetchSize av
        mapM (array_fetchVal av) [0..size-1]
    array_store       :: a -> VArray -> Eval ()
    array_store av list = do
        forM_ ([0..] `zip` list) $ \(idx, val) -> do
            sv <- array_fetchElem av idx
            writeIVar sv val
        array_storeSize av (length list)
    array_fetchKeys   :: a -> Eval [ArrayIndex]
    array_fetchKeys av = do
        svList <- array_fetch av
        return $ zipWith const [0..] svList
    array_fetchElemAll :: a -> Eval [IVar VScalar]
    array_fetchElemAll av = do
        size <- array_fetchSize av
        mapM (array_fetchElem av) [0..size-1]
    array_fetchElem   :: a -> ArrayIndex -> Eval (IVar VScalar) -- autovivify
    array_fetchElem av idx = do
        return $ proxyScalar (array_fetchVal av idx) (array_storeVal av idx)
    array_storeElem   :: a -> ArrayIndex -> IVar VScalar -> Eval () -- binding
    array_storeElem av idx sv = do
        val <- readIVar sv
        array_storeVal av idx val
    array_fetchVal    :: a -> ArrayIndex -> Eval Val
    array_fetchVal av idx = do
        rv <- array_existsElem av idx
        if rv then readIVar =<< array_fetchElem av idx
              else return undef
    array_storeVal    :: a -> ArrayIndex -> Val -> Eval ()
    array_storeVal av idx val = do
        sv <- array_fetchElem av idx
        writeIVar sv val
    array_fetchSize   :: a -> Eval ArrayIndex
    array_fetchSize av = do
        vals <- array_fetch av
        return $ length vals
    array_storeSize   :: a -> ArrayIndex -> Eval ()
    array_storeSize av sz = do
        size <- array_fetchSize av
        case size `compare` sz of
            GT -> mapM_ (const $ array_pop av) [size .. sz-1]
            EQ -> return () -- no need to do anything
            LT -> mapM_ (\idx -> array_storeElem av idx lazyUndef) [size .. sz-1]
    array_extendSize  :: a -> ArrayIndex -> Eval ()
    array_extendSize _ 0 = return ()
    array_extendSize av sz = do
        size <- array_fetchSize av
        when (size < sz) $ do
            mapM_ (\idx -> array_storeElem av idx lazyUndef) [size .. sz-1]
    array_deleteElem  :: a -> ArrayIndex -> Eval ()
    array_deleteElem av idx = do
        size <- array_fetchSize av
        let idx' = if idx < 0 then idx `mod` size else idx
        case (size - 1) `compare` idx' of
            GT -> return ()                             -- no such index
            EQ -> array_storeSize av (size - 1)               -- truncate
            LT -> array_storeElem av idx' lazyUndef            -- set to undef
    array_existsElem  :: a -> ArrayIndex -> Eval VBool
    array_existsElem av idx = do
        size <- array_fetchSize av
        return $ size > (if idx < 0 then idx `mod` size else idx)
    array_clear       :: a -> Eval ()
    array_clear av = array_storeSize av 0
    array_push        :: a -> [Val] -> Eval ()
    array_push av vals = do
        size <- array_fetchSize av
        forM_ ([size..] `zip` vals) $ \(idx, val) -> do
            array_storeElem av idx (lazyScalar val)
    array_pop         :: a -> Eval Val
    array_pop av = do
        size <- array_fetchSize av
        if size == 0
            then return undef
            else do
                sv <- array_fetchElem av $ size - 1
                array_storeSize av $ size - 1
                readIVar sv
    array_shift       :: a -> Eval Val
    array_shift av = do
        vals <- array_splice av 0 1 []
        return $ last (undef:vals)
    array_unshift     :: a -> [Val] -> Eval ()
    array_unshift av vals = do
        array_splice av 0 0 vals
        return ()
    array_splice      :: a -> ArrayIndex -> ArrayIndex -> [Val] -> Eval [Val]
    array_splice av off len vals = do
        size <- array_fetchSize av
        let off' = if off < 0 then off + size else off
            len' = if len < 0 then len + size - off' else len
        result <- mapM (array_fetchElem av) [off' .. off' + len' - 1]
        let off = if off' > size then size else off'
            len = if off + len' > size then size - off else len'
            cnt = length vals
        case cnt `compare` len of
            GT -> do
                -- Move items up to make room
                let delta = cnt - len
                array_extendSize av (size + delta)
                (`mapM_` reverse [off + len .. size - 1]) $ \idx -> do
                    val <- array_fetchElem av idx
                    array_storeElem av (idx + delta) val
            LT -> do
                let delta = len - cnt
                (`mapM_` [off + len .. size - 1]) $ \idx -> do
                    val <- array_fetchElem av idx
                    array_storeElem av (idx - delta) val
                array_storeSize av (size - delta)
            _ -> return ()
        forM_ ([0..] `zip` vals) $ \(idx, val) -> do
            array_storeElem av (off + idx) (lazyScalar val)
        mapM readIVar result
    array_clone :: a -> STM a
    array_clone = return

instance ArrayClass IArraySlice where
    array_iType = const $ mkType "Array::Slice"
    array_store av vals = mapM_ (uncurry writeIVar) (zip av (vals ++ repeat undef))
    array_fetchSize = return . length
    array_fetchElem av idx = getIndex idx Nothing (return av) Nothing
    array_storeSize _ _ = return () -- XXX error?
    array_storeElem _ _ _ = retConstError undef

a_size :: [:IVar VScalar:] -> Int
a_size = lengthP

a_update :: Int -> IVar VScalar -> [:IVar VScalar:] -> [:IVar VScalar:]
a_update i x xs = takeP i xs +:+ [:x:] +:+ sliceP (i + 1) (lengthP xs - 1) xs

instance ArrayClass IArray where
    array_clone (MkIArray iv) = do
        a   <- readTVar iv
        tvs <- mapM cloneIVar (fromP a)
        fmap MkIArray (newTVar (toP tvs))
    array_store (MkIArray iv) vals = stm $ do
        tvs <- mapM newScalar vals
        writeTVar iv (toP tvs)
    array_fetchSize (MkIArray iv) = stm $ do
        a   <- readTVar iv
        return $ a_size a
    array_storeSize (MkIArray iv) sz = stm $ do
        a       <- readTVar iv
        case a_size a `compare` sz of
            GT -> writeTVar iv (takeP sz a) -- shrink
            EQ -> return ()
            LT -> do
                tvs <- replicateM (sz - a_size a) (newScalar undef)
                writeTVar iv (a +:+ toP tvs) -- extend
    array_shift (MkIArray iv) = join . stm $ do
        a   <- readTVar iv
        case a_size a of
            0   -> return (return undef)
            l   -> do
                writeTVar iv (sliceP 1 (l - 1) a)
                return (readIVar (a !: 0))
    array_unshift _ [] = return ()
    array_unshift (MkIArray iv) vals = stm $ do
        a   <- readTVar iv
        tvs <- mapM newScalar vals
        writeTVar iv (toP tvs +:+ a)
    array_pop (MkIArray iv) = join . stm $ do
        a   <- readTVar iv
        case a_size a of
            0   -> return (return undef)
            sz  -> do
                writeTVar iv (takeP (sz - 1) a)
                return (readIVar (a !: (sz - 1)))
    array_push _ [] = return ()
    array_push (MkIArray iv) vals = stm $ do
        a   <- readTVar iv
        tvs <- mapM newScalar vals
        writeTVar iv (a +:+ toP tvs)
    array_extendSize (MkIArray iv) sz = stm $ do
        a       <- readTVar iv
        case a_size a `compare` sz of
            LT  -> do
                tvs <- replicateM (sz - a_size a) (newScalar undef)
                writeTVar iv (a +:+ toP tvs)
            _   -> return ()
    array_fetchVal arr idx = do
        rv  <- getArrayIndex idx (Just $ constScalar undef)
                (return arr)
                Nothing -- don't bother extending
        readIVar rv
    array_fetchKeys (MkIArray iv) = stm $ do
        a   <- readTVar iv
        return [0 .. (a_size a - 1)]
    array_fetchElem arr idx = do
        getArrayIndex idx Nothing
            (return arr)
            (Just (array_extendSize arr $ idx+1))
    array_existsElem arr idx | idx < 0 = array_existsElem arr (abs idx - 1)    -- FIXME: missing mod?
    array_existsElem (MkIArray iv) idx = stm $ do
        a   <- readTVar iv
        return (idx < a_size a)
    array_storeElem arr@(MkIArray iv) idx sv = do
        a   <- stm $ readTVar iv
        let size = a_size a
        when (size <= idx) $ do
            array_extendSize arr (idx + 1)
        stm $ modifyTVar iv (a_update idx sv)
    array_deleteElem (MkIArray iv) idx = stm $ do
        a   <- readTVar iv
        let idx' | idx < 0   = idx `mod` size        --- XXX wrong; wraparound => what do you mean?
                 | otherwise = idx
            size = a_size a
        case (size-1) `compare` idx' of
            LT -> return ()
            EQ -> writeTVar iv (takeP (size-1) a)
            GT -> do
                tvar <- newScalar undef
                writeTVar iv (a_update idx' tvar a)
    array_splice (MkIArray iv) off len vals = join . stm $ do
        a    <- readTVar iv

        let off' = if off < 0 then off + size else off
            len' = if len < 0 then len + size - off' else len
            size = a_size a

        let result = mapM readIVar (fromP (sliceP off' (off' + len' - 1) a))

        let off = if off' > size then size else off'
            len = if off + len' > size then size - off else len'

        tvars  <- mapM newScalar vals
        writeTVar iv (takeP off a +:+ toP tvars +:+ sliceP (off+len) (size-1) a)
        return result

instance ArrayClass VArray where
    array_iType = const $ mkType "Array::Const"
    array_store [] _ = return ()
    array_store (VUndef:as) (_:vs) = array_store as vs
    array_store as [] = forM_ as $ \a -> do
        -- clear out everything
        ref <- fromVal a
        if isaType "List" (refType ref)
            then writeRef ref (VList [])
            else writeRef ref VUndef
    array_store (a:as) vals@(v:vs) = do
        ref <- fromVal a
        if isaType "List" (refType ref)
            then do
                writeRef ref (VList vals)
                array_store as []
            else do
                writeRef ref v
                array_store as vs
    array_fetch = return
    array_fetchSize = return . length
    array_fetchVal av idx = getIndex idx (Just undef) (return av) Nothing
    array_fetchElemAll av = return $ map constScalar av
    array_fetchElem av idx = do
        val <- array_fetchVal av idx
        return $ constScalar val
    array_storeVal _ _ _ = retConstError undef
    array_storeElem _ _ _ = retConstError undef
    array_existsElem av idx = return . not . null $ drop idx av

instance ArrayClass (IVar VPair) where
    array_iType = const $ mkType "Pair"
    array_fetch pv = do
        (k, v)  <- readIVar pv
        return [k, v]
    array_existsElem _ idx = return (idx >= -2 || idx <= 1)
    array_fetchSize        = const $ return 2
    array_fetchVal pv (-2) = return . fst =<< readIVar pv
    array_fetchVal pv (-1) = return . snd =<< readIVar pv
    array_fetchVal pv 0    = return . fst =<< readIVar pv
    array_fetchVal pv 1    = return . snd =<< readIVar pv
    array_fetchVal _  _    = return undef
    array_storeVal a _ _   = retConstError $ VStr $ show a
    array_storeElem a _ _  = retConstError $ VStr $ show a
    array_deleteElem a _   = retConstError $ VStr $ show a

perl5EvalApply :: String -> [PerlSV] -> Eval Val
perl5EvalApply code args = do
    env     <- ask
    envSV   <- io $ mkEnv env
    subSV   <- io $ evalPerl5 code envSV (enumCxt cxtItemAny)
    runInvokePerl5 subSV nullSV args

instance ArrayClass PerlSV where
    array_iType = const $ mkType "Array::Perl"
    array_fetchVal sv idx = do
        idxSV   <- fromVal $ castV idx
        perl5EvalApply "sub { $_[0]->[$_[1]] }" [sv, idxSV]
    array_clear sv = do
        perl5EvalApply "sub { undef @{$_[0]} }" [sv]
        return ()
    array_storeVal sv idx val = do
        idxSV   <- fromVal $ castV idx
        valSV   <- fromVal val
        perl5EvalApply "sub { $_[0]->[$_[1]] = $_[2] }" [sv, idxSV, valSV]
        return ()
    array_deleteElem sv idx = do
        idxSV   <- fromVal $ castV idx
        perl5EvalApply "sub { delete $_[0]->[$_[1]] }" [sv, idxSV]
        return ()
