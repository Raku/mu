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

instance ArrayClass IArraySlice where
    array_iType = const $ mkType "Array::Slice"
    array_store av vals = mapM_ (uncurry writeIVar) (zip av vals)
    array_fetchSize = return . length
    array_fetchElem av idx = getIndex idx Nothing (return av) Nothing
    array_storeSize _ _ = return () -- XXX error?
    array_storeElem _ _ _ = retConstError undef

instance ArrayClass IArray where
    array_store av vals = do
        let svList = map lazyScalar vals
        liftSTM $ writeTVar av $ IntMap.fromAscList ([0..] `zip` svList)
    array_fetchSize av = do
        avMap <- liftSTM $ readTVar av
        return $ IntMap.size avMap
    array_storeSize av sz = do
        liftSTM $ modifyTVar av $ \avMap ->
            let size = IntMap.size avMap in
            case size `compare` sz of
                GT -> fst $ IntMap.split sz avMap
                EQ -> avMap
                LT -> IntMap.union avMap $
                    IntMap.fromAscList ([size .. sz-1] `zip` repeat lazyUndef)
    array_shift av = do
        svList <- liftSTM $ fmap IntMap.elems $ readTVar av
        case svList of
            (sv:rest) -> do
                liftSTM $ writeTVar av $ IntMap.fromAscList ([0..] `zip` rest)
                readIVar sv
            _ -> return undef
    array_unshift av vals = do
        liftSTM $ modifyTVar av $ \avMap ->
            let svList = IntMap.elems avMap in
            IntMap.fromAscList ([0..] `zip` ((map lazyScalar vals) ++ svList))
    array_pop av = do
        avMap <- liftSTM $ readTVar av
        if IntMap.null avMap
            then return undef
            else do
                let idx = IntMap.size avMap - 1
                liftSTM $ writeTVar av $ IntMap.delete idx avMap
                readIVar . fromJust $ IntMap.lookup idx avMap
    array_push av vals = do
        liftSTM $ modifyTVar av $ \avMap ->
            let svList = IntMap.elems avMap in
            IntMap.fromAscList ([0..] `zip` (svList ++ (map lazyScalar vals)))
    array_extendSize _ 0 = return ()
    array_extendSize av sz = do
        liftSTM $ modifyTVar av $ \avMap ->
            let size = IntMap.size avMap in
            case size `compare` sz of
                GT -> avMap
                EQ -> avMap
                LT -> IntMap.union avMap $
                    IntMap.fromAscList ([size .. sz-1] `zip` repeat lazyUndef)
    array_fetchVal av idx = do
        readIVar =<< getMapIndex idx (Just $ constScalar undef)
            (liftSTM $ readTVar av) 
            Nothing -- don't bother extending
    array_fetchKeys av = do
        avMap <- liftSTM $ readTVar av
        return $ IntMap.keys avMap
    array_fetchElem av idx = do
        sv <- getMapIndex idx Nothing
            (liftSTM $ readTVar av) 
            (Just (array_extendSize av $ idx+1))
        if refType (MkRef sv) == mkType "Scalar::Lazy"
            then do
                val <- readIVar sv
                sv' <- newScalar val
                liftSTM . modifyTVar av $ \avMap ->
                    let idx' = idx `mod` IntMap.size avMap in
                    IntMap.adjust (const sv') idx' avMap
                return sv'
            else return sv
    array_existsElem av idx | idx < 0 = array_existsElem av (abs idx - 1)
    array_existsElem av idx = do
        avMap <- liftSTM $ readTVar av
        return $ IntMap.member idx avMap
    array_deleteElem av idx = do
        liftSTM . modifyTVar av $ \avMap ->
            let size = IntMap.size avMap
                idx' | idx < 0   = idx `mod` IntMap.size avMap -- XXX wrong; wraparound
                     | otherwise = idx in
            case (size - 1) `compare` idx' of
                LT -> avMap
                EQ -> IntMap.delete idx' avMap
                GT -> IntMap.adjust (const lazyUndef) idx' avMap
    array_storeElem av idx sv = do
        liftSTM . modifyTVar av $ \avMap ->
            let size = IntMap.size avMap
                idx' | idx < 0   = idx `mod` IntMap.size avMap -- XXX wrong; wraparound
                     | otherwise = idx in
            if size > idx'
                then IntMap.adjust (const sv) idx' avMap
                else IntMap.union avMap $
                    IntMap.fromAscList ([size .. idx'] `zip` (sv:repeat lazyUndef))

instance ArrayClass VArray where
    array_iType = const $ mkType "Array::Const"
    array_store [] _ = return ()
    array_store _ [] = return ()
    array_store (VUndef:as) (_:vs) = array_store as vs
    array_store (a:as) vals@(v:vs) = do
        env <- ask
        ref <- fromVal a
        if isaType (envClasses env) "List" (refType ref)
            then writeRef ref (VList vals)
            else do
                writeRef ref v
                array_store as vs
    array_fetch = return
    array_fetchSize = return . length
    array_fetchVal av idx = getIndex idx (Just undef) (return av) Nothing
    array_fetchElem av idx = do
        val <- array_fetchVal av idx
        return $ constScalar val
    array_storeVal _ _ _ = retConstError undef
    array_storeElem _ _ _ = retConstError undef

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
    array_storeVal _ _ _   = retConstError undef
    array_storeElem _ _ _  = retConstError undef
    array_deleteElem _ _   = retConstError undef

evalPerl5Sub :: String -> [PerlSV] -> Eval Val
evalPerl5Sub code args = do
    env <- ask
    rv  <- liftIO $ do
        envSV <- mkVal env
        subSV <- evalPerl5 code envSV (enumCxt cxtItemAny)
        invokePerl5 subSV nullSV args envSV (enumCxt cxtItemAny)
    return $ case rv of
        [sv]    -> PerlSV sv
        _       -> VList (map PerlSV rv)

instance ArrayClass PerlSV where
    array_iType = const $ mkType "Array::Perl"
    array_fetchVal sv idx = do
        idxSV   <- fromVal $ castV idx
        evalPerl5Sub "sub { $_[0]->[$_[1]] }" [sv, idxSV]
    array_clear sv = do
        evalPerl5Sub "sub { undef @{$_[0]} }" [sv]
        return ()
    array_storeVal sv idx val = do
        idxSV   <- fromVal $ castV idx
        valSV   <- fromVal val
        evalPerl5Sub "sub { $_[0]->[$_[1]] = $_[2] }" [sv, idxSV, valSV]
        return ()
    array_deleteElem sv idx = do
        idxSV   <- fromVal $ castV idx
        evalPerl5Sub "sub { delete $_[0]->[$_[1]] }" [sv, idxSV]
        return ()
