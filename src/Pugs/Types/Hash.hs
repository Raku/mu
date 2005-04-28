type HashIndex = VStr

class (Typeable a) => HashClass a where
    hash_iType :: a -> Type
    hash_iType = const $ mkType "Hash"
    hash_fetch       :: a -> Eval VHash
    hash_fetch hv = do
        keys <- hash_fetchKeys hv
        vals <- mapM (hash_fetchVal hv) keys
        return . Map.fromList $ keys `zip` vals
    hash_store       :: a -> VHash -> Eval ()
    hash_store hv vals = do
        hash_clear hv
        forM_ (Map.assocs vals) $ \(key, val) -> do
            hash_storeVal hv key val
    hash_fetchElem   :: a -> HashIndex -> Eval (IVar VScalar) -- autovivify
    hash_fetchElem hv key = do
        return $ proxyScalar (hash_fetchVal hv key) (hash_storeVal hv key)
    hash_storeElem   :: a -> HashIndex -> IVar VScalar -> Eval () -- binding
    hash_storeElem hv idx sv = do
        val <- readIVar sv
        hash_storeVal hv idx val
    hash_fetchVal    :: a -> HashIndex -> Eval Val
    hash_fetchVal hv key = do
        rv <- hash_existsElem hv key
        if rv then readIVar =<< hash_fetchElem hv key
              else return undef
    hash_storeVal    :: a -> HashIndex -> Val -> Eval ()
    hash_storeVal hv key val = do
        sv <- hash_fetchElem hv key
        writeIVar sv val
    hash_fetchKeys   :: a -> Eval [HashIndex]
    hash_fetchKeys hv = do
        vals <- hash_fetch hv
        return $ Map.keys vals
    hash_deleteElem  :: a -> HashIndex -> Eval ()
    hash_existsElem  :: a -> HashIndex -> Eval VBool
    hash_existsElem hv idx = do
        keys <- hash_fetchKeys hv
        return $ idx `elem` keys
    hash_clear       :: a -> Eval ()
    hash_clear hv = do
        keys <- hash_fetchKeys hv
        mapM_ (hash_deleteElem hv) keys
    hash_isEmpty     :: a -> Eval VBool
    hash_isEmpty hv = do
        keys <- hash_fetchKeys hv
        return $ null keys 

instance HashClass (IVar VPair) where
    hash_iType = const $ mkType "Pair"
    hash_fetch pv = do
        (k, v)  <- readIVar pv
        str     <- fromVal k
        return $ Map.singleton str v
    hash_fetchVal pv idx = do
        (k, v)  <- readIVar pv
        str     <- fromVal k
        if str == idx
            then return v
            else return undef
    hash_storeVal _ _ _ = retConstError undef
    hash_deleteElem _ _ = retConstError undef

instance HashClass VHash where
    hash_iType = const $ mkType "Hash::Const"
    hash_fetch = return
    hash_fetchKeys = return . Map.keys
    hash_fetchVal hv idx = return $ Map.findWithDefault undef idx hv
    hash_clear _ = retConstError undef
    hash_store _ _ = retConstError undef
    hash_storeVal _ _ _ = retConstError undef
    hash_storeElem _ _ _ = retConstError undef
    hash_deleteElem _ _ = retConstError undef

instance HashClass IHashEnv where
    hash_iType = const $ mkType "Hash::Env"
    hash_fetch _ = do
        envs <- liftIO getEnvironment
        return . Map.map VStr $ Map.fromList envs
    hash_fetchVal _ key = tryIO undef $ do
        str <- getEnv key
        return $ VStr str
    hash_storeVal _ key val = do
        str <- fromVal val
        liftIO $ setEnv key str True
    hash_existsElem _ key = tryIO False $ do
        getEnv key
        return True
    hash_deleteElem _ key = do
        liftIO $ unsetEnv key

instance HashClass IHash where
    hash_fetch hv = do
        svMap <- liftSTM $ readTVar hv
        fmap Map.fromList $ forM (Map.assocs svMap) $ \(key, sv) -> do
            val <- readIVar sv
            return (key, val)
    hash_fetchKeys hv = do
        liftSTM . fmap Map.keys $ readTVar hv
    hash_fetchElem hv idx = do
        svMap <- liftSTM $ readTVar hv
        case Map.lookup idx svMap of
            Just sv -> return sv
            Nothing -> do
                sv <- newScalar undef
                liftSTM $ modifyTVar hv (Map.insert idx sv)
                return sv
    hash_storeElem hv idx sv = do
        liftSTM $ modifyTVar hv (Map.insert idx sv)
    hash_deleteElem hv idx = do
        liftSTM $ modifyTVar hv (Map.delete idx)
    hash_existsElem hv idx = do
        liftSTM $ do
            svMap <- readTVar hv
            return $ Map.member idx svMap
