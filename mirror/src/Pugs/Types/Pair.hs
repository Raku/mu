
class (Typeable a) => PairClass a where
    pair_iType :: a -> Type
    pair_iType = const $ mkType "Pair"
    pair_fetch :: a -> Eval VPair
    pair_fetch pv = do
        key <- pair_fetchKey pv
        val <- pair_fetchVal pv
        return (key, val)
    pair_fetchKey  :: a -> Eval VScalar
    pair_fetchVal  :: a -> Eval VScalar
    pair_fetchVal pv = do
        readIVar =<< pair_fetchElem pv
    pair_storeVal  :: a -> Val -> Eval ()
    pair_storeVal pv val = do
        sv <- pair_fetchElem pv
        writeIVar sv val
    pair_fetchElem :: a -> Eval (IVar VScalar)
    pair_fetchElem pv = do
        return $ proxyScalar (pair_fetchVal pv) (pair_storeVal pv)

instance PairClass VPair where
    pair_fetchKey = return . fst
    pair_fetchVal = return . snd
    pair_storeVal pv val = do
        ref <- fromVal (snd pv)
        writeRef ref val
    pair_fetch pv = do
        key <- pair_fetchKey pv
        val <- pair_fetchVal pv
        return (key, val)
    pair_fetchElem pv = do
        return $ proxyScalar (pair_fetchVal pv) (pair_storeVal pv)

instance PairClass IPairHashSlice where
    pair_iType = const $ mkType "Pair::HashSlice"
    pair_fetchKey  = return . VStr . fst
    pair_fetchElem = return . snd
