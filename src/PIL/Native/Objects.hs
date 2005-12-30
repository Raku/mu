{-# OPTIONS_GHC -fglasgow-exts #-}

module PIL.Native.Objects (
    Boxable(..),
    ObjectSpace,
    dumpObjSpace,
    newObject, registerObject, genObject,
    newScalarObj,
    getAttr, setAttr, addAttr, hasAttr
) where
import PIL.Native.Coerce
import PIL.Native.Types
import PIL.Native.Pretty
import System.Mem.Weak
import Control.Exception
import Control.Monad.State

newScalarObj :: NativeObj -> NativeSeq -> STM NativeObj
newScalarObj cls args = do
    tvar <- newTVar (args ! 0)
    mkPrimObject cls
        (const $ readTVar tvar)   -- fetch
        (const $ return True)     -- exists
        (const $ writeTVar tvar)  -- store

class IsNative a => Boxable a where
    boxType :: a -> NativeStr
    boxType _ = error "Cannot autobox"
    autobox :: (MonadState ObjectSpace m, MonadSTM m, MonadIO m) => a -> NativeObj -> m NativeObj
    autobox v cls = registerObject $ mkPrimObject cls
        (const $ return (toNative v))   -- fetch
        (const $ return True)           -- exists
        (const . const $ failWith "Cannot store into autoboxed" (boxType v)) -- store

defaultCreate :: NativeObj -> NativeSeq -> STM NativeObj
defaultCreate cls args = genObject cls (fromNative $ args ! 0)

mkPrimObject :: NativeObj -> (NativeStr -> STM Native) -> (NativeStr -> STM Bool) -> (NativeStr -> Native -> STM ()) -> STM NativeObj 
mkPrimObject cls _fetch _exists _store = do
    let freeze = fail "freeze"
        thaw   = fail "thaw"
        create = const (fail "create")
    return (MkObject 0 cls _fetch _exists _store freeze thaw create)

-- Int becomes Haskell Integer when autoboxed
instance Boxable NativeInt where
    boxType _ = mkStr "::Int"
    autobox v = autobox (toInteger v)

instance Boxable NativeBit where boxType _ = mkStr "::Bit"
instance Boxable NativeNum where boxType _ = mkStr "::Num"
instance Boxable NativeStr where boxType _ = mkStr "::Str"
instance Boxable NativeSeq where boxType _ = mkStr "::Seq"
instance Boxable NativeMap where boxType _ = mkStr "::Map"
instance Boxable NativeSub where boxType _ = mkStr "::Sub"

instance Boxable Integer where
    boxType _ = mkStr "::Int"

type ObjectSpace = SeqOf (Weak NativeObj)

dumpObjSpace :: ObjectSpace -> IO ()
dumpObjSpace ptrs = mapM_ dumpObj (elems ptrs)
    where
    dumpObj ptr = do
        rv <- deRefWeak ptr
        case rv of
            Just obj -> do
                putStr $ "#obj# " ++ pretty obj
                (handle (const $ putStrLn "")) $ do
                    val <- getAttr obj (mkStr "$!name")
                    print $! toString val
            Nothing  -> return ()

getAttr :: MonadSTM m => NativeObj -> NativeStr -> m Native
getAttr obj att = liftSTM $ o_fetch obj att

hasAttr :: MonadSTM m => NativeObj -> NativeStr -> m Bool
hasAttr obj att = liftSTM $ o_exists obj att

setAttr :: MonadSTM m => NativeObj -> NativeStr -> Native -> m ()
setAttr obj att val = liftSTM $ o_store obj att val

addAttr :: MonadSTM m => NativeObj -> NativeStr -> m ()
addAttr obj att = liftSTM $ o_store obj att nil

genObject :: NativeObj -> NativeMap -> STM NativeObj
genObject cls attrs = do
    tvar  <- liftSTM $ newTVar attrs
    let fetch key = fmap (! key) $ readTVar tvar
        exists key = do
            attrs <- readTVar tvar
            return (PIL.Native.Coerce.exists attrs key)
        store key val = do
            attrs <- readTVar tvar
            writeTVar tvar (insert attrs key val)
        freeze = fail "freeze"
        thaw = fail "thaw"
        create = defaultCreate cls
    return (MkObject (-1) cls fetch exists store freeze thaw create)

newObject :: (MonadState ObjectSpace m, MonadIO m, MonadSTM m) =>
    NativeObj -> NativeMap -> m NativeObj
newObject cls attrs = registerObject (genObject cls attrs)

registerObject :: (MonadState ObjectSpace m, MonadIO m, MonadSTM m) => STM NativeObj -> m NativeObj
registerObject gen = do
    obj  <- liftSTM gen
    objs <- get
    let obj' = obj{ o_id = oid }
        oid = size objs
    ptr <- liftIO $ mkWeakPtr obj' Nothing
    put (insert objs oid ptr)
    return obj'

