{-# OPTIONS_GHC -fglasgow-exts #-}

module PIL.Native.Objects (
    Boxable(..),
    ObjectSpace,
    dumpObjSpace,
    newObject,
    getAttr, setAttr, addAttr, hasAttr
) where
import PIL.Native.Coerce
import PIL.Native.Types
import PIL.Native.Pretty
import System.Mem.Weak
import Control.Exception
import Control.Monad.State

class IsNative a => Boxable a where
    boxType :: a -> NativeStr
    boxType _ = error "Cannot autobox"
    autobox :: (MonadState ObjectSpace m, MonadIO m) => a -> NativeObj -> m NativeObj
    autobox v cls = do
        objs <- get
        let obj = MkObject oid cls fetch exists store freeze thaw
            oid = size objs
            fetch _ = return (toNative v)
            exists _ = return True
            store _ _ = failWith "Cannot store into autoboxed" (boxType v)
            freeze = fail "freeze"
            thaw = fail "thaw"
        ptr <- liftIO $ mkWeak obj obj Nothing
        put (insert objs oid ptr)
        return obj

instance Boxable NativeBit where boxType _ = mkStr "::Bit"
instance Boxable NativeInt where boxType _ = mkStr "::Int"
instance Boxable NativeNum where boxType _ = mkStr "::Num"
instance Boxable NativeStr where boxType _ = mkStr "::Str"
instance Boxable NativeSeq where boxType _ = mkStr "::Seq"
instance Boxable NativeMap where boxType _ = mkStr "::Map"
instance Boxable NativeSub where boxType _ = mkStr "::Sub"

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

newObject :: (MonadState ObjectSpace m, MonadIO m, MonadSTM m) =>
    NativeObj -> NativeMap -> m NativeObj
newObject cls attrs = do
    tvar  <- liftSTM $ newTVar attrs
    objs  <- get
    let obj = MkObject oid cls fetch exists store freeze thaw
        oid = size objs
        fetch key = fmap (! key) $ readTVar tvar
        exists key = do
            attrs <- readTVar tvar
            return (PIL.Native.Coerce.exists attrs key)
        store key val = do
            attrs <- readTVar tvar
            writeTVar tvar (insert attrs key val)
        freeze = fail "freeze"
        thaw = fail "thaw"
    ptr <- liftIO $ mkWeak tvar obj Nothing
    put (insert objs oid ptr)
    return obj
