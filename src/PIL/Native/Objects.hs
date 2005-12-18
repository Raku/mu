{-# OPTIONS_GHC -fglasgow-exts #-}

module PIL.Native.Objects (
    ObjectSpace,
    dumpObjSpace,
    newObject,
    getAttr, setAttr, addAttr,
) where
import PIL.Native.Coerce
import PIL.Native.Types
import PIL.Native.Pretty
import System.Mem.Weak
import Control.Exception
import Control.Monad.State

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

setAttr :: MonadSTM m => NativeObj -> NativeStr -> Native -> m ()
setAttr obj att val = liftSTM $ o_store obj att val

addAttr :: MonadSTM m => NativeObj -> NativeStr -> m ()
addAttr obj att = liftSTM $ o_store obj att nil

newObject :: (MonadState ObjectSpace m, MonadIO m, MonadSTM m) =>
    NativeObj -> NativeMap -> m NativeObj
newObject cls attrs = do
    tvar  <- liftSTM $ newTVar attrs
    objs  <- get
    let obj = MkObject oid cls fetch store freeze thaw
        oid = size objs
        fetch key = fmap (! key) $ readTVar tvar
        store key val = do
            attrs <- readTVar tvar
            writeTVar tvar (insert attrs key val)
        freeze = error "freeze"
        thaw = error "thaw"
    ptr <- liftIO $ mkWeak tvar obj Nothing
    put (insert objs oid ptr)
    return obj
