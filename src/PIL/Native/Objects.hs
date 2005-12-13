{-# OPTIONS_GHC -fglasgow-exts #-}

module PIL.Native.Objects (ObjectSpace, dumpObjSpace, getAttr) where
import PIL.Native.Coerce
import PIL.Native.Types
import PIL.Native.Pretty
import System.Mem.Weak
import Control.Monad

type ObjectSpace = SeqOf (Weak NativeObj)

dumpObjSpace :: ObjectSpace -> IO ()
dumpObjSpace ptrs = mapM_ dumpObj (assocs ptrs)
    where
    dumpObj (idx, ptr) = do
        rv <- deRefWeak ptr
        case rv of
            Just obj -> do
                putStr $ show idx ++ ": "
                putStrLn =<< pretty obj
            Nothing -> return ()

getAttr :: MonadSTM m => NativeObj -> NativeStr -> m Native
getAttr obj att = do
    attrs <- liftSTM $ readTVar (o_attrs obj)
    case attrs `fetch` att of
        Just val -> return val
        Nothing  -> fail "no such attribute"
