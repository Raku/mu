{-# OPTIONS_GHC -fglasgow-exts #-}

module PIL.Native.Objects (
    -- Boxable(..),
    ObjectSpace,
    dumpObjSpace,
    createObject, createOpaque, objectReprName, callRepr,
    _get_attr,
) where
import PIL.Native.Coerce
import PIL.Native.Types
import PIL.Native.Pretty
import PIL.Repr
import System.Mem.Weak
import Control.Exception
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Seq as Seq

type ObjectSpace = SeqOf (Weak NativeObj)

_p6opaque = mkStr "p6opaque"
_get_attr = mkStr "get_attr"

createOpaque :: (MonadState ObjectSpace m, MonadIO m, MonadSTM m) =>
    NativeObj -> NativeMap -> m NativeObj
createOpaque cls attrs = createObject cls _p6opaque (toNative attrs)

createObject :: (MonadState ObjectSpace m, MonadIO m, MonadSTM m) =>
    NativeObj -> NativeStr -> Native -> m NativeObj
createObject cls name init = registerObject $ do
    repr <- liftSTM $ createRepr name init
    return (MkObject 0 cls repr)

objectReprName :: NativeObj -> NativeStr
objectReprName = reprName . o_repr

registerObject :: (MonadState ObjectSpace m, MonadIO m, MonadSTM m) => STM NativeObj -> m NativeObj
registerObject gen = do
    obj  <- liftSTM gen
    objs <- get
    let obj' = obj{ o_id = oid }
        oid  = size objs
    ptr <- liftIO $ mkWeakPtr obj' Nothing
    put (insert objs oid ptr)
    return obj'

dumpObjSpace :: ObjectSpace -> IO ()
dumpObjSpace ptrs = mapM_ dumpObj (elems ptrs)
    where
    dumpObj ptr = do
        rv <- deRefWeak ptr
        maybe (return ()) doDumpObj rv
    doDumpObj obj = do
        putStr $ "#obj# " ++ pretty obj
        (handle (const $ putStrLn "")) $ do
            val <- liftSTM $ callRepr (o_repr obj) _get_attr (mkSeq [toNative "$!name"])
            print $! toString val


{-
class IsNative a => Boxable a where
    boxType :: a -> NativeStr
    boxType _ = error "Cannot autobox"
    autobox :: (MonadState ObjectSpace m, MonadSTM m, MonadIO m) => a -> NativeObj -> m NativeObj
    autobox v cls = registerObject $ mkPrimObject cls
        [ ("unbox", const $ return (toNative v)) ]

defaultCreate :: NativeObj -> NativeSeq -> STM Native
defaultCreate cls args = fmap toNative (genObject cls (fromNative $ args ! 0))

mkPrimObject :: Monad m => NativeObj -> [(String, ObjectPrim)] -> m NativeObj
mkPrimObject cls repr = return (MkObject 0 cls (mkMap (clsPrim:repr)))
    where
    clsPrim = ("meta", const (return (toNative cls)))

-- Int becomes Haskell Integer when autoboxed
instance Boxable NativeInt where
    boxType _ = mkStr "^Int"
    autobox v = autobox (toInteger v)

instance Boxable NativeBit where boxType _ = mkStr "^Bit"
instance Boxable NativeNum where boxType _ = mkStr "^Num"
instance Boxable NativeStr where boxType _ = mkStr "^Str"
instance Boxable NativeSeq where boxType _ = mkStr "^Seq"
instance Boxable NativeMap where boxType _ = mkStr "^Map"
instance Boxable NativeSub where boxType _ = mkStr "^Sub"

instance Boxable Integer where
    boxType _ = mkStr "^Int"
-}
