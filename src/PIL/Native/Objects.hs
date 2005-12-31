{-# OPTIONS_GHC -fglasgow-exts #-}

module PIL.Native.Objects (
    Boxable(..),
    ObjectSpace,
    dumpObjSpace,
    newClass, newObject, registerObject, genObject,
    newScalarObj,
    callRepr, addRepr,
) where
import PIL.Native.Coerce
import PIL.Native.Types
import PIL.Native.Pretty
import System.Mem.Weak
import Control.Exception
import Control.Monad.State

newScalarObj :: NativeObj -> NativeSeq -> STM Native
newScalarObj cls args = do
    tvar <- newTVar (args ! 0)
    fmap toNative $ mkPrimObject cls
        [ ("fetch", const $ readTVar tvar)
        , ("store", \args -> let arg = (args ! 0) in writeTVar tvar arg >> return arg)
        ]

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
    clsPrim = ("class", const (return (toNative cls)))

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
                    val <- callRepr obj "get_attr" (mkSeq [toNative "$!name"])
                    print $! toString val
            Nothing  -> return ()

callRepr :: MonadSTM m => NativeObj -> String -> NativeSeq -> m Native
callRepr obj meth args = liftSTM $ (o_repr obj ! mkStr meth) args

genObject :: NativeObj -> NativeMap -> STM NativeObj
genObject cls attrs = do
    tvar  <- liftSTM $ newTVar attrs
    let getAttr args = fmap (! (fromNative (args ! 0))) $ readTVar tvar
        hasAttr args = fmap (toNative . (`PIL.Native.Coerce.exists` (fromNative (args ! 0))))
            $ readTVar tvar
        setAttr args = do
            attrs <- readTVar tvar
            writeTVar tvar (insert attrs (fromNative (args ! 0)) (args ! 1))
            return (args ! 1)
    mkPrimObject cls
        [ ("get_attr", getAttr)
        , ("set_attr", setAttr)
        , ("has_attr", hasAttr)
        ]

newClass :: (MonadState ObjectSpace m, MonadIO m, MonadSTM m) =>
    NativeObj -> NativeMap -> m NativeObj
newClass cls attrs = registerObject $ do
    obj <- genObject cls attrs
    return (addRepr obj "mro_merge" $ defaultCreate cls)

newObject :: (MonadState ObjectSpace m, MonadIO m, MonadSTM m) =>
    NativeObj -> NativeMap -> m NativeObj
newObject cls attrs = registerObject (genObject cls attrs)

registerObject :: (MonadState ObjectSpace m, MonadIO m, MonadSTM m) => STM NativeObj -> m NativeObj
registerObject gen = do
    obj  <- liftSTM gen
    objs <- get
    let obj' = addRepr (addRepr obj{ o_id = oid } "id" (const (return (toNative oid))))
                       "new_opaque" (defaultCreate obj')
        oid = size objs
    ptr <- liftIO $ mkWeakPtr obj' Nothing
    put (insert objs oid ptr)
    return obj'

addRepr :: NativeObj -> String -> ObjectPrim -> NativeObj
addRepr obj name prim = obj{ o_repr = repr' }
    where
    repr' = insert (o_repr obj) (mkStr name) prim
