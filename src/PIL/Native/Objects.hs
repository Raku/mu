{-# OPTIONS_GHC -fglasgow-exts #-}

module PIL.Native.Objects (
    Boxable(..),
    ObjectSpace,
    dumpObjSpace,
    newClass, newObject, registerObject, genObject,
    newScalarObj, newArrayObj, newHashObj,
    callRepr, addRepr,
) where
import PIL.Native.Coerce
import PIL.Native.Types
import PIL.Native.Pretty
import System.Mem.Weak
import Control.Exception
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Seq as Seq

_0 :: Seq.Seq a -> a
_0 = (`Seq.index` 0)

_1 :: Seq.Seq a -> a
_1 = (`Seq.index` 1)

withArgs :: (NativeSeq -> STM a) -> ObjectPrim 
withArgs f args = f args >> return nil

withArg0 :: (IsNative a, IsNative b) => (a -> STM b) -> ObjectPrim 
withArg0 f args = fmap toNative (f (fromNative (_0 args)))

newScalarObj :: NativeObj -> NativeSeq -> STM Native
newScalarObj cls args = do
    tvar <- newTVar (_0 args)
    fmap toNative $ mkPrimObject cls
        [ ("fetch", const $ readTVar tvar)
        , ("store", withArg0 (writeTVar tvar))
        ]

newArrayObj :: NativeObj -> NativeSeq -> STM Native
newArrayObj cls args = do
    tvar <- newTVar (fromNative $ _0 args)
    fmap toNative $ mkPrimObject cls
        [ ("fetch_list", const $ fmap toNative (readTVar tvar))
        , ("store_list", withArg0 (writeTVar tvar . fromNative))
        , ("fetch_elem", withArg0 (\x -> fmap (`Seq.index` x) (readTVar tvar)))
        , ("store_elem", withArgs $ \args -> do
            -- XXX - dynamic extension
            av <- readTVar tvar
            writeTVar tvar (Seq.update (fromNative (_0 args)) (_1 args) av)
          )
        ]

newHashObj :: NativeObj -> NativeSeq -> STM Native
newHashObj cls args = do
    tvar <- newTVar (seqToMap (_0 args))
    fmap toNative $ mkPrimObject cls
        [ ("fetch_list", const $ fmap (toNative . mapToList) (readTVar tvar))
        , ("store_list", withArg0 (writeTVar tvar . seqToMap))
        , ("fetch_elem", withArg0 (\x -> fmap (\hv -> (Map.!) hv x) (readTVar tvar)))
        , ("store_elem", withArgs $ \args -> do
            -- XXX - dynamic extension
            av <- readTVar tvar
            writeTVar tvar (Map.insert (fromNative (_0 args)) (_1 args) av)
          )
        ]
    where
    mapToList = foldr (\(x,y) z -> (x:y:z)) [] . Map.toAscList
    seqToMap = Map.fromList . roll . fromNative
    roll [] = []
    roll [_] = error "odd number of hash elements"
    roll (k:v:xs) = ((k, v):roll xs)

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
                       "create" (defaultCreate obj')
        oid = size objs
    ptr <- liftIO $ mkWeakPtr obj' Nothing
    put (insert objs oid ptr)
    return obj'

addRepr :: NativeObj -> String -> ObjectPrim -> NativeObj
addRepr obj name prim = obj{ o_repr = repr' }
    where
    repr' = Map.insertWith (flip const) (mkStr name) prim (o_repr obj)
