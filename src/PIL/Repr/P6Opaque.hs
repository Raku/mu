module PIL.Repr.P6Opaque (P6Opaque, create, prims) where
import PIL.Native.Coerce
import PIL.Native.Types
import PIL.Repr.Internals

-- Implement with skip-index later -- just a Seq for now
type P6Opaque = TVar NativeMap

create :: Native -> STM P6Opaque
create = newTVar . fromNative

prims :: MapOf (P6Opaque -> ObjectPrim)
prims = mkMap
    [ ("has_attr", hasAttr)
    , ("get_attr", getAttr)
    , ("set_attr", setAttr)
    , ("set_attr_hash", setAttrHash)
    , ("as_bit", const (const (return (toNative True))))
    ]

getAttr tvar args = fmap (! (fromNative (args ! 0))) $ readTVar tvar

hasAttr tvar args = fmap (toNative . (`PIL.Native.Coerce.exists` (fromNative (args ! 0)))) $ readTVar tvar

setAttr tvar args = do
    attrs <- readTVar tvar
    writeTVar tvar (insert attrs (fromNative (args ! 0)) (args ! 1))
    return (args ! 1)

setAttrHash tvar args = do
    let [attVal, keyVal, val] = elems args
        att = fromNative attVal :: NativeStr
        key = fromNative keyVal :: NativeStr
    attrs  <- readTVar tvar
    let submap = fromNative (attrs ! att) :: NativeMap
    writeTVar tvar (insert attrs att (toNative (insert submap key val)))
    return val
