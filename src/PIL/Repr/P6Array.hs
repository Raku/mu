module PIL.Repr.P6Array (P6Array, create, prims) where
import PIL.Native.Types
import PIL.Native.Coerce
import PIL.Repr.Internals
import qualified Data.Seq as Seq

-- Implement with skip-index later -- just a Seq for now
type P6Array = TVar NativeSeq

create :: Native -> STM P6Array
create = newTVar . fromNative

prims :: MapOf (P6Array -> ObjectPrim)
prims = mkMap
    [ ("fetch", _fetch)
    , ("store", _store)
    , ("replace", _replace)
    , ("as_seq", _as_seq)
    ]

_as_seq tvar _ = fmap toNative (readTVar tvar)
_fetch tvar args = fmap (`Seq.index` (fromNative $ _0 args)) (readTVar tvar)
_store tvar args = do
    av <- readTVar tvar
    writeTVar tvar (Seq.update (fromNative $ _0 args) (_1 args) av)
    return nil
_replace tvar args = do
    writeTVar tvar (fromNative $ _0 args)
    return nil
