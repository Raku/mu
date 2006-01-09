module PIL.Repr.P6Hash (P6Hash, create, prims) where
import PIL.Native.Types
import PIL.Native.Coerce
import PIL.Repr.Internals
import Data.Map as Map

-- Implement with skip-index later -- just a Seq for now
type P6Hash = TVar (Map Native Native)

create :: Native -> STM P6Hash
create = newTVar . seqToMap

prims :: MapOf (P6Hash -> ObjectPrim)
prims = mkMap
    [ ("fetch", _fetch)
    , ("store", _store)
    , ("replace", _replace)
    , ("as_seq", _as_seq)
    ]

_as_seq tvar _ = fmap (toNative . mapToList) (readTVar tvar)
_fetch tvar args = fmap (\hv -> (Map.!) hv (_0 args)) (readTVar tvar)
_store tvar args = do
    av <- readTVar tvar
    writeTVar tvar (Map.insert (fromNative (_0 args)) (_1 args) av)
    return nil
_replace tvar args = do
    writeTVar tvar (seqToMap $ _0 args)
    return nil

mapToList = foldr (\(x,y) z -> (x:y:z)) [] . Map.toAscList
seqToMap = Map.fromList . roll . fromNative
roll [] = []
roll [_] = error "odd number of hash elements"
roll (k:v:xs) = ((k, v):roll xs)
