module PIL.Repr.P6Hash (P6Hash, create, prims) where
import PIL.Native.Types

-- Implement with skip-index later -- just a Seq for now
type P6Hash = TVar NativeSeq

create :: Native -> STM P6Hash
create = undefined

prims :: MapOf ObjectPrim
prims = undefined
