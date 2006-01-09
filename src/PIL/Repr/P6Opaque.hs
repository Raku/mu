module PIL.Repr.P6Opaque (P6Opaque, create, prims) where
import PIL.Native.Types

-- Implement with skip-index later -- just a Seq for now
type P6Opaque = TVar NativeSeq

create :: Native -> STM P6Opaque
create = undefined

prims :: MapOf ObjectPrim
prims = undefined
