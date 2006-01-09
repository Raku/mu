module PIL.Repr.P6Array (P6Array, create, prims) where
import PIL.Native.Types

-- Implement with skip-index later -- just a Seq for now
type P6Array = TVar NativeSeq

create :: Native -> STM P6Array
create = undefined

prims :: MapOf ObjectPrim
prims = undefined
