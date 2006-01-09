module PIL.Repr.P6Scalar (P6Scalar, create, prims) where
import PIL.Native.Types

-- Implement with skip-index later -- just a Seq for now
type P6Scalar = TVar NativeSeq

create :: Native -> STM P6Scalar
create = undefined

prims :: MapOf ObjectPrim
prims = undefined
