{-# OPTIONS_GHC -fglasgow-exts -fallow-overlapping-instances #-}
module PIL.Repr.P6Scalar (P6Scalar, create, prims) where
import PIL.Native.Types
import PIL.Native.Coerce
import PIL.Repr.Internals

type P6Scalar = TVar Native

create :: Native -> STM P6Scalar
create = newTVar

prims :: MapOf (P6Scalar -> ObjectPrim)
prims = mkMap
    [ prim "fetch"  readTVar
    , prim "store"  writeTVar
    , prim "as_bit" asBit
    , prim "as_int" asInt
    , prim "as_num" asNum
    , prim "as_str" asStr
    ]

asBit :: P6Scalar -> STM NativeBit
asBit = coerceAs
asInt :: P6Scalar -> STM NativeInt
asInt = coerceAs
asNum :: P6Scalar -> STM NativeNum
asNum = coerceAs
asStr :: P6Scalar -> STM NativeStr
asStr = coerceAs

coerceAs :: IsNative a => P6Scalar -> STM a
coerceAs = fmap fromNative . readTVar

